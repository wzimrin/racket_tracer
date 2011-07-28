#lang racket

(require (prefix-in kernel: syntax/kerncase)
         gui-debugger/marks
         mzlib/etc
         (prefix-in srfi: srfi/1/search)
         (for-syntax scheme/base)
         (only-in mzscheme [apply plain-apply]))

(define (arglist-bindings arglist-stx)
    (syntax-case arglist-stx ()
      [var
       (identifier? arglist-stx)
       (list arglist-stx)]
      [(var ...)
       (syntax->list arglist-stx)]
      [(var . others)
       (cons #'var (arglist-bindings #'others))]))

(define annotate-stx
    (opt-lambda (stx break-wrap record-bound-id record-top-level-id [source #f])
      
      (define breakpoints (make-hasheq))
      
      (define (previous-bindings bound-vars)
        (if (null? bound-vars)
            #'null
            #'(#%plain-app debugger-local-bindings)))
      
      (define (top-level-annotate stx)
        (kernel:kernel-syntax-case/phase
         stx (namespace-base-phase)
         [(module identifier name (plain-module-begin . module-level-exprs))
          (with-syntax ([(module . _) stx])
            (quasisyntax/loc stx (module identifier name
                                   (plain-module-begin 
                                    #,@(map (lambda (e) (module-level-expr-iterator
                                                         e (list (syntax-e #'identifier)
                                                                 (syntax-source #'identifier))))
                                            (syntax->list #'module-level-exprs))))))]
         [else-stx
          (general-top-level-expr-iterator stx  #f)]))
      
      (define (module-level-expr-iterator stx module-name )
        (kernel:kernel-syntax-case
         stx #f
         [(#%provide . provide-specs)
          stx]
         [else-stx
          (general-top-level-expr-iterator stx module-name )]))
      
      (define (general-top-level-expr-iterator stx module-name )
        (kernel:kernel-syntax-case
         stx #f
         [(define-values (var ...) expr)
          
          (begin
            (for-each (lambda (v) (record-bound-id 'bind v v))
                      (syntax->list #'(var ...)))
            (quasisyntax/loc stx
              (begin (define-values (var ...) #,(annotate #`expr '() #t module-name))
                     #,(if (syntax-source stx)
                           #`(begin (#%plain-app 
                                     #,record-top-level-id '#,module-name #'var (case-lambda
                                                                                 [() var]
                                                                                 [(v) (set! var v)])) ...)
                           #'(#%plain-app void))
                     (#%plain-app void)))
            )
          ]
         [(define-syntaxes (var ...) expr)
          stx]
         [(define-values-for-syntax (var ...) expr)
          ;; define-values-for-syntax's RHS is compile time, so treat it
          ;; like define-syntaxes
          stx]
         [(begin . top-level-exprs)
          (quasisyntax/loc stx (begin #,@(map (lambda (expr)
                                                (module-level-expr-iterator expr module-name ))
                                              (syntax->list #'top-level-exprs))))]
         [(#%require . require-specs)
          stx]
         [else
          (annotate stx '() #f module-name )]))
      
      (define (annotate expr bound-vars is-tail? module-name)
        
        (define annotate-break?
          (let ([pos (syntax-position expr)]
                [src (syntax-source expr)])
            (and src pos
                 (hash-ref breakpoints pos (lambda () #t))
                 (kernel:kernel-syntax-case
                  expr #f
                  [(if test then else) #t]
                  [(begin . bodies) #t]
                  [(begin0 . bodies) #t]
                  [(let-values . clause) #t]
                  [(letrec-values . clause) #t]
                  [(set! var val) #t]
                  [(with-continuation-mark key mark body) #t]
                  [(#%plain-app . exprs) #t]
                  [_ #f])
                 (begin
                   (hash-set! breakpoints pos #f)
                   (when (not is-tail?)
                     (hash-set! breakpoints (+ pos (syntax-span expr) -1) #f))
                   #t))))
        
        (define (let/rec-values-annotator letrec?)
          (kernel:kernel-syntax-case
           expr #f
           [(label (((var ...) rhs) ...) . bodies)
            (let* ([new-bindings (apply append
                                        (map syntax->list
                                             (syntax->list #`((var ...) ...))))]
                   [all-bindings (append new-bindings bound-vars)]
                   [new-rhs (map (lambda (expr)
                                   (annotate expr 
                                             (if letrec? all-bindings bound-vars)
                                             #f module-name ))
                                 (syntax->list #'(rhs ...)))]
                   [last-body (car (reverse (syntax->list #'bodies)))]
                   [all-but-last-body (reverse (cdr (reverse (syntax->list #'bodies))))]
                   [bodies (append (map (lambda (expr)
                                          (annotate expr all-bindings #f module-name ))
                                        all-but-last-body)
                                   (list (annotate
                                          last-body
                                          all-bindings 
                                          is-tail? module-name )))]
                   [local-debug-info (assemble-debug-info new-bindings new-bindings 'normal #f)]
                   [previous-bindings (previous-bindings bound-vars)])
              (for-each (lambda (id) (record-bound-id 'bind id id)) new-bindings)
              (with-syntax ([(new-rhs/trans ...) new-rhs]
                            [previous-bindings previous-bindings])
                (if letrec?
                    (quasisyntax/loc expr
                      (let ([old-bindings previous-bindings])
                        (label (((debugger-local-bindings) (#%plain-lambda ()
                                                             (#%plain-app
                                                              list*
                                                              #,@local-debug-info
                                                              old-bindings)))
                                ((var ...) new-rhs/trans) ...)
                               #,@bodies)))
                    (quasisyntax/loc expr
                      (label (((var ...) new-rhs/trans) ...)
                             (let ([debugger-local-bindings (#%plain-lambda ()
                                                              (#%plain-app
                                                               list*
                                                               #,@local-debug-info
                                                               previous-bindings))])
                               #,@bodies))))))]))
        
        (define (lambda-clause-annotator clause)
          (kernel:kernel-syntax-case
           clause #f
           [(arg-list . bodies)
            (let* ([new-bound-vars (arglist-bindings #'arg-list)]
                   [all-bound-vars (append new-bound-vars bound-vars)]
                   [new-bodies (let loop ([bodies (syntax->list #'bodies)])
                                 (if (equal? '() (cdr bodies))
                                     (list (annotate (car bodies) all-bound-vars #t module-name ))
                                     (cons (annotate (car bodies) all-bound-vars #f module-name )
                                           (loop (cdr bodies)))))])
              (for-each (lambda (id) (record-bound-id 'bind id id)) new-bound-vars)
              (quasisyntax/loc clause
                (arg-list 
                 (let ([debugger-local-bindings
                        (#%plain-lambda ()
                          (#%plain-app
                           list*
                           #,@(assemble-debug-info new-bound-vars new-bound-vars 'normal #f)
                           #,(previous-bindings bound-vars)))])
                   #,@new-bodies))))]))
        
        (define annotated
          (syntax-recertify
           (kernel:kernel-syntax-case
            expr #f
            [var-stx (identifier? (syntax var-stx))
                     (let ([binder (and (syntax-original? expr)
                                        (srfi:member expr bound-vars free-identifier=?))])
                       (if binder
                           (record-bound-id 'ref expr (car binder))
                           (record-bound-id 'top-level expr expr))
                       expr)]
            
            [(#%plain-lambda . clause)
             (quasisyntax/loc expr 
               (#%plain-lambda #,@(lambda-clause-annotator #'clause)))]
            
            [(case-lambda . clauses)
             (quasisyntax/loc expr
               (case-lambda #,@(map lambda-clause-annotator (syntax->list #'clauses))))]
            
            [(if test then else)
             (quasisyntax/loc expr (if #,(annotate #'test bound-vars #f module-name )
                                       #,(annotate #'then bound-vars is-tail? module-name )
                                       #,(annotate #'else bound-vars is-tail? module-name )))]
            
            [(begin . bodies)
             (letrec ([traverse
                       (lambda (lst)
                         (if (and (pair? lst) (equal? '() (cdr lst)))
                             `(,(annotate (car lst) bound-vars is-tail? module-name ))
                             (cons (annotate (car lst) bound-vars #f module-name )
                                   (traverse (cdr lst)))))])
               (quasisyntax/loc expr (begin #,@(traverse (syntax->list #'bodies)))))]
            
            [(begin0 . bodies)
             (quasisyntax/loc expr (begin0 #,@(map (lambda (expr)
                                                     (annotate expr bound-vars #f module-name ))
                                                   (syntax->list #'bodies))))]
            
            [(let-values . clause)
             (let/rec-values-annotator #f)]
            
            [(letrec-values . clause) 
             (let/rec-values-annotator #t)]
            
            [(set! var val)
             (let ([binder (and (syntax-original? #'var)
                                (srfi:member #'var bound-vars free-identifier=?))])
               (when binder
                 (record-bound-id 'set expr (car binder)))
               (quasisyntax/loc expr (set! var #,(annotate #`val bound-vars #f module-name ))))]
            
            [(quote _) expr]
            
            [(quote-syntax _) expr]
            
            [(with-continuation-mark key mark body)
             (quasisyntax/loc expr (with-continuation-mark key
                                     #,(annotate #'mark bound-vars #f module-name )
                                     #,(annotate #'body bound-vars is-tail? module-name )))]
            
            [(#%plain-app . exprs)
             (let ([subexprs (map (lambda (expr) 
                                    (annotate expr bound-vars #f module-name ))
                                  (syntax->list #'exprs))])
               (if (or is-tail? (not (syntax-source expr)))
                   (quasisyntax/loc expr (#%plain-app . #,subexprs))
                   (wcm-wrap (make-debug-info module-name expr bound-vars bound-vars 'normal #f (previous-bindings bound-vars))
                             (quasisyntax/loc expr (#%plain-app . #,subexprs)))))]
            
            [(#%top . var) expr]
            [(#%variable-reference . _) expr]
            
            [else (error 'expr-syntax-object-iterator "unknown expr: ~a"
                         (syntax->datum expr))])
           expr
           (current-code-inspector)
           #f))
        
        (if annotate-break?
            (break-wrap
             (make-debug-info module-name expr bound-vars bound-vars 'at-break #f (previous-bindings bound-vars))
             annotated
             expr
             is-tail?)
            annotated))
      
      (values (top-level-annotate stx) (hash-map breakpoints (lambda (k v) k)))))

(provide annotate-and-eval)

(define (annotate-and-eval stx src)

  (map (lambda (x)
         (let-values ([(a b)
                       (annotate-stx x
                                     (lambda (a b c d)
                                       ;(displayln a)
                                       ;(displayln b)
                                       (displayln c)
                                       ;(displayln d)
                                       ;(displayln "----------")
                                       b)
                                     (lambda (a b c)
                                       ;(displayln "called")
                                       ;(displayln a)
                                       ;(displayln b)
                                       ;(displayln c)
                                       (void))
                                     (lambda (a b c)
                                       ;(displayln a)
                                       ;(displayln b)
                                       ;(displayln c)
                                       ;(displayln "called@")
                                       (void)))])
           a))
       stx))

(define original-syntax
  #'(begin
       (define (f x)
         (add1 x))))

(define test-syntax
  (list (parameterize ([current-namespace (make-base-namespace)])
          
          #;(eval original-syntax)
          (expand-syntax original-syntax))))

(annotate-and-eval test-syntax "")


#|
#lang racket

(require "buttons.rkt")

(require [except-in lang/htdp-intermediate-lambda
                    #%app define lambda require #%module-begin let local
                    check-expect let* letrec image? λ and or if])
(require [prefix-in isl:
                    [only-in lang/htdp-intermediate-lambda
                             define lambda require let local image? and or if]])
(require test-engine/racket-tests)
(require syntax-color/scheme-lexer)
(require racket/pretty)
(require [only-in net/sendurl
                  send-url/contents])
(require [only-in planet/resolver
                  resolve-planet-path])
(require [only-in web-server/templates
                  include-template])
(require [only-in 2htdp/image
                  image?])

(require [only-in racket/gui
                  message-box])
(require syntax/toplevel)

(require [for-syntax racket/port])
(require net/base64)
(require file/convertible)
(require mzlib/pconvert)

(require (planet dherman/json:3:0))

(provide let local let* letrec)

(provide [rename-out (app-recorder #%app)
                     (check-expect-recorder check-expect)
                     (custom-define define)
                     (custom-lambda lambda)
                     (custom-lambda λ)])
;(provide app-recorder)
(provide [all-from-out lang/htdp-intermediate-lambda])
(provide [rename-out #;(isl:define define)
                     #;(isl:lambda lambda)
                     (isl:require require)
                     (isl:and and)
                     (isl:or or)
                     (isl:if if)
                     ;(ds-recorder define-struct)
                     (isl:image? image?)
                     #;(isl:let let)])

;(provide struct-accessor-procedure?)

(provide #%module-begin)



;the actual struct that stores our data
(struct node (name func formal result actual kids linum idx span src-idx src-span) #:mutable #:transparent)

(struct wrapper (value id) #:transparent)

(define (unwrap x)
  (if (wrapper? x)
      (wrapper-value x)
      x))

(define (wrap x)
  (wrapper x (gensym "value")))

(define src (box ""))

;creates a node with no result or children
;takes a name, a formals list, and an actuals list
(define (create-node n func f a l i s s-i s-s)
  (node n func f 'no-result a empty l i s s-i s-s))

;adds a kid k to node n
(define (add-kid n k)
  (set-node-kids! n (cons k (node-kids n))))

;the current definition we are in
(define current-call (make-parameter (create-node 'top-level #f empty empty 0 0 0 0 0)))
(define topCENode (create-node 'CE-top-level #f empty empty 0 0 0 0 0))

(define current-linum (make-parameter 0))
(define current-idx (make-parameter 0))
(define current-span (make-parameter 0))
(define current-fun (make-parameter #f))
(define current-app-call (make-parameter empty))

(define ce-hash (make-hash))

(define (add-to-hash h key idx span success)
  (hash-set! h key (list idx span success))) 

(define-syntax (check-expect-recorder e)
  (with-syntax ([linum (syntax-line e)]
                [idx (syntax-position e)]
                [span (syntax-span e)]
                [actual 'actual]
                [expected 'expected])
    (syntax-case e ()
      [(_ actualStx expectedStx)
       (let* ([datum (syntax-e #'actualStx)]
              [func (if (pair? datum)
                        (car datum)
                        datum)]
              [ce-name func]
              [args (when (pair? datum)
                      (cdr datum))])
         #`(begin 
             (define parent-node (create-node '#,ce-name #f empty empty linum idx span 0 0))
             (check-expect 
              (let ([actual-node (create-node 'actual 
                                              #f
                                              (list 'actualStx)
                                              empty
                                              #,(syntax-line #'actualStx)
                                              #,(syntax-position #'actualStx)
                                              #,(syntax-span #'actualStx)
                                              0
                                              0)])
                (add-kid parent-node actual-node)
                (parameterize ([current-call actual-node])
                  (set-node-result! actual-node actualStx))
                ;Check if actual and expected are the same
                (let ([ce-correct? (apply equal?
                                          (map node-result
                                               (node-kids parent-node)))])
                  
                  ;When ce is true, add to hash
                  #,(when (pair? datum)
                      #`(add-to-hash ce-hash
                                     (list #,func (node-result actual-node) (list . #,args))
                                     idx
                                     span
                                     ce-correct?))
                  ;When ce is false, create a ce node
                  (when (not ce-correct?)
                    (set-node-result! parent-node #f)
                    (set-node-kids! parent-node (reverse (node-kids parent-node)))
                    (add-kid topCENode parent-node)))
                (node-result actual-node))
              (let ([expected-node (create-node 'expected 
                                                #f
                                                (list 'expectedStx)
                                                empty
                                                #,(syntax-line #'expectedStx)
                                                #,(syntax-position #'expectedStx)
                                                #,(syntax-span #'expectedStx)
                                                0
                                                0)])
                (add-kid parent-node expected-node)
                (parameterize ([current-call expected-node])
                  (let [(result expectedStx)]
                    (set-node-result! expected-node result)
                    result))))))])))

(define-for-syntax (lambda-body args body name orig fun)
  #`(let* ([app-call? (eq? #,fun (current-fun))]
           [n (if app-call?
                  (struct-copy node (current-app-call)
                               [src-idx #,(syntax-position orig)]
                               [src-span #,(syntax-span orig)])
                  (create-node '#,name #,fun empty #,args
                               0 0 0
                               #,(syntax-position orig)
                               #,(syntax-span orig)))]
           [parent (if app-call?
                       (current-call)
                       (current-app-call))])
      (add-kid parent n)
      (parameterize ([current-call n])
        (let ([result #,body])
          (set-node-result! n result)
          result))))

(define-syntax (custom-lambda e)
  (syntax-case e ()
    [(_ args body)
     (let ([sym (gensym)])
       #`(letrec ([#,sym (lambda args
                                    #,(lambda-body #'(list . args) #'body #'lambda e sym))])
           (procedure-rename #,sym 'lambda)))]))

(define-syntax (custom-define e)
  (syntax-case e (lambda)
    [(_ (fun-expr arg-expr ...) body)
     #`(define (fun-expr arg-expr ...)
         #,(lambda-body #'(list arg-expr ...) #'body #'fun-expr e #'fun-expr))]
    [(_ fun-expr (lambda (arg-expr ...) body))
     #'(custom-define (fun-expr arg-expr ...) body)]
    [(_ id val)
     #'(define id val)]))

(define (function-sym datum)
  (if (cons? datum)
      (function-sym (first datum))
      datum))

;records all function calls we care about - redefinition of #%app
(define-syntax (app-recorder e)
  (syntax-case e ()
    [(_ fun-expr arg-expr ...)
     (with-syntax ([linum (syntax-line e)]
                   [idx (syntax-position e)]
                   [span (syntax-span e)])
     #'(let* ([fun fun-expr]
              [args (list arg-expr ...)]       
              [n (create-node (function-sym 'fun-expr) fun empty args
                              linum idx span 0 0)]
              [result (parameterize ([current-linum linum]
                                     [current-idx idx]
                                     [current-span span]
                                     [current-fun fun]
                                     [current-app-call n])
                        (apply fun args))])
         (when (not (empty? (node-kids n)))
           (set-node-result! n result)
           (add-kid (current-call) n))
         result))]))

(define (print-right t)
  (node (node-formal t)
        (node-result t)
        (node-actual t)
        (reverse (map print-right (node-kids t)))))

; Why is this a macro and not a function?  Because make it a function
; affects the call record!

(define-syntax-rule (show-trace)
  (print-right (current-call)))

(define (get-base64 img)
  (base64-encode (convert img 'png-bytes)))

(define (uri-string img)
  (string-append "data:image/png;charset=utf-8;base64,"
                 (bytes->string/utf-8 (get-base64 img))))

(define (json-image img)
  (hasheq 'type "image"
          'src (uri-string img)))


(define (print-list lst)
  (let* ([ppl (pretty-format lst (pretty-print-columns))]
         [lines (length (regexp-match* "\n" ppl))]
         [lists (length (regexp-match* "list" ppl))])
    (if (= lines lists)
        (begin 
          (displayln "one line per list")
          ppl)
        ;need to split into two lines
        (let*-values ([(l-beg l-end-rev) (split-list lst)])
          (plh l-beg l-end-rev "(list" ")")))))

(define (split-list lst)
  (let ([left (ceiling (/ (length lst) 2))]
        [right (floor (/ (length lst) 2))])
    (values (drop-right lst left)
            (reverse (take-right lst right)))))

(define (plh fwd rev s-fwd s-rev)
  (cond
    [(and (empty? fwd) (empty? rev))
     (string-append s-fwd "...\n      ..." s-rev)]
    [(empty? fwd) (plh fwd (rest rev) s-fwd (add-item s-rev rev))]
    [(empty? rev) (plh (rest fwd) rev (add-item s-fwd fwd) s-rev)]
    ;both have elements left
    [(and (cons? fwd) (cons? rev))
     (plh (rest fwd)
          (rest rev)
          (add-item fwd s-fwd true)
          (add-item rev s-rev false))]
    ))

(define (add-item lst s fwd)
  (let ([next-item (pretty-format (first lst) (pretty-print-columns))])
    (if (< (+ (string-length s)
              (string-length next-item)
              (if fwd 0 6))
           (pretty-print-columns))
        (if fwd 
            (string-append s " " next-item)
            (string-append " " next-item s))
        s)))
           
(define (format-nicely x depth width literal)
  ;print the result string readably
  (if (image? x)
      (json-image x)
      (let* ([p (open-output-string "out")])
        ;set columns and depth
        (parameterize ([pretty-print-columns width]
                       [pretty-print-depth depth]
                       [constructor-style-printing #t])
          (if (and (procedure? x) (object-name x))
              (display (object-name x) p)            
              (pretty-write (print-convert x) p)))
        ;return what was printed
        (hasheq 'type "value"
                'value (get-output-string p)))))

(define (ce-info n)
  (let* ([key (list (node-func n) (node-result n)  (node-actual n))]
         [l (hash-ref ce-hash key (list #f #f #f))])
    (values (first l) (second l) (third l))))


(define (node->json t)
  ;calls format-nicely on the elements of the list and formats that into a 
  ;javascript list
  (local [(define (format-list lst depth literal)
            (map (lambda (x)
                   (format-nicely x depth 40 literal))
                 lst))]
    (let-values ([(ce-idx ce-span ce-correct?) (ce-info t)])
      (hasheq 'name
              (format "~a" (node-name t))
              'formals
              (format-list (node-formal t) #f #f)
              'formalsShort
              (format-list (node-formal t) 2 #f)
              'actuals
              (format-list (node-actual t) #f #t)
              'actualsShort
              (format-list (node-actual t) 2 #t)
              'result
              (format-nicely (node-result t) #f 40 #t)
              'resultShort
              (format-nicely (node-result t) 2 40 #t)
              'linum
              (node-linum t)
              'idx
              (node-idx t)
              'span
              (node-span t)
              'srcIdx
              (node-src-idx t)
              'srcSpan
              (node-src-span t)
              'children
              (map node->json (reverse (node-kids t)))
              'ceIdx
              ce-idx
              'ceSpan
              ce-span
              'ceCorrect
              ce-correct?
              ))))
    

; Why is this a macro and not a function?  Because make it a function
; affects the call record!

(define (range start end)
  (build-list (- end start) (lambda (x) (+ start x))))

(define (lex-port p actual)
  (let*-values ([(str type junk start end) (scheme-lexer p)]
                [(span) (and start end
                             (if (equal? str "λ")
                                 1
                                 (- end start)))])
    (if (eq? type 'eof)
        empty
        (cons (list type (take actual span))
              (lex-port p (drop actual span))))))

(define (colors src)
  (apply append
         (map (lambda (lst)
                (let ([type (format "~a" (first lst))])
                  (if (andmap (lambda (x)
                                (and (char? x)
                                     (not (equal? x #\λ))))
                              (second lst))
                      (list (hasheq 'type "string"
                                    'color type
                                    'text (list->string (second lst))))
                      (map (lambda (val)
                             (cond 
                               [(image? val)
                                (hasheq 'type "image"
                                         'color type
                                         'src (uri-string val))]
                               [(equal? val #\λ)
                                (hasheq 'type "html"
                                        'color type
                                        'html "&lambda;")]
                               [else
                                (hasheq 'type 'string
                                        'color type
                                        'text (format "~a" val))]))
                           (second lst)))))
              (lex-port (let-values ([(in out) (make-pipe-with-specials)])
                          (for ([x src])
                            (if (char? x)
                                (display x out)
                                (write-special x out)))
                          (close-output-port out)
                          in)
                        src))))

#;(define-syntax-rule (trace->json offset)
  (format "var theTrace = ~a\nvar code = ~a\nvar codeOffset = ~a"
          (jsexpr->json (node->json (current-call)))
          (jsexpr->json (colors (unbox src)))
          offset))

(define (tree->json call)
  (jsexpr->json (node->json call)))

(define (code->json)
  (jsexpr->json (colors (unbox src))))

(define-for-syntax (print-expanded d)
  (printf "~a\n"
          (syntax->datum (local-expand d 'module (list)))))

(define (page name o)
  (let* ([title (string-append name " Trace")]
        [CSSPort (open-input-file (resolve-planet-path 
                                         '(planet tracer/tracer/tracer.css)))]
        [tracerCSS (port->string CSSPort)]
        [sideImageSrc (format "~s" (uri-string normal-side-arrow))]
        [downImageSrc (format "~s" (uri-string normal-down-arrow))]
        [correctCEImageSrc (format "~s" (uri-string normal-correct-checkbox))]
        [failedCEImageSrc (format "~s" (uri-string normal-failed-checkbox))]
        [correctCEImageSelSrc (format "~s" (uri-string highlight-correct-checkbox))]
        [failedCEImageSelSrc (format "~s" (uri-string highlight-failed-checkbox))]
        [toDefImageSrc (format "~s" (uri-string normal-src-button))]
        [toDefImageSelSrc (format "~s" (uri-string highlight-src-button))]
        [jQueryPort (open-input-file (resolve-planet-path
                                         '(planet tracer/tracer/jquery.js)))]
         
        [jQuery (port->string jQueryPort)]
        [tracerJSPort (open-input-file (resolve-planet-path
                                         '(planet tracer/tracer/tracer.js)))]
        [tracerJS (port->string tracerJSPort)]
        [theTrace (tree->json (current-call))]
        [ceTrace (tree->json topCENode)]
        [code (code->json)]
        [offset o]
        [template (include-template "index.html")])
    (close-input-port CSSPort)
    (close-input-port jQueryPort)
    (close-input-port tracerJSPort)
    template))


;adds trace->json and send-url to the end of the file
(define-syntax (#%module-begin stx)
  (syntax-case stx ()
    [(_ name source offset body ...)
     #`(#%plain-module-begin
        (annotate-and-eval #'(#%plain-module-begin body ...)
                           source)
        (set-box! src source)
        body ...
        (run-tests)
        (display-results)
        ;If empty trace generate error message
        (if (and (empty? (node-kids (current-call)))
                 (empty? (node-kids topCENode)))
            (message-box "Error" 
                         "There is nothing to trace in this file. Did you define any functions in this file? Are they called from this file?" 
                         #f 
                         '(ok stop))
            (send-url/contents (page name offset))))]))

#;(port-write-handler 
         p
         (lambda (val port [depth 0])
           (begin
             (displayln "pph lambda")
           (if (and (cons? val)
                    (equal? 'list (first val)))
               (begin
                 (displayln "pph lambda true if")
                 (displayln val)
                 (display (print-list(rest val)) p))
               (begin
                 (displayln "pph lambda false if")
                 (displayln val)
               (pretty-write val p))))))
;The code we want the above to evaluate to is
#;(add-to-hash correct-ce-hash
               function-name
               (list (node-result actual-node) (+ sub-arg1 sub-arg2) arg2 arg3))

|#