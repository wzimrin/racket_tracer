#lang racket

(require [except-in lang/htdp-intermediate-lambda
                    #%app define lambda require #%module-begin let local define-struct check-expect let*])
(require [prefix-in isl:
                    [only-in lang/htdp-intermediate-lambda
                             define lambda require let local define-struct]])
(require test-engine/racket-tests)
(require syntax-color/scheme-lexer)
(require racket/pretty)
(require net/sendurl)
(require [for-syntax racket/port])
(require net/base64)

(provide let local let*)

(provide [rename-out (app-recorder #%app)
                     (check-expect-recorder check-expect)
                     (custom-define define)
                     (custom-lambda lambda)])
;(provide app-recorder)
(provide [all-from-out lang/htdp-intermediate-lambda])
(provide [rename-out #;(isl:define define)
                     #;(isl:lambda lambda)
                     (isl:require require)
                     (ds-recorder define-struct)
                     #;(isl:let let)])

;(provide struct-accessor-procedure?)

(provide show-trace trace->json #%module-begin)

;the actual struct that stores our data
(struct node (name formal result actual kids linum idx span) #:mutable #:transparent)

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
(define (create-node n f a l i s)
  (node n f 'no-result a empty l i s))

;adds a kid k to node n
(define (add-kid n k)
  (set-node-kids! n (cons k (node-kids n))))

;the current definition we are in
(define current-call (make-parameter (create-node 'top-level empty empty 0 0 0)))

(define current-linum (make-parameter 0))
(define current-idx (make-parameter 0))
(define current-span (make-parameter 0))

;a boxed list of all functions that define-struct has defined in this namespace
(define ds-fun-names (box empty))

;adds one define-struct worth of names to ds-fun-names,
;given the name of the struct and its fields
(define (register-ds name fields)
  (let* ([name-s (symbol->string name)]
         [name-s-d (string-append name-s
                                 "-")])
    (set-box! ds-fun-names
              (append
               ;make-struct and struct?
               (map string->symbol (list (string-append "make-" name-s)
                                         (string-append name-s "?")))
               ;struct-field
               (map (lambda (field)
                      (string->symbol 
                       (string-append name-s-d
                                      (symbol->string field))))
                    fields)
               (unbox ds-fun-names)))))

;macro redefinition of define-struct that calls isl:define struct and register-ds
(define-syntax (ds-recorder e)
  (syntax-case e ()
    [(define-struct name fields)
     #'(begin (isl:define-struct name fields)
              (register-ds 'name 'fields))]))

(define-syntax (check-expect-recorder e)
  (with-syntax ([linum (syntax-line e)]
                [idx (syntax-position e)]
                [span (syntax-span e)]
                [ce 'check-expect]
                [actual 'actual]
                [expected 'expected])
    (syntax-case e ()
      [(_ actualStx expectedStx)
       #`(begin (define parent-node (create-node 'ce empty empty linum idx span))
                (check-expect (let ([actual-node (create-node 'actual (list 'actualStx)
                                                              empty
                                                              #,(syntax-line #'actualStx)
                                                              #,(syntax-position #'actualStx)
                                                              #,(syntax-span #'actualStx))])
                                (add-kid parent-node actual-node)
                                (parameterize ([current-call actual-node])
                                  (set-node-result! actual-node actualStx))
                                (when (not (apply equal?
                                                  (map node-result
                                                       (node-kids parent-node))))
                                  (set-node-result! parent-node #f)
                                  (set-node-kids! parent-node (reverse (node-kids parent-node)))
                                  (add-kid (current-call) parent-node))
                                (node-result actual-node))
                              (let ([expected-node (create-node 'expected (list 'expectedStx)
                                                                empty
                                                                #,(syntax-line #'expectedStx)
                                                                #,(syntax-position #'expectedStx)
                                                                #,(syntax-span #'expectedStx))])
                                (add-kid parent-node expected-node)
                                (parameterize ([current-call expected-node])
                                  (let [(result expectedStx)]
                                    (set-node-result! expected-node result)
                                    result)))))])))

(define-syntax (custom-lambda e)
  (syntax-case e ()
    [(_ args body)
     (with-syntax ([lambda 'lambda])
       #'(custom-lambda lambda args body))]
    [(_ name (arg-expr ...) body)
     #'(lambda (arg-expr ...)
         (let ([n (create-node 'name empty (list arg-expr ...)
                               (current-linum) (current-idx) (current-span))])
           (add-kid (current-call) n)
           (parameterize ([current-call n])
             (let ([result body])
               (set-node-result! n result)
               result))))]))

(define-syntax (custom-define e)
  (syntax-case e ()
    [(_ (fun-expr arg-expr ...) body)
     #'(define fun-expr
         (custom-lambda fun-expr (arg-expr ...) body))]
    [(_ fun-expr (lambda arg-exprs body))
     #'(custom-define (fun-expr arg-exprs) body)]
    [(_ id val)
     #'(define id val)]))

;records all function calls we care about - redefinition of #%app
(define-syntax (app-recorder e)
  (syntax-case e ()
    [(_ fun-expr arg-expr ...)
     (with-syntax ([linum (syntax-line e)]
                   [idx (syntax-position e)]
                   [span (syntax-span e)])
       #'(parameterize ([current-linum linum]
                        [current-idx idx]
                        [current-span span])
           (#%app fun-expr arg-expr ...)))
     ;ensure that fun-expr is a function
     #;(identifier? #'fun-expr) 
     ;gets where fun-expr was defined
     #;(let* ([binding (identifier-binding #'fun-expr)];get the binding
            ;if vals = 'lexical, fun-expr was locally defined
            ;if vals = '(#f #f), fun-expr was a top-level definition in this module
            ;if vals = anything else, fun-expr was bound somewhere else
            [vals (if (list? binding)
                      ;if the binding is a list, we want to split the first element
                      (call-with-values
                       (lambda ()
                         (module-path-index-split (car binding)))
                       list)
                      ;otherwise, return it
                      binding)]
            [linum (syntax-line e)]
            [idx (syntax-position e)]
            [span (syntax-span e)])
       ;we want to potentially trace fun-expr if it was bound in the file
       (with-syntax ([linum linum]
                     [idx idx]
                     [span span])
         (if (or (equal? vals 'lexical)
               (equal? vals '(#f #f)))
           ;we also need to check at runtime if fun-expr was defined by define-struct
           #`(if (or (member 'fun-expr (unbox ds-fun-names))
                     (struct-accessor-procedure? fun-expr))
                 ;if not a function you want to trace, leave as is
                 (#%app fun-expr arg-expr ...)
                 ;otherwise trace
                 (let ([n (create-node 'fun-expr '(arg-expr ...)
                                       "nothing here yet!"
                                       linum idx span)])
                   (begin
                     ;adds n to current-call's kids 
                     (add-kid (current-call) n)
                     ;evaluate fun-expr and its planet tracer/tracerargs
                     (let* ([fun fun-expr]
                            [args (list arg-expr ...)])
                       ;set current-call to n while you evaluate (fun-expr . args)
                       (parameterize ([current-call n])
                         (begin
                           ;set the actuals, run the function, and set the result
                           (set-node-actual! n args)
                           (let ([v (#%app apply fun args)])
                             (begin
                               (set-node-result! n v)
                               v))))))))
           #'(#%app fun-expr arg-expr ...))))]))

(define (print-right t)
  (node (node-formal t)
        (node-result t)
        (node-actual t)
        (reverse (map print-right (node-kids t)))))

; Why is this a macro and not a function?  Because make it a function
; affects the call record!

(define-syntax-rule (show-trace)
  (print-right (current-call)))

#;(define (get-base64 img)
  (base64-encode (convert img 'png-bytes)))

#;(define (json-image img)
  (string-append "data:image/png;charset=utf-8;base64,"
                 (bytes->string/utf-8 (get-base64 img))))

(define (format-nicely x depth width literal)
  ;print the result string readably
  (if (image? x)
      #;(json-image x)
      x
      (format "~S"
          (let [(p (open-output-string "out"))]
            ;set columns and depth
            (parameterize [(pretty-print-columns width)
                           (pretty-print-depth depth)]
              ;choose whether you want x printed readably or for viewing
              ((if literal
                   pretty-print
                   pretty-display) x p))
            ;return what was printed
            (get-output-string p)))))

(define (node->json t)
  ;calls format-nicely on the elements of the list and formats that into a 
  ;javascript list
  (local [(define (format-list lst depth literal)
            (string-append "["
                           (string-join (map (lambda (x)
                                               (format-nicely x depth 40 literal))
                                             lst)
                                        ",")
                           "]"))]
    (format "{name: \"~a\",
            formals: ~a,
            formalsShort: ~a,
            actuals: ~a,
            actualsShort: ~a,
            result: ~a,
            resultShort: ~a,
            linum: ~a,
            idx: ~a,
            span: ~a,
            children: [~a]}"
            (node-name t)
            (format-list (node-formal t) #f #f)
            (format-list (node-formal t) 4 #f)
            (format-list (node-actual t) #f #t)
            (format-list (node-actual t) 4 #t)
            (format-nicely (node-result t) #f 40 #t)
            (format-nicely (node-result t) 4 40 #t)
            (node-linum t)
            (node-idx t)
            (node-span t)
            (if (empty? (node-kids t))
                ""
                (local ([define (loop k)
                          (if (empty? (rest k))
                              (first k)
                              (string-append (first k)
                                             ","
                                             (loop (rest k))))])
                  (loop (map node->json (reverse (node-kids t)))))))))

; Why is this a macro and not a function?  Because make it a function
; affects the call record!

(define-syntax-rule (trace->json)
  (local [#;(define (range start end)
            (build-list (- end start) (lambda (x) (+ start x))))
          #;(define (lex-port p)
            (let-values ([(str type junk start end) (scheme-lexer p)])
              (if (eq? type 'eof)
                  empty
                  (cons (list type start end)
                        (lex-port p)))))
          #;(define (colors src)
            (foldl (lambda (vals hsh)
                     (foldl (lambda (num hsh)
                              (hash-set hsh (first vals)
                                        (cons num
                                              (hash-ref hsh (first vals) empty))))
                            hsh
                            (range (second vals)
                                   (third vals))))
                   (hash)
                   (lex-port (open-input-string src))))]
    (with-output-to-file "tree-of-trace.js"
      (lambda ()
        (display (format "var theTrace = ~a\nvar code = ~S"
                         (node->json (current-call))
                         (unbox src))))
      #:exists 'replace)))

(define-for-syntax (print-expanded d)
  (printf "~a\n"
          (syntax->datum (local-expand d 'module (list)))))

;adds trace->json and send-url to the end of the file
(define-syntax (#%module-begin stx)
  (syntax-case stx ()
    [(_ source body ...)
     #`(#%plain-module-begin
        (set-box! src source)
        body ...
        (run-tests)
        (display-results)
        (trace->json)
        (external-browser 'firefox)
        (send-url "index.html")
        (external-browser 'google-chrome)
        (send-url "index.html"))]))
