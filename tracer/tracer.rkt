#lang racket

(require [except-in lang/htdp-intermediate-lambda
                    #%app define lambda require #%module-begin let local define-struct check-expect let* image?])
(require [prefix-in isl:
                    [only-in lang/htdp-intermediate-lambda
                             define lambda require let local define-struct image?]])
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

(require (planet dherman/json:3:0))

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
                     (isl:image? image?)
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
         (#%app fun-expr arg-expr ...)))]))

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

(define (json-image img)
  (hasheq 'type "image"
          'src (string-append "data:image/png;charset=utf-8;base64,"
                              (bytes->string/utf-8 (get-base64 img)))))

(define (format-nicely x depth width literal)
  ;print the result string readably
  (displayln x)
  (displayln (image? x))
  (if (image? x)
      (json-image x)
      (let [(p (open-output-string "out"))]
        ;set columns and depth
        (parameterize [(pretty-print-columns width)
                       (pretty-print-depth depth)]
          ;choose whether you want x printed readably or for viewing
          ((if literal
               pretty-print
               pretty-display) x p))
        ;return what was printed
        (hasheq 'type "value"
                'value (get-output-string p)))))

(define (node->json t)
  ;calls format-nicely on the elements of the list and formats that into a 
  ;javascript list
  (local [(define (format-list lst depth literal)
            (map (lambda (x)
                   (format-nicely x depth 40 literal))
                 lst))]
    (hasheq 'name
            (format "~a" (node-name t))
            'formals
            (format-list (node-formal t) #f #f)
            'formalsShort
            (format-list (node-formal t) 4 #f)
            'actuals
            (format-list (node-actual t) #f #t)
            'actualsShort
            (format-list (node-actual t) 4 #t)
            'result
            (format-nicely (node-result t) #f 40 #t)
            'resultShort
            (format-nicely (node-result t) 4 40 #t)
            'linum
            (node-linum t)
            'idx
            (node-idx t)
            'span
            (node-span t)
            'children
            (map node->json (reverse (node-kids t))))))
    

; Why is this a macro and not a function?  Because make it a function
; affects the call record!

(define (range start end)
  (build-list (- end start) (lambda (x) (+ start x))))

(define (lex-port p actual)
  (let-values ([(str type junk start end) (scheme-lexer p)])
    (if (eq? type 'eof)
        empty
        (cons (list type (substring actual (sub1 start) (sub1 end)))
              (lex-port p actual)))))

(define-syntax-rule (trace->json offset)
  (local [(define (colors src)
            (map (lambda (lst)
                   (hasheq 'type (format "~a" (first lst))
                           'text (format "~a" (second lst))))
                 (lex-port (open-input-string src) src)))]
    (format "var theTrace = ~a\nvar code = ~a\nvar codeOffset = ~a"
            (jsexpr->json (node->json (current-call)))
            (jsexpr->json (colors (unbox src)))
            offset)))


(define-for-syntax (print-expanded d)
  (printf "~a\n"
          (syntax->datum (local-expand d 'module (list)))))

(define (page json)
    (let ([tracerCSS 
           (port->string (open-input-file (resolve-planet-path 
                                           '(planet tracer/tracer/tracer.css))))]
          [jQuery 
           (port->string (open-input-file (resolve-planet-path
                                           '(planet tracer/tracer/jquery.js))))]
          [tracerJS 
           (port->string (open-input-file (resolve-planet-path
                                           '(planet tracer/tracer/tracer.js))))]
          [treeOfTrace json])
      (include-template "index.html")))


;adds trace->json and send-url to the end of the file
(define-syntax (#%module-begin stx)
  (syntax-case stx ()
    [(_ source offset body ...)
     #`(#%plain-module-begin
        (set-box! src source)
        body ...
        (run-tests)
        (display-results)
        (begin
          ;If empty trace generate error message
          (if (equal? empty (node-kids (current-call)))
              (message-box "Error" 
                           "This file cannot be traced because none of functions defined within it are called. " #f '(ok stop))
               (send-url/contents (page (trace->json offset)))))
       )]))


