#lang racket

(require [except-in lang/htdp-intermediate-lambda
                    #%app define lambda require #%module-begin let local define-struct])
(require [prefix-in isl:
                    [only-in lang/htdp-intermediate-lambda
                             define lambda require let local define-struct]])

(require racket/pretty)
(require net/sendurl)

(provide let local define)

(provide [rename-out (app-recorder #%app)])
;(provide app-recorder)
(provide [all-from-out lang/htdp-intermediate-lambda])
(provide [rename-out #;(isl:define define)
                     (isl:lambda lambda)
                     (isl:require require)
                     (ds-recorder define-struct)
                     #;(isl:let let)])

;(provide struct-accessor-procedure?)

(provide show-trace trace->json #%module-begin)

(struct node (name formal result actual kids) #:mutable #:transparent)
(define (create-node n f a)
  (node n f 'no-result a empty))

(define (add-kid n k)
  (set-node-kids! n (cons k (node-kids n))))

(define current-call (make-parameter (create-node 'top-level empty empty)))

#;(define blocked-fun-names 
    (namespace-mapped-symbols
     (module->namespace 'lang/htdp-intermediate-lambda)))

(define ds-fun-names (box empty))

(define (register-ds name fields)
  (let* ([name-s (symbol->string name)]
         [name-s-d (string-append name-s
                                 "-")])
    (set-box! ds-fun-names
              (append (map string->symbol (list (string-append "make-" name-s)
                                                (string-append name-s "?")))
                      (map (lambda (field)
                             (string->symbol 
                              (string-append name-s-d
                                             (symbol->string field))))
                           fields)
                      (unbox ds-fun-names)))))

(define-syntax (ds-recorder e)
  (syntax-case e ()
    [(define-struct name fields)
     #'(begin (isl:define-struct name fields)
              (register-ds 'name 'fields))]))

(define-syntax (app-recorder e)
  (syntax-case e ()
    
    [(_ fun-expr arg-expr ...) 
     ;ensure that fun-expr is a function
     (identifier? #'fun-expr) 
     ;result-expr -- is [block blocked-fun-names] just for ease of reading the code?
     (let* ([binding (identifier-binding #'fun-expr)]
            [vals (if (list? binding)
                      (call-with-values
                       (lambda ()
                         (module-path-index-split (car binding)))
                       list)
                      binding)])
       (if (or (equal? vals 'lexical)
               (equal? vals '(#f #f)))
           #'(if (or (member 'fun-expr (unbox ds-fun-names))
                     (struct-accessor-procedure? fun-expr))
                 ;if not a function you want to trace, leave as is
                 (#%app fun-expr arg-expr ...)
                 ;otherwise trace
                 (let ([n (create-node 'fun-expr '(arg-expr ...)
                                       "nothing here yet!")])
                   (begin
                     ;adds n to current-call's kids 
                     (add-kid (current-call) n)
                     (let* ([fun fun-expr]
                            [args (list arg-expr ...)])
                       (parameterize ([current-call n])
                         (begin
                           (set-node-actual! n args)
                           (let ([v (#%app apply fun args)])
                             (begin
                               (set-node-result! n v)
                               v))))))))
           #'(#%app fun-expr arg-expr ...)))]))

(define (print-right t)
  (node (node-formal t)
        (node-result t)
        (node-actual t)
        (reverse (map print-right (node-kids t)))))

; Why is this a macro and not a function?  Because make it a function
; affects the call record!

(define-syntax-rule (show-trace)
  (print-right (current-call)))

(define (format-nicely x depth width literal)
  (format "~S" (let [(p (open-output-string "out"))]
    (parameterize [(pretty-print-columns width)
                   (pretty-print-depth depth)]
      ((if literal
           pretty-print
           pretty-display) x p))
    (get-output-string p))))

(define (node->json t)
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
            children: [~a]}"
           (node-name t)
           (format-list (node-formal t) #f #f)
           (format-list (node-formal t) 4 #f)
           (format-list (node-actual t) #f #t)
           (format-list (node-actual t) 4 #t)
           (format-nicely (node-result t) #f 40 #t)
           (format-nicely (node-result t) 4 40 #t)
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
    (with-output-to-file "tree-of-trace.js"
      (lambda ()
        (display (format "var theTrace = ~a" (node->json (current-call)))))
    #:exists 'replace))

(define-for-syntax (print-expanded d)
  (printf "~a\n"
          (syntax->datum (local-expand d 'module (list)))))

(define-syntax (#%module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     #`(#%plain-module-begin
        body ...
        (trace->json)
        (send-url "index.html"))]))