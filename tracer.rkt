#lang racket

(require [except-in lang/htdp-intermediate 
                    #%app define lambda require])
(require [prefix-in isl:
                    [only-in lang/htdp-intermediate 
                             define lambda require let]])

(provide [rename-out (app-recorder #%app)])
(provide [all-from-out lang/htdp-intermediate])
(provide [rename-out (isl:define define)
                     (isl:lambda lambda)
                     (isl:require require)
                     (isl:let let)])

(provide show-trace trace->json)

(struct node (formal result actual kids) #:mutable #:transparent)
(define (create-node f a)
  (node f 'no-result a empty))

(define (add-kid n k)
  (set-node-kids! n (cons k (node-kids n))))

(define current-call (make-parameter (create-node 'top-level 'top-level)))

(define-for-syntax blocked-fun-names '(+ - * / 
                                         zero? add1 sub1
                                         cons first rest))

(define-syntax (app-recorder e)
  (syntax-case e ()
    
    [(_ fun-expr arg-expr ...) 
     ;ensure that fun-expr is a function
     (identifier? #'fun-expr) 
     ;result-expr -- is [block blocked-fun-names] just for ease of reading the code?
     
     (with-syntax ([blocked blocked-fun-names])
       #'(if (member 'fun-expr 'blocked)
             ;if not a function you want to trace, leave as is
             (#%app fun-expr arg-expr ...)
             ;otherwise trace
             (let ([n (create-node '(fun-expr arg-expr ...)
                                   "nothing here yet!")])
               (begin
                 ;adds n to current-call's kids 
                 (add-kid (current-call) n)
                 (parameterize ([current-call n])
                   (let* ([fun fun-expr]
                          [args (list arg-expr ...)])
                     (begin
                       (set-node-actual! n `(fun-expr ,@args))#|(map (lambda(an-arg)
                                                               (if (identifer? an-arg)
                                                                   ...
                                                                   ...)) 
                                                             args)))|#
                       (let ([v (#%app apply fun args)])
                         (begin
                           (set-node-result! n v)
                           v)))))))))]))

(define (print-right t)
  (node (node-formal t)
        (node-result t)
        (node-actual t)
        (reverse (map print-right (node-kids t)))))

; Why is this a macro and not a function?  Because make it a function
; affects the call record!

(define-syntax-rule (show-trace)
  (print-right (current-call)))

(define (node->json t)
 
  (format "{formals: \"~a\",
            actuals: \"~a\",
            result: \"~a\",
            children: [~a]}"
          (node-formal t)
          (node-actual t)
          (node-result t)
          (if (empty? (node-kids t))
              ""
              (local ([define (loop k)
                        (if (empty? (rest k))
                            (first k)
                            (string-append (first k)
                                           ","
                                           (loop (rest k))))])
                (loop (map node->json #|(reverse|# (node-kids t)))))))#|)|#

; Why is this a macro and not a function?  Because make it a function
; affects the call record!




(define-syntax-rule (trace->json)
    (with-output-to-file "tree-of-trace.js"
      (lambda ()
        (display (format "var theTrace = ~a" (node->json (current-call)))))
    #:exists 'replace))
