#lang racket

;the actual struct that stores our data
(struct node (name func formal result actual kids linum idx span src-idx src-span) #:mutable #:transparent)

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

;-- What does this do?
(define (function-sym datum)
  (if (cons? datum)
      (function-sym (first datum))
      datum))

(define (match stx)
  (syntax-case stx (#%app #%module-begin)
    [(#%app fun-expr arg-expr ...) 
     (let ([orig-syntax (hash-ref syntax-hash '((syntax-position stx) (syntax-span stx)))])
     (with-syntax ([linum (syntax-line stx)]
                   [idx (syntax-position stx)]
                   [span (syntax-span stx)]
                   [orig-function (syntax-case orig-syntax ()
                                    [(_ orig-fun . _) orig-fun])])
       #'(let* ([ofun orig-function]
                [fun fun-expr]
                [args '(arg-expr ...)]       
                [n (create-node (function-sym 'orig-function) fun empty args
                                linum idx span 0 0)]
                [result (parameterize ([current-linum linum]
                                       [current-idx idx]
                                       [current-span span]
                                       [current-fun fun]
                                       [current-app-call n])
                          (apply fun args))])
           (set-node-result! n result)
           (add-kid (current-call) n)
           #;(when (not (empty? (node-kids n)))
               (set-node-result! n result)
               (add-kid (current-call) n))
           result)))]
    [(#%module-begin name body ...)
     #`(#%plain-module-begin
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
