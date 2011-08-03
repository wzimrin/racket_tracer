#lang racket

(require (prefix-in kernel: syntax/kerncase))

(require syntax/strip-context)
(require "std_annotate.rkt")

(define (make-syntax-hash src)
  (letrec ([syntaxes (parameterize ([read-accept-reader #t])
                       (let iter ([vals empty])
                         (let ([v (read-syntax #f src)])
                           (if (eof-object? v)
                               (reverse vals)
                               (iter (cons v vals))))))]
           [hash (make-hash)]
           [iter (lambda (syntax)
                   (and (syntax-position syntax)
                        (syntax-span syntax)
                        (hash-set! hash
                                   (list (syntax-position syntax)
                                         (syntax-span syntax))
                                   syntax))
                   (let ([datum (syntax-e syntax)])
                     (when (list? datum)
                       (map iter datum))))])
    (for ([s syntaxes])
      (iter s))
    hash))

(define pre-code
  #'((struct node (name func formal result actual kids linum idx span src-idx src-span) #:mutable #:transparent)
     ;(provide node)
     (define (create-node n func f a l i s s-i s-s)
       (node n func f 'no-result a empty l i s s-i s-s))
     (define current-call (make-parameter (create-node 'top-level #f empty empty 0 0 0 0 0)))
     (define current-linum (make-parameter 0))
     (define current-idx (make-parameter 0))
     (define current-span (make-parameter 0))
     (define current-fun (make-parameter #f))
     (define current-app-call (make-parameter empty))
     (define (function-sym datum)
       (if (cons? datum)
           (function-sym (first datum))
           datum))
     ;adds a kid k to node n
     (define (add-kid n k)
       (set-node-kids! n (cons k (node-kids n))))
     #;(provide (all-defined-out))))

(begin
  (struct node (name func formal result actual kids linum idx span src-idx src-span)
    #:mutable #:transparent)
  ;(provide node)
  (define (create-node n func f a l i s s-i s-s)
    (node n func f 'no-result a empty l i s s-i s-s))
  (define current-call (make-parameter (create-node 'top-level #f empty empty 0 0 0 0 0)))
  (define current-linum (make-parameter 0))
  (define current-idx (make-parameter 0))
  (define current-span (make-parameter 0))
  (define current-fun (make-parameter #f))
  (define current-app-call (make-parameter empty))
  (define (function-sym datum)
    (if (cons? datum)
        (function-sym (first datum))
        datum))
  ;adds a kid k to node n
  (define (add-kid n k)
    (set-node-kids! n (cons k (node-kids n)))))

(provide create-node function-sym add-kid
         current-call current-linum current-idx current-span current-fun current-app-call)

(define (lambda-body args body name orig fun)
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

;returns a function that applies arg-fun to every argument passed
;and calls fun on the result.  (on equal? struct-value) checks if
;the passed structs have equal? values
(define ((on fun arg-fun) . args)
  (apply fun (map arg-fun args)))

(define (annotate stx src)
  (displayln src)
  (define syntax-hash
    (make-syntax-hash src))
  (display syntax-hash)
  
  (define (lambda-tracer expanded original)
    (syntax-case expanded (#%plain-lambda)
      [(#%plain-lambda args body ...)
       (syntax-case* original (define lambda) (on equal? syntax->datum)
         [(define (name a ...) bd ...)
          #`(#%plain-lambda args
                            #,(lambda-body #'(list . args) #'(list body ...) #'name original #'name))]
         [(lambda as bd ...)
          (let ([sym (gensym)])
            #`(letrec ([#,sym (lambda args)])
                (#%plain-lambda args
                                #,(lambda-body #'(list . args)
                                               #'(list body ...)
                                               #'lambda
                                               original
                                               sym))))])]))
  
  (define annotated-stx-list
    (map (lambda (x)
           (let-values 
               ([(annotated-stx breaks)
                 (annotate-stx 
                  x
                  (lambda (frame expanded original is-tail?)
                    (kernel:kernel-syntax-case 
                     original #f 
                     [(#%plain-lambda args body ...)
                       (lambda-tracer expanded 
                                      (hash-ref syntax-hash
                                                (list (syntax-position original)
                                                      (syntax-span original))))]
                     [(#%plain-app fun-expr arg-expr ...)
                      (let* ([orig-syntax (hash-ref syntax-hash
                                                     (list (syntax-position original)
                                                           (syntax-span original)))])
                         (let ([linum (syntax-line original)]
                               [idx (syntax-position original)]
                               [span (syntax-span original)])
                           #'(let* ([fun fun-expr]
                                    [args (list arg-expr ...)]  
                                    [n (create-node (function-sym 'orig-syntax) fun empty args
                                                    linum idx span 0 0)]
                                    [result (parameterize ([current-linum ,linum]
                                                           [current-idx ,idx]
                                                           [current-span ,span]
                                                           [current-fun fun]
                                                           [current-app-call n])
                                              (apply fun args))])
                               #;(set-node-result! n result)
                               #;(add-kid (current-call) n)
                               (when (not (empty? (node-kids n)))
                                 (set-node-result! n result)
                                 (add-kid (current-call) n))
                               result)))]
                     [_ expanded]))
                  (lambda (a b c)
                    (void))
                  (lambda (a b c)
                    (void)))])
             annotated-stx))
         stx))
  
  (map (lambda (annotated-stx)
         (syntax-case annotated-stx (module)
           [(module some-name some-lang (modbeg topform ...))
            (cons
             (quasisyntax/loc annotated-stx
               (module some-name some-lang
                 (modbeg 
                  ;#,@(strip-context pre-code)
                  topform ... 
                  (provide (all-defined-out)))))
             (syntax->datum #''some-name))]
           [_ (cons annotated-stx #f)]))
       annotated-stx-list))

(provide annotate)