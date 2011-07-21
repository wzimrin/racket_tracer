#lang planet tracer/tracer

(check-expect (fib 3) 2)
(check-expect (fib 4) (+ (fib 3) (fib 2)))

(define (fib x)
  (if (<= x 1)
      x
      (+ (fib (- x 1))
         (fib (- x 2)))))

(fib 6)

(check-expect (take (list 1 2 3 4) 2)
              (list 1 2))
(define (take lst n)
  (cond
    [(empty? lst) empty]
    [(cons? lst)
     (if (= n 0)
         empty
         (cons (first lst)
               (take (rest lst)
                     (sub1 n))))]))

(define (add-list lst)
  (foldl - 0 lst))

(define (add-first-n lst n)
  (add-list (take lst 2)))
