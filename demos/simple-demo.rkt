#lang planet tracer/tracer

(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (fib-iterative n)
  (local [(define (iter a b i)
            (if (= i n)
                a
                (iter b (+ a b) (add1 i))))]
    (iter 0 1 0)))

(define (add-2-to-all lst)
  (map (lambda (x)
         (+ x 2))
       lst))

(fib 5)
(fib-iterative 5)
(add-2-to-all (list 1 2 3 4))