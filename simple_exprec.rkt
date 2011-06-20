#lang planet tracer/tracer

(let [(x (lambda (y) y))]
  (x 2))


(define (add x y)
  (+ x y))

(add 1 2)

(define (fib x)
  (if (< x 2)
      x
      (+ (fib (- x 1))
         (fib (- x 2)))))

(define (squared x)
  (* x x))

(define (dist x1 y1 x2 y2)
  (sqrt (add (squared (- x1 x2))
             (squared (- y1 y2)))))

(define (close-enough? x y)
  (< (dist x y 0 0) 4))

(fib 10)
(close-enough? 3 3)

(define (all-close-enough? xs)
  (andmap (lambda (x)
            (close-enough? (first x) (second x)))
          xs))

(all-close-enough? '((1 1) (2 2) (3 3) (4 4)))

;(send-url "index.html")

#;(begin0 (let-values (((x) (lambda (y) y)))
            (letrec-syntaxes+values (((x1) (make-undefined-check (quote-syntax check-not-undefined)
                                                                 (quote-syntax x))))
              ()
              (letrec-syntaxes+values (((x) (values (make-rename-transformer (quote-syntax x1)))))
                ()
                (#%app (#%app check-not-undefined 'x x) '1)))))