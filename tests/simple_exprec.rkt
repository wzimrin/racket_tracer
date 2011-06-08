;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname simple_exprec) (read-case-sensitive #t) (teachpacks ((lib "cs019.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "cs019.rkt" "installed-teachpacks")))))
(define (fib x)
  (if (< x 2)
      x
      (+ (fib (- x 1))
         (fib (- x 2)))))

(define (squared x)
  (* x x))

(define (dist x1 y1 x2 y2)
  (sqrt (+ (squared (- x1 x2))
           (squared (- y1 y2)))))

(define (close-enough? x y)
  (< (dist x y 0 0) 4))

(fib 10)
(close-enough 3 3)