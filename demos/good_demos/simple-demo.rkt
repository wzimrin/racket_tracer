;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname simple-demo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;#lang racket

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

(fib 5)
(fib-iterative 5)