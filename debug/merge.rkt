#lang planet tracer/tracer


;Consumes an integer n greater than 0 and produces of (list-of (list-of integers)) where the outer and all of the inner lists are of length n, and in any given inner list each number from 0 to n-1 appears exactly one time
(define (generate-input n)
  (build-lists n (+ n 1)))

;Consumes an integer n greater than 0 indicating the wanted size for each list
;A second integer the current number of lists left to create
;Produces a (list-of (list-of Integers)) where each list contains the numbers from 0 to n-1 exactly once
(define (build-lists n to-make)
  (cond
    [(= to-make 1) (random-list (- n 1))]
    [(> to-make 1) (cons (random-list (+ n 1))
                         (build-lists n (- to-make 1)))]))

;Consumes an integer to-add and produces a list of integers where each number from 0 to to-add inclusive appears exactly one time
(define (random-list to-add)
  (cond 
    [(= to-add 0) (list 0)]
    [(> to-add 0) (ins-at to-add 
                          (random to-add) 
                          (random-list (- to-add 1)))]))

;Consumes two integers, n and pos, and a list of integers. Produces a list with the the integer n inserted into the list of integers at pos.
;Will shift everything from pos on one spot to the right
(define (ins-at num pos list)
  (cond
    [(= pos 0) (cons num list)]
    [(> pos 0) (cons num (ins-at num (- pos 1) (rest list)))]))

(generate-input 5)