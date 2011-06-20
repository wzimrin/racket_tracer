#lang s-exp "tracer.rkt"

(define (concat xs ys)
  (if (empty? xs)
      ys
      (cons (first xs)
            (concat (rest xs) ys))))

(define (map-concat f l)
  (cond [(empty? l) empty]
        [(cons? l) (append (f (first l))
                           (map-concat f (rest l)))]))
#|
(check-expect (map-concat (λ(x) (list (add1 x)))
                          (list 2 3 4 5))
              (list 3 4 5 6))
(check-expect (map-concat rest
                          (list (list 0 1 2)
                                (list 1 2 3)
                                (list 3 4 5)))
              (list 1 2 2 3 4 5))|#


(define (drop x lst)
  (cond
    [(empty? lst) empty]
    [(equal? (first lst) x) (rest lst)]
    [#t (cons (first lst) (drop x (rest lst)))]))

#|(check-expect (drop 1 (list 1)) empty)
(check-expect (drop 2 (list 2 1)) (list 1))
(check-expect (drop 2 (list 1 2)) (list 1))
(check-expect (drop 3 (list 1 3 2)) (list 1 2))|#

(define (my-map f lst)
  (cond
    [(empty? lst) empty]
    [(cons? lst) (cons (f (first lst))
                       (my-map f (rest lst)))]))

#|
(check-expect (my-map add1 (list 1)) (list 2))
(check-expect (my-map add1 (list 3 1 2)) (list 4 2 3))
(check-expect (my-map rest (list (list 1 2) (list 1 2 3) (list 1 2 3 4)))
              (list (list 2) (list 2 3) (list 2 3 4)))|#



(define (permutations lst)
  (cond
    [(empty? lst) (list empty)]
;    [(empty? (rest lst)) (list lst)]
    [(cons? lst) (map-concat
                  (lambda (num)
                    (my-map (lambda (a-perm)
                               (cons num a-perm))
                             (permutations 
                              (drop num lst))))
                  lst)]))

(permutations (list 1 2 3 4))
(permutations (list 1))

(lambda (num)(my-map (lambda (a-perm) (cons num a-perm))
                     (permutations (drop num lst))))
