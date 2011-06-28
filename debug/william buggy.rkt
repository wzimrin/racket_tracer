#lang planet tracer/tracer

;concatenates two strings
(define (concat xs ys)
  (if (empty? xs)
      ys
      (cons (first xs)
            (concat (rest xs) ys))))

;maps a function that returns a list across a list
;and concatenates all the resulting lists together
(define (map-concat f l)
  (cond [(empty? l) empty]
        [(cons? l) (append (f (first l))
                           (map-concat f (rest l)))]))

;drops the first instance of x from lst
(define (drop x lst)
  (cond
    [(empty? lst) empty]
    [(equal? (first lst) x) (rest lst)]
    [#t (cons (first lst) (drop x (rest lst)))]))

;a reimplementation of map
(define (my-map f lst)
  (cond
    [(empty? lst) empty]
    [(cons? lst) (cons (f (first lst))
                       (my-map f (rest lst)))]))

;calculates a list of all the permutations of lst
(define (permutations lst)
  (cond
    [(empty? lst) empty]
    [(cons? lst) (map-concat
                  (lambda (num)
                    (my-map (lambda (a-perm)
                               (cons num a-perm))
                             (permutations 
                              (drop num lst))))
                  lst)]))

(permutations (list 1 2 3 4))