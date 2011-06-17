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

(define (drop x lst)
  (cond
    [(empty? lst) empty]
    [(equal? (first lst) x) (rest lst)]
    [#t (cons (first lst) (drop x (rest lst)))]))

(define (collect f lst)
  (cond
    [(empty? lst) empty]
    [(cons? lst) (cons (f (first lst))
                       (collect f (rest lst)))]))

(define (permutations lst)
  (cond
    [(empty? lst) empty]
    [(cons? lst) (map-concat
                  (lambda (num)
                    (collect (lambda (perms)
                               (cons num perms))
                             (permutations
                              (drop num lst))))
                  lst)]))

(permutations (list 1 2 3 4))