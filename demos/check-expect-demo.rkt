#lang planet tracer/tracer:1:3

#|
Shows how check expects are displayed.  Check expects that pass are uninteresting,
so are not displayed.  Check expects that fail, however, are displayed.  In this case,
we also see the usefulness of the tracer.  The bug in these functions is in dist, but
due to the poor choice of tests, the bug doesn't show up until the test for 
close-enough?.  However, when we inspect what happened in the incorrect call to
close-enough?, we can easily see that the error (or at least an error) lies in dist.
|#

(define close-enough-dist 3)

(check-expect (square 2) 4)
(check-expect (square 3) 9)
(define (square x)
  (* x x))

(check-expect (dist 4 5 1 5) 3)
(check-expect (dist 10 2 3 2) 7)
(define (dist x1 y1 x2 y2)
  (sqrt (+ (square (- x1 x2))
           (- y1 y2))))

(check-expect (close-enough? '(2 2)) #t)
(check-expect (close-enough? '(2 3)) #f)
(define (close-enough? pt)
  (< (dist (first pt) (second pt) 0 0)
     close-enough-dist))

(check-expect (find-close-enough? '((1 2) (2 3) (2 2)))
             '((1 2) (2 2))) 
(define (find-close-enough? lst)
  (filter close-enough? lst))

(find-close-enough? '((1 2) (2 3) (2 2)))