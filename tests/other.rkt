#lang planet tracer/tracer

(define (fib x)
  (if (< x 2)
      x
      (+ (fib (- x 1))
         (fib (- x 2)))))

(define (add x y)
  (+ x y))

(define (squared x)
  (* x x))

(define (dist x1 y1 x2 y2)
  (sqrt (add (squared (- x1 x2))
             (squared (- y1 y2)))))

(define (close-enough? x y)
  (< (dist x y 0 0) 4))

(define (all-close-enough? xs)
  (andmap (lambda (x)
            (close-enough? (first x) (second x)))
          xs))

(fib 10)

(close-enough? 3 3)

(all-close-enough? '((1 1) (2 2) (3 3) (4 4)))

(check-expect (fib 5) (+ (fib 4) (fib 3)))
(check-expect (fib 4) (+ (fib 3) (fib 3)))
(check-expect (fib 3) 1)

(define-struct dir (name dirs files))
(define-struct file (name size content))

;Capital letters are folders, lower case are files
;Folder A has folders B, C, D and files e, f, g in it
;A
;B              C          D  e f g
;H   I j k      l m        n
;o   

(define e1 (make-file "e" 1 empty))
(define f (make-file "f" 1 empty))
(define g (make-file "g" 1 empty))
(define j (make-file "j" 1 empty))
(define k (make-file "k" 1 empty))
(define l (make-file "l" 1 empty))
(define m (make-file "m" 1 empty))
(define n (make-file "n" 1 empty))
(define o (make-file "o" 1 empty))
(define H (make-dir "H" empty (list o)))
(define I (make-dir "I" empty empty))
(define B (make-dir "B" (list H I) (list j k)))
(define C (make-dir "C" empty (list l m)))
(define D (make-dir "D" empty (list n)))
(define A (make-dir "A" (list B C D) (list e1 f g)))

;Consumes: a directory
;Produces: number of files in this directory and subdirectories
;(: how-many (dir -> Number))
(define (how-many d)
  (cond
    ;No more directories - number of files in this directory
    [(empty? (dir-dirs d)) (length (dir-files d))] 
    ;More directories - number of files in this directory and in subdirectories 
    [(cons? (dir-dirs d))
     (+ (length (dir-files d))
        (how-many (first (dir-dirs d)))
        (how-many (make-dir
                   (dir-name d)
                   (rest (dir-dirs d))
                   empty)))]))

;9
(how-many A)