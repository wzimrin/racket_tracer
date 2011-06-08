;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test_exprec) (read-case-sensitive #t) (teachpacks ((lib "cs019.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "cs019.rkt" "installed-teachpacks")))))

(require net/sendurl)

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
(: how-many (dir -> Number))
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
(trace->json)
(send-url "index.html")