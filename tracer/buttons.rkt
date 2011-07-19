#lang racket

(require 2htdp/image)

(define (html-color s)
  (make-color (string->number (substring s 1 3) 16)
              (string->number (substring s 3 5) 16)
              (string->number (substring s 5 7) 16)))

(define (rounded-triangle side-length stroke-width mode color)
  (let* ([p (make-pen color stroke-width "solid" "round" "round")]
         [t (triangle side-length mode color)]
         [t-stroke (triangle side-length "outline" p)]
         [w (image-width t)]
         [h (image-height t)])
    (overlay t-stroke t)))

(define button-size 18)
(define triangle-size 10)
(define triangle-stroke-width 3)
(define normal-background-color (html-color "#000000"))
(define normal-foreground-color "white")
(define highlight-background-color (html-color "#ffff9e"))
(define highlight-foreground-color "black")
(define checkbox-size 13)
(define checkbox-stroke-width 1)
(define check-stroke-width 3)
(define correct-check-color "limegreen")
(define failed-check-color "red")
(define circle-size 4)
(define circle-handle-offset 2)
(define source-stroke-width 2)

(define (make-checkbox foreground-color background)
  (overlay (square checkbox-size "outline" (make-pen foreground-color 
                                                     checkbox-stroke-width
                                                     "solid"
                                                     "projecting"
                                                     "miter"))
           background))

(define (make-correct-checkbox check-color foreground-color background)
  (let ([line-pen (make-pen check-color check-stroke-width "solid" "round" "round")])
    (c (add-curve (add-line (make-checkbox foreground-color background)
                            4 9 7 13
                            line-pen)
                  7 13 45 1/3
                  15 5 20 1/3
                  line-pen))))

(define (make-failed-checkbox check-color foreground-color background)
  (let ([line-pen (make-pen check-color check-stroke-width "solid" "round" "round")])
    (add-line (add-line (make-checkbox foreground-color background)
                        5 5 13 13
                        line-pen)
              5 13 13 5
              line-pen)))

(define (make-src-button foreground-color background)
  (c (add-line (overlay/offset (circle circle-size "outline"
                                       (make-pen foreground-color source-stroke-width
                                                 "solid" "round" "round"))
                               2 2
                               background)
               10 10 15 15
               (make-pen foreground-color source-stroke-width
                         "solid" "butt" "miter"))))

(define (c img)
  (freeze 0 0 button-size button-size img))

(define normal-background
  (c (square button-size "solid" normal-background-color)))

(define highlight-background
  (c (square button-size "solid" highlight-background-color)))

(define normal-up-arrow 
  (c (overlay (rounded-triangle triangle-size triangle-stroke-width "solid" normal-foreground-color)
              normal-background)))

(define normal-down-arrow
  (c (flip-vertical normal-up-arrow)))

(define normal-correct-checkbox
  (make-correct-checkbox correct-check-color normal-foreground-color normal-background))

(define normal-failed-checkbox
  (make-failed-checkbox failed-check-color normal-foreground-color normal-background))

(define normal-src-button
  (make-src-button normal-foreground-color normal-background))

(define highlight-correct-checkbox
  (make-correct-checkbox correct-check-color highlight-foreground-color highlight-background))

(define highlight-failed-checkbox
  (make-failed-checkbox failed-check-color highlight-foreground-color highlight-background))

(define highlight-src-button
  (make-src-button highlight-foreground-color highlight-background))

(provide normal-up-arrow
         normal-down-arrow
         normal-correct-checkbox
         normal-failed-checkbox
         normal-src-button
         highlight-correct-checkbox
         highlight-failed-checkbox
         highlight-src-button
         button-size)
