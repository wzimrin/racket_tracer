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
(define check-color "limegreen")
(define circle-size 4)
(define circle-handle-offset 2)
(define source-stroke-width 2)

(define (make-checkbox check-color foreground-color background)
  (let ([line-pen (make-pen check-color check-stroke-width "solid" "round" "round")])
    (add-curve (add-line (overlay (square checkbox-size "outline" (make-pen foreground-color 
                                                                           checkbox-stroke-width
                                                                           "solid"
                                                                           "projecting"
                                                                           "miter"))
                                 background)
                        4 9 7 13
                        line-pen)
              7 13 45 1/3
              15 5 20 1/3
              line-pen)))

(define (make-src-button foreground-color background)
  (add-line (overlay/offset (circle circle-size "outline"
                                    (make-pen foreground-color source-stroke-width
                                              "solid" "round" "round"))
                            2 2
                            background)
            10 10 15 15
            (make-pen foreground-color source-stroke-width
                      "solid" "butt" "miter")))
  
(define normal-background (square button-size "solid" normal-background-color))

(define highlight-background (square button-size "solid" highlight-background-color))

(define normal-up-arrow 
  (overlay (rounded-triangle triangle-size triangle-stroke-width "solid" normal-foreground-color)
           normal-background))

(define normal-down-arrow
  (flip-vertical normal-up-arrow))

(define normal-checkbox
  (make-checkbox check-color normal-foreground-color normal-background))

(define normal-src-button
  (make-src-button normal-foreground-color normal-background))

(define highlight-checkbox
  (make-checkbox check-color highlight-foreground-color highlight-background))

(define highlight-src-button
  (make-src-button highlight-foreground-color highlight-background))

(provide normal-up-arrow
         normal-down-arrow
         normal-checkbox
         normal-src-button
         highlight-checkbox
         highlight-src-button)
