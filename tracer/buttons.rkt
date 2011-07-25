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
(define highlight-background-color (html-color "#ffff61"))
(define highlight-foreground-color "fireBrick")
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

(define (make-src-button2 txt fg-color bg style)
  (c (overlay (text/font txt 12 fg-color
                    #f style
                    'normal
                    'bold
                    #f) bg)))

;trapezoid book
(define (make-src-button3 fgc bgc bg)
  (let* ([triangle (right-triangle (/ button-size 2.5) 
                                   (/ button-size 8) 
                                   "solid" fgc)]
         [rect (rectangle (/ button-size 2.5) (/ button-size 2) "solid" fgc)]
         [trap (above triangle rect (flip-vertical triangle))]
         [book (beside trap (flip-horizontal trap))])
    (c (overlay book bg))))

;ellipse book
(define (make-src-button4 fgc bgc bg)
  (let* ([t-el (ellipse (/ button-size 2.5) (/ button-size 5) "solid" fgc)]
         [b-el (ellipse (/ button-size 2.5) (/ button-size 5) "solid" bgc)]
         [top-semi (crop 0 0 (image-width t-el) (/ (image-height t-el) 3) t-el)]
         [bottom-semi (crop 0 0 (image-width b-el) (/ (image-height b-el) 3) b-el)]
         [bottom-semi (overlay bottom-semi (rectangle (image-width bottom-semi)
                                                      (image-height bottom-semi)
                                                      "solid"
                                                      fgc))]
         [rect (rectangle (/ button-size 2.5) (/ button-size 2) "solid" fgc)]
         [rect (add-line rect
                         (image-width rect) 0
                         (image-width rect) (image-height rect)
                         bgc)]
         [side (above top-semi rect bottom-semi)]
         [book (beside side (flip-horizontal side))])
    (c (overlay book bg))))

(define (c img)
  (freeze 0 0 button-size button-size img))

(define normal-background
  (c (square button-size "solid" normal-background-color)))

(define highlight-background
  (c (square button-size "solid" highlight-background-color)))

(define normal-up-arrow 
  (c (overlay (rounded-triangle triangle-size triangle-stroke-width "solid" normal-foreground-color)
              normal-background)))

(define normal-side-arrow
  (c (rotate 270 normal-up-arrow)))

(define normal-down-arrow
  (c (flip-vertical normal-up-arrow)))

(define normal-correct-checkbox
  (make-correct-checkbox correct-check-color normal-foreground-color normal-background))

(define normal-failed-checkbox
  (make-failed-checkbox failed-check-color normal-foreground-color normal-background))

;;Options for source buttons

;lowercase d
(define normal-src-button2
  (make-src-button2 "d" normal-foreground-color normal-background 'symbol))

(define normal-src-button3
  (make-src-button2 "d" normal-foreground-color normal-background 'decorative))

(define normal-src-button4
  (make-src-button2 "d" normal-foreground-color normal-background 'modern))

;lowercase delta
(define normal-src-button5
  (make-src-button2 "δ" normal-foreground-color normal-background 'symbol))

;book with straight sides
(define normal-src-button6
  (make-src-button3 normal-foreground-color normal-background-color normal-background))

;book with curved sides
(define normal-src-button7
  (make-src-button4 normal-foreground-color normal-background-color normal-background))

;Magnifying glass
(define normal-src-button8
  (make-src-button normal-foreground-color normal-background))

;Sets which source buttons are used in the tracer
;To change bind normal-src-button to the button you want
;And highlight-src-button to the same buttom with highlight-foreground-color and highlight-background-color
(define normal-src-button normal-src-button5)
(define highlight-src-button
  (make-src-button2 "δ" highlight-foreground-color highlight-background 'symbol))
  
;;End of source buttons

(define highlight-correct-checkbox
  (make-correct-checkbox correct-check-color highlight-foreground-color highlight-background))

(define highlight-failed-checkbox
  (make-failed-checkbox failed-check-color highlight-foreground-color highlight-background))

(provide normal-side-arrow
         normal-down-arrow
         normal-correct-checkbox
         normal-failed-checkbox
         normal-src-button
         highlight-correct-checkbox
         highlight-failed-checkbox
         highlight-src-button
         button-size)
