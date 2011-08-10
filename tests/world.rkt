#lang planet tracer/tracer
(require [only-in racket curry])
#|
(require 2htdp/universe)
(require htdp/image)


(define MAX-SIZE 25)

(define-struct world (num))

(define (tick w)
 (make-world (add1 (world-num w))))

(define (DRAW w)
  (circle (world-num w) "solid" "blue"))

(define (key w a-key)
  (cond
    [(key=? a-key "up") (if (< (world-num w) MAX-SIZE)
                            (make-world (add1 (world-num w)))
                            w)]
    [(key=? a-key "down") (if (> (world-num w) 0)
                              (make-world (sub1 (world-num w)))
                              w)]))
(define (stop w)
  (< MAX-SIZE (world-num w)))
  
#;(big-bang (make-world 1)
            [on-tick tick 1]
            [on-key key]
            [to-draw DRAW 
                     100
                     100]
            [stop-when stop])
|#
(define (make-circle x)
  (circle x "outline" "black"))
;(make-circle 4)
(big-bang '(20 10)
          [on-tick (lambda (lst)
                     (cons (+ 10 (first lst))
                           lst)) 1]
          [stop-when (lambda (lst)
                       (= (first lst) 50))]
          [to-draw (compose (curry apply overlay)
                            reverse
                            (curry map
                                   make-circle))
                   200 200])