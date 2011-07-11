#lang htdp/isl+

(require 2htdp/universe)
(require htdp/image)

(define-struct world (num))

(big-bang (make-world 1)
          [on-tick (lambda(w) (make-world (add1 (world-num w))))]
          [to-draw (lambda(w) (circle (world-num w) "solid" "blue"))
                   100
                   100]
          [stop-when (lambda(w) (< 50 (world-num w)))])