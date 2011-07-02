#lang planet tracer/tracer:1:1

(require 2htdp/image)

(check-expect (sq 2) 4)
(define (sq x)
  (* x x))

(check-expect (add-point (list 1 2) (list 3 4))
              (list 4 6))
(define (add-point p1 p2)
  (map + p1 p2))

(check-expect (scale-point 2 (list 1 2))
              (list 2 4))
(define (scale-point x p)
  (map (lambda (i) (* x i))
       p))

(check-expect (sub-point (list 3 4) (list 1 2))
              (list 2 2))
(define (sub-point p1 p2)
  (map - p1 p2))

(check-expect (dist (list 1 2) (list 4 6)) 5)
(define (dist p1 p2)
  (sqrt (apply +
               (map sq (sub-point p2 p1)))))

(check-expect (weighted-avg 4 7) 5)
(define (weighted-avg x y)
  (/ (+ x x y) 3))

(check-expect (mid-point (list 2 3) (list 4 5))
              (list 3 4))
(define (mid-point p1 p2)
  (scale-point 1/2 (add-point p1 p2)))

(check-expect (third-point (list 1 2) (list 4 5)) (list 2 3))
(define (third-point p1 p2)
  (map weighted-avg p1 p2))

(define (make-fractal depth side)
  (local [(define (draw-fractal canvas depth p1 p2)
            (let* ([mid1 (third-point p1 p2)]
                   [mid2 (third-point p2 p1)]
                   [middle (mid-point p1 p2)]
                   [side (/ (dist p1 p2) 3)]
                   [angle (+ (/ pi 2)
                             (apply atan (reverse (sub-point p2 p1))))]
                   [point (add-point middle
                                     (scale-point side
                                                  (list (cos angle)
                                                        (sin angle))))]
                   [new-depth (sub1 depth)])
              (if (<= depth 0)
                  (add-line canvas
                            (first p1) (second p1)
                            (first p2) (second p2)
                            "red")                              
                  (draw-fractal (draw-fractal (draw-fractal (draw-fractal canvas 
                                                                          new-depth p1 mid1)
                                                            new-depth mid2 p2)
                                              new-depth mid1 point)
                                new-depth point mid2))))]
    (draw-fractal (rectangle side (/ side 2) "solid" "white")
                  depth
                  (list 0 0) (list side 0))))

(make-fractal 3 80)

