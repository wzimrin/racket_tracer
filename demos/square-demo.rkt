#lang planet tracer/tracer:1:2

(require htdp/image)

(define (make-webbed-square steps side helper)
  (helper
   0 0 side side
   (/ side steps)
   side
   (rectangle side side "solid" (make-color 187 0 94))))

(define (sq-helper y1 x2 y3 x4 inc side canvas)
    (cond
      [(>= 0 y3) canvas]
      [(> y3 0)
       (let* ([new-y1 (+ y1 inc)]
              [new-x2 (+ x2 inc)]
              [new-y3 (- y3 inc)]
              [new-x4 (- x4 inc)]
              [color "white"])
         (sq-helper new-y1 new-x2 new-y3 new-x4 inc side
                    (add-line 
                     (add-line 
                      (add-line 
                       (add-line canvas new-x4 0 0 new-y1 color)
                       side new-y3 new-x4 0 color)
                      new-x2 side side new-y3 color) 
                     0 new-y1 new-x2 side color)))]))


(define (sq-helper2 y1 x2 y3 x4 inc side canvas)
  (cond
      [(>= 0 y3) canvas]
      [(> y3 0)
       (let* ([new-y1 (+ y1 inc)]
              [new-x2 (+ x2 inc)]
              [new-y3 (- y3 inc)]
              [new-x4 (- x4 inc)]
              [color "white"])
         (add-line 
          (add-line 
           (add-line 
            (add-line 
             (sq-helper2 new-y1 new-x2 new-y3 new-x4 inc side canvas)
             new-x4 0 0 new-y1 color)
            side new-y3 new-x4 0 color)
           new-x2 side side new-y3 color) 
          0 new-y1 new-x2 side color))]))

(make-webbed-square 15 60 sq-helper)
(make-webbed-square 15 60 sq-helper2)
