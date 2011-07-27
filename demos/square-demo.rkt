#lang planet tracer/tracer

(require htdp/image)

(define (make-webbed-square steps side helper)
  (helper
   0 0 side side
   (/ side steps)
   side
   (rectangle side side "solid" (make-color 187 0 94))))

(define (my-add-line canv x1 y1 x2 y2 col s)
  (add-line canv 
            (- x1 (/ s 2))
            (- y1 (/ s 2))
            (- x2 (/ s 2))
            (- y2 (/ s 2))
            col))
            

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
                    (my-add-line 
                     (my-add-line 
                      (my-add-line 
                       (my-add-line canvas new-x4 0 0 new-y1 color side)
                       side new-y3 new-x4 0 color side)
                      new-x2 side side new-y3 color side) 
                     0 new-y1 new-x2 side color side)))]))


(define (sq-helper2 y1 x2 y3 x4 inc side canvas)
  (cond
      [(>= 0 y3) canvas]
      [(> y3 0)
       (let* ([new-y1 (+ y1 inc)]
              [new-x2 (+ x2 inc)]
              [new-y3 (- y3 inc)]
              [new-x4 (- x4 inc)]
              [color "white"])
         (my-add-line 
          (my-add-line 
           (my-add-line 
            (my-add-line 
             (sq-helper2 new-y1 new-x2 new-y3 new-x4 inc side canvas)
             new-x4 0 0 new-y1 color side)
            side new-y3 new-x4 0 color side)
           new-x2 side side new-y3 color side) 
          0 new-y1 new-x2 side color side))]))

(make-webbed-square 4 40 sq-helper2)
(make-webbed-square 4 40 sq-helper)
