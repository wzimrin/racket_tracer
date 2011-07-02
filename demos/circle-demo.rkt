#lang planet tracer/tracer:1:1

(require htdp/image)

(define (circles ctr-x ctr-y rad depth canvas)
  (cond
    [(= depth 0) canvas]
    [(> depth 0)
     (let* ([cir (circle rad "outline" "red")]
           [new-rad (/ rad 3)]
           [new-depth (sub1 depth)]
           [new-can (circles ctr-x (- ctr-y (* 2 rad)) new-rad new-depth
                             (circles ctr-x (+ ctr-y (* 2 rad)) new-rad new-depth
                                      (circles (+ ctr-x (* 2 rad)) ctr-y new-rad new-depth
                                               (circles (- ctr-x (* 2 rad)) ctr-y new-rad 
                                                        new-depth canvas))))])
       (place-image cir ctr-x (- ctr-y rad)
                    (place-image cir ctr-x (+ ctr-y rad)
                                 (place-image cir (- ctr-x rad) ctr-y
                                              (place-image cir
                                                           (+ ctr-x rad) ctr-y
                                                           new-can)))))]))
;(circles 150 150 (/ 75 2) 4 (empty-scene 300 150))
;(circles 125 125 (/ 75 2) 3 (empty-scene 250 250))
;(circles 150 150 (/ 75 2) 3 (empty-scene 300 150))
(circles 50 50 (/ 50 3) 3 (empty-scene 100 100))
