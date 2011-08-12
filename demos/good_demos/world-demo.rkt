#lang planet tracer/tracer

(trace-all)

(define color-list (list "red" "green" "blue"))
(define color-list-length (length color-list))

(define (get-color w)
  (list-ref color-list 
            (modulo (world-color w)
                    color-list-length)))

(define-struct world (size color))

(define (add-color w x)
  (make-world (world-size w)
              (modulo (+ x (world-color w)) 
                      color-list-length)))

(define (add-size w x)
  (make-world (+ x (world-size w))
              (world-color w)))

(define (key w k)
  (cond
    [(equal? k "down")
     (add-color w -1)]
    [(equal? k "up")
     (add-color w 1)]))

(define (draw w)
  (circle (world-size w)
          "solid"
          (get-color w)))

(big-bang
 (make-world 10 0)
 [on-tick (lambda (w)
            (add-size w 10))
          1]
 [on-key key]
 [stop-when (lambda (w)
              (>= (world-size w) 50))]
 [to-draw draw 100 100])