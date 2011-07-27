#lang planet tracer/tracer

(define-struct heap-node (value left right))

(define (insert x h)
  (cond
    [(empty? h) (make-heap-node x empty empty)]
    [(heap-node? h)
     (let [(y (heap-node-value h))
           (l (heap-node-left h))
           (r (heap-node-right h))]
       ;always insert into the right (smaller) subtree
       (if (< x y)
         ;once done, make sure that is the left (larger) subtree
         (make-heap-node x (insert y r) l)
         ;we are inserting into an empty heap
         (make-heap-node y (insert x r) l)))]))

(define (make-heap ns)
  (foldl insert empty ns))

(make-heap (list 8 4 3 9 1 6 12 14))