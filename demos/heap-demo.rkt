#lang planet tracer/tracer:1:2

(define-struct bhnode (value left right))

(define (insert x h)
  (cond
    [(empty? h) (make-bhnode x empty empty)]
    [(bhnode? h)
     (let [(y (bhnode-value h))
           (l (bhnode-left h))
           (r (bhnode-right h))]
       ;always insert into the right (smaller) subtree
       (if (< x y)
         ;once done, make sure that is the left (larger) subtree
         (make-bhnode x (insert y r) l)
         ;we are inserting into an empty heap
         (make-bhnode y (insert x r) l)))]))

(define (make-heap ns)
  (foldl insert empty ns))
 
(define (remove-min h)
  (local [;makes a new tree from x and the l and r subtrees
          (define (insert-merge x l r)
            ;easy, since no structure changes - simply move values around
            (cond [(andmap 
                    (lambda (y)
                      ;if x is less than the bhnode-value of all existing subtrees
                      (or (not (bhnode? y))
                          (< x (bhnode-value y))))
                    (list l r))
                   ;we can simply put x here - it is a valid tree
                   (make-bhnode x l r)]
                  [;if r is empty or if l is less than r
                   (or (not (bhnode? r))
                       (< (bhnode-value l) (bhnode-value r)))
                   ;insert it into l and use l as the new min of the tree
                   (make-bhnode (bhnode-value l)
                                (insert-merge x (bhnode-left l) (bhnode-right l));we don't insert it into 
                                r)];r if r is empty because we don't want to change the structure
                  [(or (not (bhnode? l));proceed as above, with l and r flipped
                       (< (bhnode-value r) (bhnode-value l)))
                   (make-bhnode (bhnode-value r)
                                l
                                (insert-merge x (bhnode-left r) (bhnode-right r)))]))
          (define (left-most h);gets the left-most value (the only one we can easily remove)
            (let ([l (bhnode-left h)]);go left until l is empty, then return the value
              (cond [(bhnode? l) (left-most l)]
                    [(empty? l) (bhnode-value h)])))
          (define (remove-left-most h);removes the left-most value
            ;we can, since the left subtree is always >= the right tree
            ;we just move the left subtree to the right afterwards
            (let ([l (bhnode-left h)])
              (cond [(bhnode? l);go left until l is empty, than rebuild the tree without the node above the empty l
                     (make-bhnode (bhnode-value h); we can do this, because if l is empty, r has to be empty for it to
                                  (bhnode-right h);be a BrownHeap
                                  (remove-left-most l))]
                    [(empty? l) empty])))
          (define extra (left-most h));the new value that we will put into the root and then fix
          (define new-h (remove-left-most h))];the heap without extra
    (cond [(empty? new-h) empty];we removed the last value
          [(bhnode? new-h)
           (insert-merge extra;new-h is a valid BrownHeap, so this results in a valid BrownHeap
                  (bhnode-left new-h)
                  (bhnode-right new-h))])))

(remove-min (make-heap (list 8 4 3 9 1 6 12 14)))