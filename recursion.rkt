#lang planet tracer/tracer

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
  (foldl (lambda (x h) (insert x h)) empty ns))
 
(define get-min bhnode-value)

(define (remove-min h)
  
  (local [;makes a new tree from x and the l and r subtrees
          (define (insert-merge x l r)
            ;easy, since no structure changes - simply move values around
            (cond [(andmap 
                    (lambda (y)
                      ;if x is less than the get-min of all existing subtrees
                      (or (not (bhnode? y))
                          (< x (get-min y))))
                    (list l r))
                   ;we can simply put x here - it is a valid tree
                   (make-bhnode x l r)]
                  [;if r is empty or if l is less than r
                   (or (not (bhnode? r))
                       (< (get-min l) (get-min r)))
                   ;insert it into l and use l as the new min of the tree
                   (make-bhnode (get-min l)
                                (insert-merge x (bhnode-left l) (bhnode-right l));we don't insert it into 
                                r)];r if r is empty because we don't want to change the structure
                  [(or (not (bhnode? l));proceed as above, with l and r flipped
                       (< (get-min r) (get-min l)))
                   (make-bhnode (get-min r)
           
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

(define (food a b c)
  (remove-min a))

(define (munchies a b c)
  (if (= a 0)
      c
      (munchies 0 b c)))

(munchies 1 2 3)

(food (make-heap (list 3 1 4 1 5 9 2 6 5))
      (make-heap (make-list 5 5))
      (make-heap (build-list 10 (lambda(x) (* x x)))))

(define heap (make-heap (list 8 4 3 9 1 6 12 14)))
(remove-min heap)
(get-min heap)