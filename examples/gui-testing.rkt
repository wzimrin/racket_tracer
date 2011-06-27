#lang planet tracer/tracer

#|
Included
- Many varied arguments at top level and on page
     many-arguments foo-arg
- Long function names at top level and on page
     a-very-long-function-name foo-top-level
- Large arguments (tree)
     make-heap
- Multiple large arguments
     many-large-args

To-Add
- Vectors (?)
|#

;----- FUNCTION DEFINITIONS -----
;Many varied arguments
(define (many-arguments a-num 
                        a-string 
                        a-list
                        a-bool
                        a-func)
  (cond
    [(= 0 a-num)
     "goodbye world"]
    [(< 0 a-num)
     (many-arguments (sub1 a-num)
                     (string-append "hip" a-string)
                     a-list
                     a-bool
                     a-func)]))

;Long function name
(define (a-very-long-function-name a)
  (make-list a a))

;Many arguments for call not at top level
(define (foo-arg a)
  (many-arguments a
                  "hooray"
                  (make-list 20 2)
                  false
                  (λ(y) (* y y))))

;Long function name for call not at top level
(define (foo-top-level list-length)
  (map (λ(x) (a-very-long-function-name x)) 
     (build-list list-length (λ(n) (* n 2)))))

;Large data structure
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

;Many large arguments
(define (many-large-args a b c d)
  d)

;----- TO TRACE -----

(many-arguments 4 
                "hooray" 
                (make-list 20 2)
                true
                (λ(a b c d) (+ a (* c d) (* b b))))

(foo-arg 5)

(foo-top-level 7)

(make-heap (list 3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4
                 6 2 6 4 3 3 8 3 2 7 9 5 0 2 8 8 4 1 9 7 
                 1 6 9 3 9 9 3 7 5 1 0 5 8 2 0 9 7 4 9 4))

(make-heap (list 3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4))

(many-large-args (make-heap (make-list 10 1))
                 (make-heap (make-list 10 2))
                 (make-heap (make-list 10 3))
                 (make-heap (make-list 10 4)))
                                                         
;Long function names at top level
(map (λ(x) (a-very-long-function-name x)) 
     (build-list 10 (λ(n) (* n 2))))




