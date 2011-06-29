#lang racket
(require racket/pretty)

(pretty-print-columns 40)
           
(define (print-list lst)
  (let* ([ppl (pretty-format lst (pretty-print-columns))]
        [lines (length (regexp-match* "\n" ppl))])
    (if (= lines 1)
        (begin 
          (displayln "lines are 1")
          ppl)
        ;need to split into two lines
        (let*-values ([(l-beg l-end-rev) (split-list lst)])
          (plh l-beg l-end-rev "(list" ")")))))

(define (split-list lst)
  (let ([left (ceiling (/ (length lst) 2))]
        [right (floor (/ (length lst) 2))])
    (values (drop-right lst left)
            (reverse (take-right lst right)))))
                
(define (plh fwd rev s-fwd s-rev)
  (cond
    [(and (empty? fwd) (empty? rev))
     (string-append s-fwd "...\n      ..." s-rev)]
    [(empty? fwd) (plh fwd (rest rev) s-fwd (add-item s-rev rev))]
    [(empty? rev) (plh (rest fwd) rev (add-item s-fwd fwd) s-rev)]
    ;both have elements left
    [(and (cons? fwd) (cons? rev))
     (plh (rest fwd)
          (rest rev)
          (add-item fwd s-fwd true)
          (add-item rev s-rev false))]
    ))

(define (add-item lst s fwd)
  (let ([next-item (pretty-format (first lst) (pretty-print-columns))])
    (if (< (+ (string-length s)
              (string-length next-item)
              (if fwd 0 6))
           (pretty-print-columns))
        (if fwd 
            (string-append s " " next-item)
            (string-append " " next-item s))
        s)))

#|


(define p (open-output-string "hi"))
(let ([orig (port-write-handler p)])
  (port-write-handler 
   p 
   (lambda(val port [depth 0]) (orig "world" port))))
  
(write "cat" p)
;(write

(displayln (get-output-string p))

|#