#lang planet tracer/tracer

(trace-all)

(define (f x)
  x)
(provide f)
(f 4)