#lang planet tracer/tracer

(require 2htdp/image)

(define (circ r)
  (circle r "outline" "red"))

(circ 35)