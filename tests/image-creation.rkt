#lang planet tracer/tracer

(require 2htdp/image)

(define (circ r)
  (circle r "outline" "red"))

(define (id x) x)

(id (circ 35))