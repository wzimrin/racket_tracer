#lang racket

(require "annotater.js")
(require [except-in racket/base #%module-begin])
(require [prefix-in core:
                    (only-in racket/base #%module-begin)])
(provide [rename-out (module-begin #%module-begin)])
(provide [all-from-out racket/base])

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ name source offset body ...)
     #`(#%plain-module-begin
        (displayln (parameterize ([current-namespace (make-base-namespace)])
                     (expand #'(module anonymous-module racket
                                 (core:#%module-begin body ...
                                                      #;(define (fib x)
                                                          (if (< x 2)
                                                              x
                                                              (+ (fib (- x 1))
                                                                 (fib (- x 2)))))))))))]))
