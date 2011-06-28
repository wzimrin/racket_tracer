#lang s-exp syntax/module-reader
-ignored-
#:wrapper2
(lambda (in rd stx?)
  (let* ([offset (+ (file-position in) 1)]
         [port-text (port->string in)]
         [in (open-input-string port-text)])
    (port-count-lines! in)
    (let* ([mod (rd (relocate-input-port in 2 1 offset))]
           [mod  (if stx? mod (datum->syntax #f mod))]
           [r (with-syntax ([port-text port-text])
                (syntax-case mod ()
                  [(module name lang* (wrapper body ...))
                   (syntax/loc mod
                     (module name (planet tracer/tracer/tracer)
                       (wrapper
                        port-text
                        body ...)))]))])
      (if stx? r (syntax->datum r)))))

(require racket/port)
