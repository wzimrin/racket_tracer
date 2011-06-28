#lang s-exp syntax/module-reader
-ignored-
#:wrapper2
(lambda (in rd stx?)
  (let* ([port-text (port->string (relocate-input-port in
                                                       1
                                                       0
                                                       0))]
         [in (open-input-string port-text)])
    (port-count-lines! in)
    (let* ([mod  (rd in)]
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
