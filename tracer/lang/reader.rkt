#lang s-exp syntax/module-reader
(planet tracer/tracer/tracer)
#:wrapper2
(lambda (in rd stx?)
  (let* ([offset (+ (file-position in) 1)]
         [port-text (port->string in)]
         [in (open-input-string port-text)])
    (port-count-lines! in)
    (define reloc-in (relocate-input-port in 1 1 offset))
    (port-count-lines! reloc-in)
    (let* ([mod (rd reloc-in)]
           [mod  (if stx? mod (datum->syntax #f mod))]
           [r (with-syntax ([port-text port-text])
                (syntax-case mod ()
                  [(module name lang* (modbegin . body))
                   (syntax/loc mod
                     (module name lang* (modbegin port-text . body)))]))])
      (if stx? r (syntax->datum r)))))

(require racket/port)
