#lang s-exp syntax/module-reader
(planet tracer/tracer/tracer)
#:wrapper2
(lambda (in rd stx?)
  (let* ([offset (+ (file-position in) 1)]
         [port-text (regexp-replace* "λ" (port->string in) "lambda")]
         [in (open-input-string port-text)])
    (port-count-lines! in)
    (define reloc-in (relocate-input-port in 1 1 offset))
    (port-count-lines! reloc-in)
    (let*-values ([(mod) (rd reloc-in)]
                  [(mod)  (if stx? mod (datum->syntax #f mod))]
                  [(path fileName c) (split-path (syntax-source mod))]
                  [(r) (with-syntax ([port-text port-text]
                                     [offset offset]
                                     [fileName fileName])
                         (syntax-case mod ()
                           [(module name lang* (modbegin . body))
                            (syntax/loc mod
                                ;added name after modbeg
                                (module name lang* (modbegin (path->string fileName)
                                                             port-text 
                                                             offset . body)))]))])
      (if stx? r (syntax->datum r)))))

(require racket/port)
