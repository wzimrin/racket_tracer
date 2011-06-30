#lang s-exp syntax/module-reader
(planet tracer/tracer/tracer)
#:wrapper2
(lambda (in rd stx?)
  (let* ([offset (+ (file-position in) 1)]
         [port-text (regexp-replace "λ" (port->string in) "lambda")]
         [in (open-input-string port-text)])
    (port-count-lines! in)
    (define reloc-in (relocate-input-port in 1 1 offset))
    (port-count-lines! reloc-in)
    (let* ([mod (rd reloc-in)]
           [mod  (if stx? mod (datum->syntax #f mod))]
           [src (syntax-source mod)]
           [filename (if (path-string? src)
                         (let-values ([(path filename c) (split-path src)])
                           (path->string filename))
                         (format "~s" src))]
           [r (with-syntax ([port-text port-text]
                            [offset offset]
                            [filename filename])
                (syntax-case mod ()
                  [(module name lang* (modbegin . body))
                   (syntax/loc mod
                     ;added name after modbeg
                     (module name lang* (modbegin filename
                                                  port-text 
                                                  offset . body)))]))])
      (if stx? r (syntax->datum r)))))

(require racket/port)
