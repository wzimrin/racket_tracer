#lang s-exp syntax/module-reader
#:language (lambda ()
            `(planet ,(this-package-version-symbol tracer)))
#:wrapper2
(lambda (in rd stx?)
  (let*-values ([(offset) (+ (file-position in) 1)]
                [(in in-copy) (clone-port in)]
                [(port-text) 
                 (datum->syntax #f (cons 'list (port->list read-char-or-special in-copy)) #f)])
    (let ([reloc-in (relocate-input-port in 1 1 offset)])
      (port-count-lines! reloc-in)
      (let* ([mod (rd reloc-in)]
             [mod (if stx? mod (datum->syntax #f mod))]
             [src (syntax-source mod)]
             [filename (if (path-string? src)
                           (let-values ([(path filename c) (split-path src)])
                             (path->string filename))
                           (format "~s" src))]
             [r (with-syntax ([offset offset]
                              [filename filename])
                  (syntax-case mod ()
                    [(module name lang* (modbegin . body))
                     (quasisyntax/loc mod
                       ;added name after modbeg
                       (module name lang* (modbegin filename
                                                    #,port-text
                                                    offset . body)))]))])
        (if stx? r (syntax->datum r))))))

(require racket/port)
(require planet/version)


(define (clone-port in)
  (define-values (in1 out1) (make-pipe-with-specials))
  (define-values (in2 out2) (make-pipe-with-specials))
  (port-count-lines! in1)
  (port-count-lines! in2)
  (copy-port in out1 out2)
  (close-output-port out1)
  (close-output-port out2)
  (values in1 in2))

