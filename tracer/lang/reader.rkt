#lang s-exp syntax/module-reader
(planet tracer/tracer/tracer)
#:wrapper2
(lambda (in rd stx?)
  (let*-values ([(offset) (+ (file-position in) 1)]
                [(in in-copy) (clone-port in)]
                [(port-text) 
                 (port->list read-char-or-special in-copy)
                 #;(port->flattened-string in-copy)])
    (port-count-lines! in)
    (let ([reloc-in (relocate-input-port in 1 1 offset)])
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
                                                    'port-text
                                                    offset . body)))]))])
        (if stx? r (syntax->datum r))))))

(require racket/port)
(require racket/class)
(require racket/gui/base)


(define (clone-port in)
  (define-values (in1 out1) (make-pipe-with-specials))
  (define-values (in2 out2) (make-pipe-with-specials))
  (copy-port in out1 out2)
  (close-output-port out1)
  (close-output-port out2)
  (values in1 in2))

#|(define (port->list p)
  (let ([next (read-char-or-special p)])
    (if (eof-object? p)
        empty
        (cons next (port->list p)))))|#

(define (port->flattened-string p)
  (apply string-append
         (map (lambda (x)
                (cond
                  [(not (char? x)) "."]
                  [(equal? x #\λ) "lambda"]
                  [#t (list->string (list x))]))
              (port->list read-char-or-special p))))