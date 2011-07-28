#lang racket

(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button
         "annotate.rkt")
(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define tracer-bitmap
      (let* ((bmp (make-bitmap 16 16))
             (bdc (make-object bitmap-dc% bmp)))
        (send bdc erase)
        (send bdc set-smoothing 'smoothed)
        (send bdc set-pen "black" 1 'transparent)
        (send bdc set-brush "blue" 'solid)
        (send bdc draw-ellipse 2 2 8 8)
        (send bdc set-brush "red" 'solid)
        (send bdc draw-ellipse 6 6 8 8)
        (send bdc set-bitmap #f)
        bmp))

    (define (phase1) (void))
    
    (define (phase2) (void))
    
    
    (define tracer-frame-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text
                 get-current-tab)
        (inherit register-toolbar-button)
        
        
        (define tracer-button
          (new switchable-button%
               (label "Tracer")
               (bitmap tracer-bitmap)
               (parent (send this get-button-panel))
               (callback (λ (button) (send this tracer-callback)))))
        
        (define (definitions->image-and-char def)
          (letrec ([first-snip (send def find-first-snip)]
                   [process-snip 
                    (lambda (a-snip)
                      (cond 
                        [(is-a? a-snip string-snip%)
                         (reverse (string->list (send a-snip get-text 0 (send a-snip get-count))))]
                        [(equal? #f a-snip) empty]
                        [else (list a-snip)]))]
                   [add-snip (lambda (l cur-snip)
                               (if (equal? cur-snip #f)
                                   l
                                   (add-snip (append (process-snip cur-snip) l)
                                             (send cur-snip next))))])
            (reverse (add-snip empty first-snip))))
        
        (define/public (tracer-callback)
          (let* ([def-text (send this get-definitions-text)]
                 [lang-setting (send def-text get-next-settings)]
                 [text-end (string-length (send def-text get-text))]
                 [text-pos (drracket:language:text/pos def-text 0 text-end)]
                 [init (lambda() (void))]
                 [code (box empty)]
                 [kill-termination (lambda()
                                     (void))]
                 [src (definitions->image-and-char def-text)]
                 [cur-rep-text (send (send this get-current-tab) get-ints)]
                 [iter (lambda (stx cont)
                         (if (eof-object? stx)
                             (let* ([expanded-st (reverse (unbox code))])
                               (map eval expanded-st))
                             (begin (set-box! code 
                                              (cons stx (unbox code)))
                                    (cont))))])
            (send cur-rep-text reset-console)
            (send cur-rep-text
                  run-in-evaluation-thread
                  (lambda()
                    (drracket:eval:expand-program text-pos 
                                                  lang-setting
                                                  #f ;eval-compile-time-part?
                                                  init
                                                  kill-termination
                                                  iter)))
            (send cur-rep-text insert-prompt)))               
        
        (register-toolbar-button tracer-button)
        (send (get-button-panel) change-children
              (λ (l)
                (cons tracer-button (remq tracer-button l))))))
    
  (drracket:get/extend:extend-unit-frame tracer-frame-mixin)))

