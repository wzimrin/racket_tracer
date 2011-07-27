#lang racket

(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button
         "annotate.rkt")
         ;lang/stepper-language-interface)
(provide tool@)

;(define-local-member-name tracer-callback)

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
    
    #;(define (tracer-frame-mixin super%)
      (class super%
        ...))
    (define (phase1) (void))
    
    (define (phase2) (void))
    
    (define (extract-language-level definitions-text)
      (let* ([settings (send definitions-text get-next-settings)]
             [lang-level (drracket:language-configuration:language-settings-language settings)])
        lang-level))
        
    (define (extract-language-settings definitions-text)
      (let* ([settings (send definitions-text get-next-settings)]
             [lang-settings (drracket:language-configuration:language-settings-settings settings)])
        lang-settings))

    
    (define tracer-frame-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text)
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
                         (reverse (string->list (send a-snip get-text 0 99999999999999999999)))]
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
                 [text-end (send def-text get-end-position)]
                 [text-pos (drracket:language:text/pos def-text 0 text-end)]
                 [init (lambda() (void))]
                 [code (box empty)]
                 [kill-termination (lambda()
                                     (void))]
                 [iter (lambda (stx cont)
                         (if (eof-object? stx)
                             #;(annotate-and-eval (reverse (unbox code)) src)
                             (displayln (map syntax->datum (reverse (unbox code))))
                             (begin (displayln stx)
                                    (set-box! code 
                                              (cons stx (unbox code)))
                                    (cont))))]
                 [src (definitions->image-and-char def-text)])
            
            (drracket:eval:expand-program text-pos 
                                          lang-setting
                                          #f ;eval-compile-time-part?
                                          init
                                          kill-termination
                                          iter)))               
        
        (register-toolbar-button tracer-button)
        (send (get-button-panel) change-children
              (λ (l)
                (cons tracer-button (remq tracer-button l))))))
    
    
    
    
     
    
    (define (tracer-works-for? language-level)
      #t
      #;(or (send language-level stepper:supported?)
            (getenv "PLTTRACERRUNSAFE")))
  
  (drracket:get/extend:extend-unit-frame tracer-frame-mixin)))
