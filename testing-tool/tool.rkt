#lang racket/base

(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button
         lang/stepper-language-interface)
(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define tracer-button-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text)
        (inherit register-toolbar-button)
        
        (define tracer-button
          (new switchable-button%
               (label "Definitions")
               (callback 
                (λ (button) 
                  (message-box 
                   "Def Window"
                   (format "~s"
                           (let ([lang-level (extract-language-level 
                                              (get-definitions-text))])
                             (tracer-works-for? lang-level))))))
               (parent (get-button-panel))
               (bitmap reverse-content-bitmap)))
        (register-toolbar-button tracer-button)
        (send (get-button-panel) change-children
              (λ (l)
                (cons tracer-button (remq tracer-button l))))))
    
    (define reverse-content-bitmap
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
    
    (define (extract-language-level definitions-text)
      (settings->language-level (definitions-text->settings definitions-text)))
    
    (define (definitions-text->settings definitions-text)
      (send definitions-text get-next-settings))
    
    (define (settings->language-level settings)
      (drracket:language-configuration:language-settings-language settings))
    
    (define (tracer-works-for? language-level)
      (or (send language-level stepper:supported?)
          (getenv "PLTTRACERRUNSAFE")))
    
    (drracket:get/extend:extend-unit-frame tracer-button-mixin)))
