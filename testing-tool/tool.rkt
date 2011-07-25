#lang racket/base

(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button)
(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define reverse-button-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text)
        (inherit register-toolbar-button)
        
        (let ([btn
               (new switchable-button%
                    (label "Definitions")
                    (callback (λ (button) 
                                (message-box "Def Window"
                                             (send 
                                              (extract-language-level 
                                               (get-definitions-text))
                                              get-one-line-summary))))
                    (parent (get-button-panel))
                    (bitmap reverse-content-bitmap))])
          (register-toolbar-button btn)
          (send (get-button-panel) change-children
                (λ (l)
                  (cons btn (remq btn l)))))))
    
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
    
    (drracket:get/extend:extend-unit-frame reverse-button-mixin)))
