#lang racket/gui

(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button)
(provide tool@)
 
;(define secret-key "easter egg")
;(define to-insert "easter ")
 
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
                    [label "A button"]
                    [callback (λ (b) (message-box "Title" "you clicked on me"))]
                    [bitmap (make-bitmap 50 50)]
                    [parent (get-button-panel)])])
          (register-toolbar-button btn)
          (send (get-button-panel) change-children
                (λ (l)
                  (cons btn (remq btn l)))))))
 
    (define add-close-mixin
      (mixin (drracket:unit:tab<%>) ()
        (super-new)
        ;(inherit get-current-tab)
        #|(new switchable-button% [label "X"]
             [callback (λ(b) (message-box "close!" "you clicked on X"))]
             [bitmap (make-bitmap 50 50)]
             [parent this]))|#
        ))
                 
       
 
    (define (phase1) (void))
    (define (phase2) (void))
 
    ;(drracket:get/extend:extend-definitions-text easter-easter egg-mixin)
    (drracket:get/extend:extend-unit-frame reverse-button-mixin)
    (drracket:get/extend:extend-tab add-close-mixin)))

#;(define (reverse-content text)
      (for ((x (in-range 1 (send text last-position))))
        (send text split-snip x))
      (define snips
        (let loop ((snip (send text find-first-snip)))
          (if snip
              (cons snip (loop (send snip next)))
              '())))
      (define released-snips
        (for/list ((snip (in-list snips))
                   #:when (send snip release-from-owner))
          snip))
      (for ((x (in-list released-snips)))
        (send text insert x 0 0)))

#;(define reverse-content-bitmap
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


#;(define easter-easter eg
      (mixin ((class->interface text%)) ()
 
        (inherit begin-edit-sequence
                 end-edit-sequence
                 insert
                 get-text)
 
        (define/augment (on-insert start len)
          (begin-edit-sequence))
        (define/augment (after-insert start len)
          (check-range (max 0 (- start (string-length secret-key)))
                       (+ start len))
          (end-edit-sequence))
 
        (define/augment (on-delete start len)
          (begin-edit-sequence))
        (define/augment (after-delete start len)
          (check-range (max 0 (- start (string-length secret-key)))
                       start)
          (end-edit-sequence))
 
        (define/private (check-range start stop)
          (let/ec k
            (for ((x (in-range start stop)))
              (define after-x
                (get-text x (+ x (string-length secret-key))))
              (when (string=? after-x secret-key)
                (define before-x
                  (get-text (max 0 (- x (string-length to-insert))) x))
                (unless (string=? before-x to-insert)
                  (insert to-insert x x)
                  (k (void)))))))
 
        (super-new))) 

;g-mixin

