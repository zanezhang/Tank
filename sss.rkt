#lang racket/gui

(require 2htdp/image)
(define mymissilemap-right (read-bitmap "missile.png"))
(define mymissilemap-left (rotate 180 mymissilemap-right))
(define mymissilemap-up (rotate 90 mymissilemap-right))
(define mymissilemap-down (rotate 180 mymissilemap-up))

(save-image mymissilemap-right "missile-right.png")
(save-image mymissilemap-left "missile-left.png")
(save-image mymissilemap-up "missile-up.png")
(save-image mymissilemap-down "missile-down.png")