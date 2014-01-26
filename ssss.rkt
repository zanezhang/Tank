#lang racket/gui

(require 2htdp/image)
(define mymissilemap-right (read-bitmap "gameover.png"))
;(scale 2 (ellipse 20 30 "solid" "blue"))
(define mymissilemap-down (scale/xy  (/ 700.0 640) (/ 500 480.0) mymissilemap-right))
(save-image mymissilemap-down "mygameover.png")