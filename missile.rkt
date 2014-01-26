#lang racket
(require "mysprite.rkt")
(provide mymissile%)
(define mymissile%
  (class mysprite%
     ; initialization argument
    (init velocity direction x y width height)               
 
    ; field
    (define current-blood 1) 
    ; superclass initialization
    (super-new [velocity velocity][direction direction][x x][y y][width width][height height])                
 
    (inherit get-direction)
    (inherit set-offset-default)
    (inherit detect-outside)
    (inherit detect-collision-barrier)
    
    (define/public (get-blood)
      current-blood)
    (define/public (sub-blood amt)
      (set! current-blood (- current-blood amt)))
    (define/public (move-auto interval)
      (set-offset-default interval)) 
    (define (collision-happen)
      (sub-blood 1))
    (define/public (do-detect width height vect tile-width tile-height interval)
      (begin
        (move-auto interval)
        (cond
          [(detect-outside width height) 
           (begin
             (collision-happen)
             #false)]
          [else
           (let ([result (detect-collision-barrier vect tile-width tile-height (+ (quotient (- width 1) tile-width) 1))])
             (cond
               [result 
                (begin
                  (collision-happen)
                  result)]
               [else #false]))])))
            
  ))