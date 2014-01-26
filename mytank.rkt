#lang racket
(require "mysprite.rkt")
(provide mytank%)
(define mytank%
  (class mysprite%
     ; initialization argument
    (init blood velocity direction x y width height type)               
 
    ; field
    (define current-blood blood) 
    (define mytype type)
    (define fire-request 0)
    (define canfire (+ 1000 (* type 400)))
    (define freezetime 5000)
    (define tempfreezetime freezetime)
    (define tempcanfire canfire)
    (define direction-key-pressed 0)
 
    ; superclass initialization
    (super-new [velocity velocity][direction direction][x x][y y][width width][height height])                
 
    (inherit get-direction)
    (inherit set-direction)
    (inherit set-offset)
    (inherit set-offset-default)
    (inherit detect-outside)
    (inherit detect-collision-barrier)
    
    (define/public (get-type)
      mytype)
    (define/public (get-blood)
      current-blood)
    (define/public (sub-blood amt)
      (cond
        [(or (= mytype 1) (<= tempfreezetime 0))
         (set! current-blood (- current-blood amt))]))
    (define/public (set-command type cmd)
      (cond
        [(symbol=? type 'press)
         (cond
             [(char? cmd) 
              (let ([tempcmd (char-downcase cmd)])
                (case tempcmd
                  [(#\w) (change-direction 0)]
                  [(#\a) (change-direction 1)]
                  [(#\d) (change-direction 2)]
                  [(#\s) (change-direction 3)]
                  [(#\j) (fire)]
                  [(#\space) (fire)]))]
            [(symbol? cmd)
             (cond 
               [(symbol=? cmd 'up)(change-direction 0)]
               [(symbol=? cmd 'left)(change-direction 1)]
               [(symbol=? cmd 'right)(change-direction 2)]
               [(symbol=? cmd 'down)(change-direction 3)])])]
        [(symbol=? type 'release)
         (cond
           [(char? cmd)
            (let ([tempcmd (char-downcase cmd)])
              (case tempcmd
                [(#\w) (direction-key-release 0)]
                [(#\a) (direction-key-release 1)]
                [(#\d) (direction-key-release 2)]
                [(#\s) (direction-key-release 3)]))]
           [(symbol? cmd)
            (cond 
              [(symbol=? cmd 'up)(direction-key-release 0)]
              [(symbol=? cmd 'left)(direction-key-release 1)]
              [(symbol=? cmd 'right)(direction-key-release 2)]
              [(symbol=? cmd 'down)(direction-key-release 3)])])]))
    (define (change-direction amt)
      (begin 
        (set! direction-key-pressed 1)
        (cond 
          [(equal? (get-direction) amt)
            ;move
           (void)]
            ;change direction
          [else
           (begin
             (set-direction amt)
             (set-offset 0 0))])))
    (define (direction-key-release amt)
      (cond
        [
         (= amt (get-direction))
         (set! direction-key-pressed 0)]))
    (define/public (fire)
      (set! fire-request 1))
    (define/public (do-fire)
      (cond
        [(> tempcanfire 0) #false]
        [(or (= mytype 1)(= fire-request 1))
         (begin
           (set! tempcanfire canfire)
           (set! fire-request 0)
           #true)]
        [else #false]))
     (define/public (get-state)
       (cond
         [(<= current-blood 0) 0]
         [(= mytype 1)1]
         [(> tempfreezetime 0) 2]
         [else 1]))
    (define/public (move-auto interval)
      (set-offset-default interval)) 
    (define (collision-happen)
      (cond
        [(equal? mytype 1) (set-direction  (random 4))]))
    (define/public (do-detect width height vect tile-width tile-height interval)
      (begin
        (cond
          [(or (= mytype 1)(= direction-key-pressed 1)) (move-auto interval)]) 
        (cond
          [(detect-outside width height) (collision-happen)]
          [(detect-collision-barrier vect tile-width tile-height (+ (quotient (- width 1) tile-width) 1)) (collision-happen)])
        (cond 
          [(> tempcanfire 0) (set! tempcanfire (- tempcanfire interval))])
        (cond
          [(> tempfreezetime 0) (set! tempfreezetime (- tempfreezetime interval))])
        ))
  ))