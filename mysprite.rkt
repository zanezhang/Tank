#lang racket
(provide mysprite%)
(define mysprite%
  (class object%
    (init velocity direction x y width height)                ; initialization argument
 
    ; field
    (define current-velocity (* 1.0 velocity))
    (define current-direction direction)
    (define position-x x)
    (define position-y y)
    (define offset-x 0)
    (define offset-y 0)
    (define mywidth width)
    (define myheight height)
    
     ; superclass initialization
    (super-new)               
 
    (define/public (get-velocity)
      current-velocity)
    (define/public (set-velocity amt)
      (set! current-velocity amt))
    (define/public (get-direction)
      current-direction)
    (define/public (set-direction amt)
      (set! current-direction amt))
    (define/public (get-position)
      (list position-x position-y))
    (define/public (move-purely x y)
      (begin
        (set! position-x (+ x position-x))
        (set! position-y (+ y position-y))))
    (define/public (get-offset)
      (list offset-x offset-y))
    (define/public (set-offset x y)
      (begin 
        (set! offset-x x)
        (set! offset-y y)))
    ;移动一定的距离，默认移动offset的距离，但是因为碰撞或者越界，移动的距离由x'y决定
    (define/public (move [x 'null] [y 'null])
      (begin
        (if (or (symbol? x) (symbol? y))
            (move-purely offset-x offset-y)
            (move-purely x y))
        (set-offset 0 0)
        ))
    ;这里算出默认偏移量，由速度和方向决定
    (define/public (set-offset-default interval)
      (let ([offset (/ (* interval current-velocity) 1000)])
        (case  current-direction
          [(0) ;up
           (begin
             (set! offset-x 0)
             (set! offset-y (- 0 offset)))]
          [(1) ;left
           (begin
             (set! offset-x (- 0 offset))
             (set! offset-y 0))]
          [(2) ;right
           (begin
             (set! offset-x offset)
             (set! offset-y 0))]
          [(3) ;down
           (begin
             (set! offset-x 0)
             (set! offset-y offset))]
          [else 
           (begin
             (set! offset-x 0)
             (set! offset-y 0))])))
    (define/public (get-missile-position width height)
      (case  current-direction
        [(0) ;up
         (list (+ position-x (- (/ mywidth 2) (/ height 2))) (- position-y width))]
        [(1) ;left
         (list (- position-x width) (+ position-y (- (/ myheight 2) (/ height 2))))]
        [(2) ;right
           (list (+ position-x mywidth) (+ position-y (- (/ myheight 2) (/ height 2))))]
        [(3) ;down
         (list (+ position-x (- (/ mywidth 2) (/ height 2))) (+ position-y myheight))]))
    (define (detect-outside-iner x-min y-min x-max y-max)
      (cond
        [(> (+ position-x offset-x) x-max)
         (begin
           (set! offset-x (- x-max position-x))
           #true)]
        [(< (+ position-x offset-x) x-min)
         (begin
           (set! offset-x (- x-min position-x))
           #true)]
        [(> (+ position-y offset-y ) y-max)
         (begin
           (set! offset-y (- y-max position-y))
           #true)]
        [(< (+ position-y offset-y) y-min)
         (begin
           (set! offset-y (- y-min position-y))
           #true)]
        [else #false]))
      
    (define/public (detect-outside  amt-width amt-height [type 0])
     (cond 
       [(= type 0)
        (detect-outside-iner 0 0 (- amt-width mywidth) (- amt-height myheight))]
       [(= type 1)
        (detect-outside-iner (- 0 mywidth -1) (- 0 myheight -1) (- amt-width 1) (- amt-height 1))]))
    (define (detect-collision-barrier-iner x y vect tile-width tile-height tile-step)
      (let ([ref-num (inexact->exact (+ (floor (/ x tile-width)) (* (floor (/ y tile-height)) tile-step)))])
        (cond
          [(> (vector-ref vect ref-num) 0) ref-num]
          [else #false])))
      
    (define/public (detect-collision-barrier vect tile-width tile-height tile-step)
      (case current-direction
        [(0) ;up
         (let ([ref-num1 (detect-collision-barrier-iner (+ position-x offset-x) (+ position-y offset-y) vect tile-width tile-height tile-step)]
               [ref-num2 (detect-collision-barrier-iner (+ position-x offset-x mywidth -1) (+ position-y offset-y) vect tile-width tile-height tile-step)])
         (cond
           [(or ref-num2 ref-num1)
            (begin
              (set! offset-y (- offset-y (- (remainder (inexact->exact(floor(+ offset-y position-y))) tile-height) tile-height)))
              (list (+ position-x offset-x (/ mywidth 2)) (+ position-y offset-y) ref-num1 ref-num2))]  
           [else #false]))]
        [(1) ;left
         (let ([ref-num1 (detect-collision-barrier-iner (+ position-x offset-x) (+ position-y offset-y) vect tile-width tile-height tile-step)]
               [ref-num2 (detect-collision-barrier-iner (+ position-x offset-x) (+ position-y offset-y myheight -1) vect tile-width tile-height tile-step)])
         (cond
           [(or ref-num2 ref-num1)
            (begin
              (set! offset-x (- offset-x (- (remainder (inexact->exact(floor(+ offset-x position-x))) tile-width) tile-width)))
              (list (+ position-x offset-x) (+ position-y offset-y (/ myheight 2)) ref-num1 ref-num2))]  
           [else #false]))]
        [(2) ;right
         (let ([ref-num1 (detect-collision-barrier-iner (+ position-x offset-x mywidth -1) (+ position-y offset-y) vect tile-width tile-height tile-step)]
               [ref-num2 (detect-collision-barrier-iner (+ position-x offset-x mywidth -1) (+ position-y offset-y myheight -1) vect tile-width tile-height tile-step)])
         (cond
           [(or ref-num2 ref-num1)
            (begin 
              (set! offset-x (- offset-x (remainder (inexact->exact(ceiling(+ offset-x position-x mywidth))) tile-width)))
              (list (+ position-x offset-x mywidth) (+ position-y offset-y (/ myheight 2)) ref-num1 ref-num2))]  
           [else #false]))]
        [(3) ;down
         (let ([ref-num1 (detect-collision-barrier-iner (+ position-x offset-x) (+ position-y offset-y myheight -1) vect tile-width tile-height tile-step)]
               [ref-num2 (detect-collision-barrier-iner (+ position-x offset-x mywidth -1) (+ position-y offset-y myheight -1) vect tile-width tile-height tile-step)])
         (cond
           [(or ref-num2 ref-num1)
            (begin
              (set! offset-y (- offset-y (remainder (inexact->exact(ceiling(+ offset-y position-y myheight))) tile-height)))
             (list (+ position-x offset-x (/ mywidth 2)) (+ position-y offset-y myheight) ref-num1 ref-num2))]  
           [else #false]))]))
    
    ;;;;;;;
    (define rect-intersection
      (lambda (up1 left1 right1 down1 up2 left2 right2 down2)
        (cond
          [(or (< down1 up2) (< down2 up1) (< right2 left1) (< right1 left2))
           #false]
          [else #true])))
    (define rect-intersection-center
      (lambda (up1 left1 right1 down1 up2 left2 right2 down2)
        (list (/ (+ (cond
                             [(> left1 left2) left1]
                             [else left2])
                           (cond 
                             [(> right1 right2) right2]
                             [else right1])) 2)
              (/ (+ (cond
                             [(> up1 up2) up1]
                             [else up2])
                           (cond 
                             [(> down1 down2) down2]
                             [else down1])) 2))))
    (define rect-center
      (lambda (up left right down)
        (list (/ (+ left right) 2)
              (/ (+ up down) 2))))
      
    (define/public collision-objct
      (lambda (b)
        (let ([right (+ position-x mywidth)]
              [down (+ position-y myheight)])
          (send b collision-rect position-y position-x right down))))
    (define/public collision-rect
      (lambda (up1 left1 right1 down1)
        (let ([right (+ position-x mywidth)]
              [down (+ position-y myheight)])
          (cond
            [(rect-intersection up1 left1 right1 down1 position-y position-x right down)
             ;(rect-intersection-center up1 left1 right1 down1 position-y position-x right down)
             (rect-center up1 left1 right1 down1)]
            [else #false]))))
             
    ))