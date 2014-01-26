#lang racket
(provide mybomb%)
(define mybomb%
  (class object%
     ; initialization argument
    (init x y size)               
 
    ; field
    (define position-x x)
    (define position-y y)
    (define mysize size)
    (define step 0)
    ; superclass initialization
    (super-new) 
    
    (define/public (get-step)
      (begin
        (set! step (add1 step))
        (sub1 step)))
    (define/public (get-position)
      (list position-x position-y))
   
    (define/public (is-alive)
      (cond
        [(< step mysize) #true]
        [else #false]))
  ))