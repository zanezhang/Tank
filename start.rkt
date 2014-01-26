#lang racket/gui
(require "mytank.rkt")
(require "xml-map.rkt")
(require "missile.rkt")
(require "bomb.rkt")
(define tank-width 44)
(define tank-height 44)
(define missile-width 39)
(define missile-height 20)
(define tank-velocity 100)
(define npc-velocity 70)
(define missile-velocity 200)
(define tile-width 50)
(define tile-height 50)
(define map-width 700)
(define map-height 500)
(define tile-step (+ (quotient (- map-width 1) tile-width) 1))
(define npc-maxnum 7)
(define mybackmap (read-bitmap "00.png"))
(define myforegmap (read-bitmap "barrier1.png"))
(define mystablemap (read-bitmap "stablebarrier.png"))
(define mytankmap (read-bitmap "tank1.png"))
(define mybombmap (read-bitmap "bomb.png"))
(define mymissilemap-right (read-bitmap "missile-right.png"))
(define mymissilemap-left (read-bitmap "missile-left.png"))
(define mymissilemap-up (read-bitmap "missile-up.png"))
(define mymissilemap-down (read-bitmap "missile-down.png"))
(define mygameovermap (read-bitmap "mygameover.png"))
(define mystartmap (read-bitmap "start.png"))
(define mystopmap (read-bitmap "mystop.png"))
(define mapvectlist (read-map-xml "2.tmx"))
(define barriervect-o (cadr mapvectlist))
(define barriervect 'null)
(define barriervect-length (vector-length barriervect-o))
(define foregvect-o (car mapvectlist))
(define foregvect 'null)
(define npclist '())
(define npcmissilelist '())
(define mymissilelist '())
(define bomblist '())
(define score-num 0)
(define mycurrent-time 0)
(define myfps 0)
(define frame-num 0)
(define time-elapsed 0)
(define mylife 3)
(define relive-interval 1000)
(define temp-relive-interval relive-interval)
(define mytank 'null)
(define game-state 0)
(play-sound "backsound.wav" #t)
(define create-mytank
  (lambda ()
    (set! mytank
            (new mytank%
                 [blood 1]
                 [velocity tank-velocity]
                 [direction 0]
                 [x 0]
                 [y 0]
                 [type 0]
                 [width tank-width]
                 [height tank-height]))))
(define start-game
  (lambda()
    (begin 
      (create-mytank)
      (set! npclist empty)
      (set! npcmissilelist empty)
      (set! mymissilelist empty)
      (set! bomblist empty)
      (set! score-num 0)
      (set! temp-relive-interval relive-interval)
      (set! mylife 3)
      (set! foregvect (vector-copy foregvect-o))
      (set! barriervect (vector-copy barriervect-o))
      (send mytimer start 10)
      (set! mycurrent-time (current-milliseconds))
      (set! game-state 1))))
;;;;界面方面的东西
; Derive a new frames class to handle events
(define my-frame%
  (class frame% ; The base class is canvas
    ; Define overriding method to handle keyboard events
    (define/override (on-subwindow-char receiver event)
      (cond [(> game-state 0)
             (let([key (send event get-key-code)]
                  [releasekey (send event get-key-release-code)])
               (cond 
                 [(and (key-code-symbol? key) (symbol=? key 'release))
                  (send mytank set-command 'release releasekey)]
                 [else (send mytank set-command 'press key)]))]))
    ; Call the superclass init, passing on all init args
    (super-new)))

(define myframe
  (new my-frame% [label "坦克大战"]
       [style (list 'no-resize-border)]
       ))
(define mypanel (new  horizontal-panel% [parent myframe]
                                     [alignment '(center center)]))
(define mybuttonpanel (new vertical-panel% [parent mypanel]
                                     [alignment '(center center)]
                                     [min-width 100]
                                     [min-height 500]
                                     [stretchable-height #f]
                                     [stretchable-width #f]))
(new message% [parent mybuttonpanel]
     [label "得分："]
     [auto-resize #t]
     [font (make-object font%
             10 'decorative)])
(define myscoremsg 
  (new message% [parent mybuttonpanel]
       [label "0"]
       [auto-resize #t]
       [font (make-object font%
             15 'decorative)]))
(new message% [parent mybuttonpanel]
     [label "生命："]
     [auto-resize #t]
     [font (make-object font%
             10 'decorative)])
(define mylifemsg 
  (new message% [parent mybuttonpanel]
       [label "0"]
       [auto-resize #t]
       [font (make-object font%
             15 'decorative)]))
(new message% [parent mybuttonpanel]
     [label "帧率："]
     [auto-resize #t]
     [font (make-object font%
             10 'decorative)])
(define myfpsmsg 
  (new message% [parent mybuttonpanel]
       [label "0"]
       [auto-resize #t]
       [font (make-object font%
             15 'decorative)]))
(define startbutton
  (new button% [parent mybuttonpanel]
     [label "开始游戏"]	 
     [min-height 40]	
     [stretchable-width #t]
     (callback (lambda (button event)
                 (begin
                   (send mycanvas focus)
                   (send startbutton enable #f)
                   (send restartbutton enable #t)
                   (send suspendbutton enable #t)
                   (send continuebutton enable #f)
                   (start-game)
                   )))))
(define restartbutton
  (new button% [parent mybuttonpanel]
     [label "重新游戏"]	 
     [min-height 40]	
     [stretchable-width #t]
     ;[style (list 'deleted)]
     [enabled #f]
     (callback (lambda (button event)
                 (begin
                   (play-sound "backsound.wav" #t)
                   (send mycanvas focus)
                   (send startbutton enable #f)
                   (send restartbutton enable #t)
                   (send suspendbutton enable #t)
                   (send continuebutton enable #f)
                   (start-game)
                   )))))
(define suspendbutton
  (new button% [parent mybuttonpanel]
     [label "暂停游戏"]	 
     [min-height 40]
     [enabled #f]
     [stretchable-width #t]
     (callback (lambda (button event)
                 (begin
                   (send mycanvas focus)
                   (send startbutton enable #f)
                   (send restartbutton enable #t)
                   (send suspendbutton enable #f)
                   (send continuebutton enable #t)
                   (set! game-state 2))))))
(define continuebutton
  (new button% [parent mybuttonpanel]
     [label "继续游戏"]	 
     [min-height 40]	
     [stretchable-width #t]
     [enabled #f]
     (callback (lambda (button event)
                 (begin
                   (play-sound "backsound.wav" #t)
                   (send mycanvas focus)
                   (send startbutton enable #f)
                   (send restartbutton enable #t)
                   (send suspendbutton enable #t)
                   (send continuebutton enable #f)
                   (set! mycurrent-time (current-milliseconds))
                   (set! game-state 1))))))
(define cancelbutton
  (new button% [parent mybuttonpanel]
     [label "退出游戏"] 
     [min-height 40]
     [stretchable-width #t]
     (callback (lambda (button event)
                 (begin
                   (send myframe on-close)
                   (send myframe on-exit)
                   )))))

; Make a canvas that handles events in the frame
(define mycanvas (new canvas% 
                      [parent mypanel]
                      [min-width 700]
                      [min-height 500]
                      [paint-callback
                       (lambda (canvas dc)
                         (render-canvas dc))]))
(send mypanel change-children reverse)
(define render-canvas
  (lambda (dc)
    (begin
      (send dc suspend-flush)
      (send dc erase)
      
      (cond
        [(or (= game-state 1)(= game-state 2))
         (send dc draw-bitmap-section mybackmap 0 0 0 0 map-width map-height)
         (draw-tank dc mytank)
         (draw-npc-tank dc)
         (draw-all-missile dc)
         (draw-foreg dc foregvect myforegmap)
         (draw-all-bomb dc)
         (send dc draw-bitmap mystablemap 0 0)
         (cond
           [(= game-state 2)
            (send dc  draw-bitmap mystopmap 150 50)])]
        [(= game-state 0)
         (begin
           (send dc set-pen (make-object color% 200 255 200) 2 'solid)
           (send dc set-brush (make-object color% 255 0 0) 'transparent)
           (send dc set-font (make-object font% 70 'decorative))
           (send dc draw-bitmap-section mybackmap 0 0 0 0 map-width map-height)
           (send dc draw-bitmap mystartmap 200 50))]
        [(= game-state 3)
         (send dc draw-bitmap mygameovermap 0 0)])
        
      (send myscoremsg set-label (number->string score-num))
      (send myfpsmsg set-label (number->string myfps))
      (send mylifemsg set-label (number->string mylife))
     ; (draw-foreg dc foregvect myforegmap)
      (send dc resume-flush))))
(define draw-tank
  (lambda (dc tank)
    (let* ([position (send tank get-position)]
           [state (send tank get-state)]
           [x (car position)]
           [y (cadr position)]
           [offset-x (* (send tank get-direction) tank-width)]
           [offset-y (* tank-height (send tank get-type))])
      (cond [(> state 0)
             (begin
               (cond 
                 [(= state 2) 
                  (send dc draw-rounded-rectangle (- x 1) (- y 1) (+ tank-width 2) (+ tank-height 2) 4)])
               (send dc draw-bitmap-section mytankmap x y offset-x offset-y tank-width tank-height))]))))
(define draw-missile
  (lambda (dc missile)
    (let* ([position (send missile get-position)]
           [x (car position)]
           [y (cadr position)]
           [direction (send missile get-direction)])
      (case direction
        [(0) ;up
         (send dc draw-bitmap mymissilemap-up x y)]
        [(1) ;left
         (send dc draw-bitmap mymissilemap-left x y)]
        [(2) ;right
         (send dc draw-bitmap mymissilemap-right x y)]
        [(3) ;down
         (send dc draw-bitmap mymissilemap-down x y)])
      )))
(define draw-bomb
  (lambda (dc bomb)
    (let* ([position (send bomb get-position)]
           [x (car position)]
           [y (cadr position)]
           [step (send bomb get-step)]
           [offset-x (* (remainder step 4) 32)]
           [offset-y (* (quotient step 4) 32)])
      (send dc draw-bitmap-section mybombmap x y offset-x offset-y 32 32))))
(define draw-all-bomb
  (lambda (dc)
    (let ([power-foo (lambda (bomb)
                       (draw-bomb dc bomb))])
      (for-each power-foo bomblist))))  
(define draw-npc-tank
  (lambda (dc)
    (let ([power-foo (lambda (tank)
                       (draw-tank dc tank))])
      (for-each power-foo npclist))))
(define draw-all-missile
  (lambda (dc)
    (let ([power-foo (lambda (missile)
                       (draw-missile dc missile))])
      (begin 
        (for-each power-foo npcmissilelist)
        (for-each power-foo mymissilelist)))))
(define draw-foreg
  (lambda (dc vect map)
    (let* ([draw-one-iner (lambda (dc type loc)
                      (let ([x (* (remainder loc (/ map-width tile-width))  tile-width)]
                            [y (* (quotient loc (/ map-width tile-width)) tile-height)])
                        (cond
                          [(> type 0) (send dc draw-bitmap-section map x y (* (- type 1) tile-width) 0 tile-width tile-height)])))])
      (let loop ([n 0])
        (cond
          [(< n (/ (* map-width map-height) (* tile-width tile-height))) 
           (begin
             (draw-one-iner dc (vector-ref vect n) n)
             (loop (add1 n)))])))))
;后面是关于游戏碰撞及逻辑的

(define detect-two-object-collision 
  (lambda (a b sound)
    (let ([result (send a collision-objct b)])
      (cond 
        [result 
         (begin
           (send a sub-blood 1)
           (send b sub-blood 2)
           (let([newbomb  
                 (new mybomb%
                      [x (- (car result) 16)]
                      [y (- (cadr result) 16)]
                      [size 18])])
             (cond 
               [sound
                (play-sound sound #t)])
             (set! bomblist (append bomblist (list newbomb)))))]))))
(define detect-two-list-collision
  (lambda (list1 list2  sound)
    (for-each (lambda (x)
                (for-each (lambda (y)
                            (detect-two-object-collision x y sound)) list2)) list1)))
                            
(define avoid-collision-barrier
  (lambda (amt interval )(send amt do-detect map-width map-height barriervect tile-width tile-height interval)))
(define calculate-fps
  (lambda (interval)
    (begin 
      (set! time-elapsed (+ time-elapsed interval))
      (set! frame-num (add1 frame-num))
      (cond 
        [(> time-elapsed 1000)
          (begin 
            (set! myfps (quotient (* frame-num 1000) time-elapsed))
            (set! time-elapsed 0)
            (set! frame-num 0))]))))
(define get-interval
  (lambda ()
    (let ([now-time 0]
          [interval 0])
      (begin 
        (set! now-time (current-milliseconds))
        (set! interval (- now-time mycurrent-time))
        (set! mycurrent-time now-time)
        interval))))
    
        
(define refresh-npc 
  (let ([time-interval 0])
     (lambda ()
       (let ([one->one
              (lambda (x foo)
              (lambda (y)
                (foo y x)))])
         (begin
           (set! time-interval (get-interval))
           (calculate-fps time-interval)
           (case  game-state
             [(0) (void)]
             [(1)
              (begin
                (cond 
                  [(<= (send mytank get-state) 0)
                   (cond
                     [(<= mylife 0) ;gameover
                      (set! game-state 3)]
                     [(> temp-relive-interval 0)
                      (set! temp-relive-interval (- temp-relive-interval time-interval))]
                     [else
                      (begin 
                        (set! temp-relive-interval relive-interval)
                        (set! mylife (sub1 mylife))
                        (create-mytank))])]
                  [else (update-tank mytank time-interval)])
                (set! npclist (filter (lambda (amt)
                                        (cond
                                          [(> (send amt get-blood) 0) #t]
                                          [else
                                           (begin 
                                             (set! score-num (add1 score-num))
                                             #f)]))
                                      npclist))
                (cond
                  [(and (< (length npclist) npc-maxnum) (< (random 1000) (- npc-maxnum (length npclist))))
                   (set! npclist (append npclist (list (create-npc))))])
                (for-each (one->one time-interval update-tank) npclist)
                (set! npcmissilelist (filter (lambda (amt)
                                               (> (send amt get-blood) 0))
                                             npcmissilelist))
                (set! mymissilelist (filter (lambda (amt)
                                              (> (send amt get-blood) 0))
                                            mymissilelist))
                (for-each (one->one time-interval update-missile) npcmissilelist)
                (for-each (one->one time-interval update-missile) mymissilelist)
                (set! bomblist (filter (lambda (amt)
                                         (send amt is-alive))
                                       bomblist))
                (detect-two-list-collision npclist mymissilelist "bomb-tank.wav")
                (cond 
                  [(> (send mytank get-state) 0)
                   (detect-two-list-collision (list mytank) npcmissilelist "bomb-tank.wav")])
                (detect-two-list-collision npcmissilelist mymissilelist "bomb-missile.wav"))]
             ))))))
(define remove-barrier
  (lambda (ref-num)
    (cond
      [(and ref-num (> (vector-ref barriervect ref-num) 5))
       (begin
         (vector-set! barriervect ref-num 0)
         (vector-set! foregvect ref-num 0))])))
(define update-missile
  (lambda (missile interval)
    (begin
      (let ([result (avoid-collision-barrier missile interval)])
        (cond 
          [result 
           (let([newbomb  
                 (new mybomb%
                      [x (- (car result) 16)]
                      [y (- (cadr result) 16)]
                      [size 8])]
                [ref-num1 (caddr result)]
                [ref-num2 (cadddr result)])
             (begin
               ;(play-sound "bomb.wav" #t)
               (set! bomblist (append bomblist (list newbomb)))
               (remove-barrier ref-num1)
               (remove-barrier ref-num2)))])
        (send missile move)))))

  
(define update-tank
  (lambda (tank interval)
    (begin
      (avoid-collision-barrier tank interval)
      (send tank move)
      (create-missile tank)
      )))
(define create-missile
  (lambda (tank)
    (cond
      [(send tank do-fire)
       (let([position (send tank get-missile-position missile-width missile-height)]
             [tempdirection (send tank get-direction)]
             [type (send tank get-type)]
             [temp-width missile-width]
             [temp-height missile-height])
         (begin
           (cond [(or (= tempdirection 0) (= tempdirection 3))
                  (begin 
                    (set! temp-width missile-height)
                    (set! temp-height missile-width))])
           (let([newmissile  (new mymissile%
                               [velocity missile-velocity]
                               [direction  tempdirection]
                               [x (car position)]
                               [y (cadr position)]
                               [width temp-width]
                               [height temp-height])])
             (cond 
               [(= type 0)
                (set! mymissilelist (append mymissilelist (list newmissile)))]
               [else
                (set! npcmissilelist (append npcmissilelist (list newmissile)))]))))])))
         
(define find-npc-loction
  (lambda ()
    (let ([refnum (random barriervect-length)])
      (cond
        [(= (vector-ref barriervect refnum) 0) refnum]
        [else (find-npc-loction)]))))
(define create-npc
  (lambda()
    (let ([location (find-npc-loction)])
      (new mytank%
       [blood 1]
       [velocity npc-velocity]
       [direction (random 4)]
       [x (* (remainder location tile-step) tile-width)]
       [y (* (quotient location tile-step) tile-height)]
       [type 1]
       [width tank-width]
       [height tank-height]))))
                
  (define mytimer 
    (new timer%
       [interval #f]
       [notify-callback
        (lambda ()
          (begin
            (refresh-npc)
            (render-canvas (send mycanvas get-dc))
           ))]))
  (send myframe show #t)