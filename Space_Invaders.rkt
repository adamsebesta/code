(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 2)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 2)
(define TANK-SPEED 20)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 50)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

INVADER-Y-SPEED
(define MTS (empty-scene WIDTH HEIGHT))


;; Data Definitions:

; GAME


(define-struct game (invaders missiles tank time))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition


#;(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))
       (fn-for-time (game-time time))))


; TANK


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))       INVADER-Y-SPEED    ;going left


(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



; INVADER


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;;ListOfInvader is one of:
;; -empty
;; - (list invader ListOfinvader)
(define LOI0 (list I1 I2 I3)) ;3 invaders on the screen

#;(define (fn-for-loi loi)
    (cond [(empty? loi) (...)]
          [else
           (... (fn-for-invader (first loi))
                (fn-for-loi (rest loi)))]))



; MISSILE


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates
INVADER-Y-SPEED
(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1


#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; 
;;ListOfMissile is one of:
;; -empty
;; - (list missile ListOfMissile)
(define LOM0 (list M1 M2 M3)) ;3 missiles on the screen

(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missle (first lom))
              (fn-for-lom (rest lom)))]))




(define G0 (make-game empty empty T0 1))
(define G1 (make-game empty empty T1 10))
(define G2 (make-game (list I1) (list M1) T1 5))
(define G3 (make-game (list I1 I2) (list M1 M2) T1 1))


; TIME


(define-struct time (n))
;; n represents amount of game time that has passed, higher the #, better the score!





;; =================
;; Functions:

;; Game -> Game
;; start the world with ...
;; 
(define (main g)
  (big-bang g                   ; WS
    (on-tick   advance-game)     ; WS -> WS
    (to-draw   render-game)   ; WS -> Image
    (stop-when game-over)      ; WS -> Boolean
    (on-key    handle-key)))    ; WS KeyEvent -> WS



;  ON TICK FUNCTIONS


;; game -> game
;; produce the next ...
;; !!!
;(define (advance-game g) ...)

(check-expect (advance-game G0) (make-game '() '() (make-tank 150 1) 2))
(check-expect (advance-game (make-game (list (make-invader 100 200 -10) (make-invader WIDTH 10 10 )) (list (make-missile 10 10)) T0 1))
              (make-game (list (make-invader (- 100 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) -10) (make-invader  298  (+ 10 INVADER-Y-SPEED) -10)) 
                         (list (make-missile 10 (- 10 MISSILE-SPEED))) T0 2))
              

(define (advance-game g)
  (spawn (advance-objects (scrub-all (updateTimer g)))))
;
;
(define (advance-objects g)
  (make-game (advance-invaders (game-invaders g))
             (advance-missiles (game-missiles g))
             (game-tank g)
             (game-time g)))



;; updateTimer
;; Game -> Game
;; updates game time by 1 tick
;;(define updateTimer g) g
(check-expect (updateTimer G0)
              (make-game empty empty T0 2))
(check-expect (updateTimer G2)
              (make-game (list I1) (list M1) T1 6))

(define (updateTimer g)
  (make-game (game-invaders g) (game-missiles g) (game-tank g) (+ (game-time g) 1) ))



;;g -> g
;;spawn new invader into list of invaders per INVADE-RATE

INVADE-RATE 
(check-expect (spawn (make-game   
                      (list (make-invader (+ INVADER-X-SPEED 100 ) (+  INVADER-Y-SPEED 200 ) 10)
                            (make-invader (+ INVADER-X-SPEED 200 ) (+  INVADER-Y-SPEED 300 ) 10)) 
                      (list (make-missile 500 300)) T0 1))
              (make-game (list (make-invader (+ INVADER-X-SPEED 100 ) (+  INVADER-Y-SPEED 200 ) 10)
                               (make-invader (+ INVADER-X-SPEED 200 ) (+  INVADER-Y-SPEED 300 ) 10)) 
                         (list (make-missile 500 300)) T0 1))
(check-expect (spawn (make-game   
                      (list (make-invader (+ INVADER-X-SPEED 100 ) (+  INVADER-Y-SPEED 200 ) 10)
                            (make-invader (+ INVADER-X-SPEED 200 ) (+  INVADER-Y-SPEED 300 ) 10)) 
                      (list (make-missile 500 300)) T0 20))
              (make-game (cons (make-invader (random WIDTH) 0 (random 10)) (list (make-invader (+ INVADER-X-SPEED 100 ) (+  INVADER-Y-SPEED 200 ) 10)
                                                                                 (make-invader (+ INVADER-X-SPEED 200 ) (+  INVADER-Y-SPEED 300 ) 10))) 
                         (list (make-missile 500 300)) T0 20))

(define (spawn g)
  (if (integer? (/ (game-time g) INVADE-RATE))
      (make-game (cons (make-invader (random WIDTH) 0 (random 10)) (game-invaders g)) (game-missiles g) (game-tank g) (game-time g))
      g ))



;; g -> g
;; removes invaders and missiles that collide
(check-expect (scrub-all (make-game empty empty T0 0))
              (make-game empty empty T0 0))

(check-expect (scrub-all (make-game   
                          (list (make-invader (+ INVADER-X-SPEED 100 ) (+  INVADER-Y-SPEED 200 ) 10)
                                (make-invader (+ INVADER-X-SPEED 200 ) (+  INVADER-Y-SPEED 300 ) 10)) 
                          (list (make-missile 500 300)) T0 1))
              (make-game (list (make-invader (+ INVADER-X-SPEED 100 ) (+  INVADER-Y-SPEED 200 ) 10)
                               (make-invader (+ INVADER-X-SPEED 200 ) (+  INVADER-Y-SPEED 300 ) 10)) 
                         (list (make-missile 500 300)) T0 1))
                         
(check-expect (scrub-all (make-game   
                          (list (make-invader (+ INVADER-X-SPEED 100 ) (+  INVADER-Y-SPEED 200 ) 10)
                                (make-invader (+ INVADER-X-SPEED 200 ) (+  INVADER-Y-SPEED 300 ) 10) 
                                (make-invader (+ INVADER-X-SPEED 500 ) (+  INVADER-Y-SPEED 300 ) 10)) 
                          (list (make-missile 500 300)) T0 1))
              (make-game  (list (make-invader (+ INVADER-X-SPEED 100 ) (+  INVADER-Y-SPEED 200 ) 10)
                                (make-invader (+ INVADER-X-SPEED 200 ) (+  INVADER-Y-SPEED 300 ) 10)) 
                          empty T0 1))

(define (scrub-all g)
  (make-game
   (scrub-i (game-invaders g) (game-missiles g))
   (scrub-m (game-missiles g) (game-invaders g))
   (game-tank g)
   (game-time g)))




;; i -> i
;; advance single invader, bouncing off width and 0
;(define (advance-invader i) i)


(check-expect (advance-invader (make-invader 0 100 -10)) (make-invader 2 (+ 100 INVADER-Y-SPEED) 10))
(check-expect (advance-invader (make-invader 2 100 -10)) (make-invader 0 (+ 100 INVADER-Y-SPEED) -10))
(check-expect (advance-invader (make-invader 10 100 10)) (make-invader (+ 10 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 10))
(check-expect (advance-invader (make-invader WIDTH 100 -10)) (make-invader  298  (+ 100 INVADER-Y-SPEED) -10))
(check-expect (advance-invader (make-invader WIDTH 100 10)) (make-invader 298 (+ 100 INVADER-Y-SPEED) -10))
(check-expect (advance-invader (make-invader (- WIDTH 2) 100 10)) (make-invader WIDTH (+ 100 INVADER-Y-SPEED) 10))
(check-expect (advance-invader (make-invader (- WIDTH 2) 100 -10)) (make-invader (- (- WIDTH 2) INVADER-X-SPEED)  (+ 100 INVADER-Y-SPEED) -10))
(check-expect (advance-invader (make-invader WIDTH 100 10)) (make-invader 298 (+ 100 INVADER-Y-SPEED) -10))

(define (advance-invader i)
  (cond [ (and (<= (invader-x i) 0) (positive? (invader-dx i)))
          (make-invader (+ INVADER-X-SPEED (invader-x i)) (+ INVADER-Y-SPEED (invader-y i)) (invader-dx i))]
          [(and (<= (invader-x i) 0) (negative? (invader-dx i)))
           (make-invader (+ INVADER-X-SPEED (invader-x i)) (+ INVADER-Y-SPEED (invader-y i)) (abs (invader-dx i)))]
        [ (and (>= (invader-x i) WIDTH) (positive? (invader-dx i)))
          (make-invader (- (invader-x i) INVADER-X-SPEED ) (+ INVADER-Y-SPEED (invader-y i)) (- (invader-dx i)))]
       [ (and (>= (invader-x i) WIDTH) (negative? (invader-dx i)))
          (make-invader (- (invader-x i) INVADER-X-SPEED ) (+ INVADER-Y-SPEED (invader-y i))  (invader-dx i))]
        [ else
          (if (positive? (invader-dx i))
              (make-invader (+ INVADER-X-SPEED (invader-x i)) (+ INVADER-Y-SPEED (invader-y i)) (invader-dx i))
              (make-invader (- (invader-x i) INVADER-X-SPEED ) (+ INVADER-Y-SPEED (invader-y i)) (invader-dx i)))]))
 
                                                       
 




;; loi -> loi
;;  list of invaders, heading + on Y, bouncing off the walls
;(define (advance-invaders loi) ...)

(check-expect (advance-invaders empty) empty)

(check-expect (advance-invaders (list (make-invader 0 100 -10) (make-invader 2 100 -10) (make-invader 10 100 10)))
              (list (make-invader 2 (+ 100 INVADER-Y-SPEED) 10) (make-invader 0 (+ 100 INVADER-Y-SPEED) -10)
                    (make-invader (+ 10 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 10)))

(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons  (advance-invader (first loi))
                (advance-invaders (rest loi)))]))




;; lom -> lom
;; advance all missiles in list
;(define (advance-missiles lom) lom)

(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (list (make-missile 10 MISSILE-SPEED))) (list (make-missile 10 (-  MISSILE-SPEED 10))))
(check-expect (advance-missiles (list (make-missile 10 50) (make-missile 10 (- 50 MISSILE-SPEED)))) 
              (list (make-missile 10 (- 50 MISSILE-SPEED)) (make-missile 10  (- 40 MISSILE-SPEED))))

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (off? (first lom))
                (advance-missiles (rest lom))
                (cons (advance-missile (first lom))
                        (advance-missiles (rest lom))))]))


(define (advance-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED  )))




;; Missile -> Boolean
;; produce false if missile is off screen
(check-expect (off? (make-missile 100 100)) false)
(check-expect (off? (make-missile 100 0)) false)
(check-expect (off? (make-missile 100 (- (/ (image-height MISSILE) 2)))) true)
(check-expect (off? (make-missile 10 -40)) true)

;(define (off? m) false) ;stub

(define (off? m)
  (<= (missile-y m) (- (/ (image-height MISSILE) 2))))





; lom loi -> lom 
; scrub list of any hit missiles            
;(define (scrub-m lom loi)lom )

(check-expect (scrub-m empty empty) empty)
(check-expect (scrub-m (list (make-missile 200 300))  (list (make-invader (+ INVADER-X-SPEED 100 ) (+  INVADER-Y-SPEED 200 ) 10)))
                                                    
              (list (make-missile 200 300)))
                  
(check-expect (scrub-m (list (make-missile 500 300))  (list (make-invader (+ INVADER-X-SPEED 100 ) (+  INVADER-Y-SPEED 200 ) 10)
                                                            (make-invader (+ INVADER-X-SPEED 200 ) (+  INVADER-Y-SPEED 300 ) 10)))
              (list  (make-missile 500 300)))    


(define (scrub-m lom loi)
  (cond [(empty? lom) empty]
        [else
         (cond [(m-hit? (first lom) loi) (scrub-m (rest lom) loi)]
               [else
                (cons (first lom) (scrub-m (rest lom) loi))])]))

; loi lom -> loi
; scrub list of any hit invaders
;(define (scrub-i i lom) loi)

(check-expect (scrub-i empty empty) empty)
(check-expect (scrub-i (list(make-invader (+ INVADER-X-SPEED 200 ) (+  INVADER-Y-SPEED 300 ) 10))
                       (list (make-missile 200 300))) empty)
(check-expect (scrub-i (list (make-invader (+ INVADER-X-SPEED 402 ) (+  INVADER-Y-SPEED 403 ) 10) (make-invader (+ INVADER-X-SPEED 200 ) (+  INVADER-Y-SPEED 300 ) 10))
                       (list (make-missile 300 300) (make-missile 200 300)(make-missile 400 400)))
              empty)
(check-expect (scrub-i (list (make-invader (+ INVADER-X-SPEED 250 ) (+  INVADER-Y-SPEED 300 ) 10))
                       (list (make-missile 300 300) (make-missile 200 300)) ) (list (make-invader (+ INVADER-X-SPEED 250 ) (+  INVADER-Y-SPEED 300 ) 10)))
              
(define (scrub-i loi lom)
  (cond [(empty? loi) empty]
        [else
         (cond [(i-hit? (first loi) lom) (scrub-i (rest loi) lom)]
               [else
                (cons (first loi) (scrub-i (rest loi) lom))])]))


;; m loi -> Boolean
;; produce true if Missile pos = within range of invader pos

(check-expect (m-hit? (make-missile 1 10)   (list(make-invader (+ INVADER-X-SPEED 100 ) (+ INVADER-Y-SPEED 200 ) 10))) false)
(check-expect (m-hit? (make-missile 100 200)  
                      (list(make-invader (+ INVADER-X-SPEED 100 ) (+  INVADER-Y-SPEED 200 ) 10))) true)
(check-expect (m-hit? (make-missile 100 200)  
                      (list (make-invader (+ INVADER-X-SPEED 400 ) (+  INVADER-Y-SPEED 200 ) 10) (make-invader (+ INVADER-X-SPEED 100 ) (+  INVADER-Y-SPEED 200 ) 10))) true)


(define (m-hit? m loi)
  (cond [(empty? loi) false]
        [ else
          (if (and (<= (abs (- (invader-x (first loi)) (missile-x m))) HIT-RANGE) (<= (abs (- (missile-y m) (invader-y (first loi)))) HIT-RANGE))
              true
              (m-hit? m (rest loi)))]))



;; m loi -> Boolean
;; produce true if Missile pos = within range of invader pos
;; (define (i-hit? i lom) false

(check-expect (i-hit?   (make-invader (+ INVADER-X-SPEED 100 ) (+ INVADER-Y-SPEED 200 ) 10) (list (make-missile 1 10)  )) false)
(check-expect (i-hit?  (make-invader (+ INVADER-X-SPEED 100 ) (+  INVADER-Y-SPEED 200 ) 10) (list(make-missile 100 200)))  
              true)


(define (i-hit? i lom)
  (cond [(empty? lom) false]
  
        [else
         (if (and (<= (abs (- (missile-x (first lom)) (invader-x i))) HIT-RANGE)  (<= (abs (- (missile-y (first lom)) (invader-y i))) HIT-RANGE) )
             true
             (i-hit? i (rest lom)))]))
          
  
           
;; game -> game
;; end game when invader reaches HEIGHT, 

(check-expect (game-over (make-game  (list (make-invader 100 200 10) (make-invader 100 (- HEIGHT 1) -10)) LOM0 T1 1))
              false )
(check-expect (game-over (make-game  (list (make-invader 100 200 -10) (make-invader 100 HEIGHT 10 )) LOM0 T1 1)) 
              true)



;(define (game-over g)...)


(define (game-over g)
 
  (if (dead? (game-invaders g))
      true
      false))


;; loi-> Boolean
;; checks list to see if any invaders have reached limit
;(define (dead? loi) true)
(check-expect (dead? (list (make-invader 100 200 -10) (make-invader 100 HEIGHT 10 ))) true)
(check-expect (dead? (list (make-invader 100 200 -10) (make-invader 100 (- HEIGHT 1) 10 ))) false)
(check-expect (dead? (list (make-invader 100 200 -10) (make-invader 100 (+ HEIGHT 1) 10 ) (make-invader 100 (-  HEIGHT 1) 10 ))) true)
                                         
(define (dead? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT) 
             true
             (dead? (rest loi)))]))



; ;;Key-event functions


;; keyevent game-> game
;; move tank left or right with arrow keys, fire missles w/ space bar
;(define (handle-key g ke) g)

(check-expect (handle-key  (make-game empty empty T0 1) "a" ) (make-game empty empty T0 1))
(check-expect (handle-key   (make-game empty empty (make-tank 50 1)1) "left") (make-game empty empty (make-tank (-  50 TANK-SPEED) -1) 1))
(check-expect (handle-key   (make-game empty empty (make-tank 50 -1)1) "right") (make-game empty empty (make-tank (+  50 TANK-SPEED) 1) 1))
(check-expect (handle-key   (make-game empty empty (make-tank 50 1)1) "right") (make-game empty empty (make-tank (+  50 TANK-SPEED) 1) 1))
(check-expect (handle-key   (make-game empty empty (make-tank 50 -1)1) "left") (make-game empty empty (make-tank (-  50 TANK-SPEED) -1) 1))
(check-expect (handle-key  (make-game LOI0 empty (make-tank 50 1) 1) " " ) (make-game LOI0 (list (make-missile 50 (- HEIGHT 1))) (make-tank 50 1) 1))


(define (handle-key g ke)
  (cond [(key=? ke " ") (make-game (game-invaders g) (cons (make-missile (tank-x (game-tank g)) (- HEIGHT 1)) (game-missiles g)) (game-tank g) (game-time g))] 
        [ (key=? ke "left")
          (cond [ (negative? (tank-dir (game-tank g))) 
                  (make-game (game-invaders g) (game-missiles g) (make-tank (- (tank-x (game-tank g)) TANK-SPEED) (tank-dir (game-tank g) )) (game-time g))]  
                [ (positive? (tank-dir (game-tank g)))
                  (make-game (game-invaders g) (game-missiles g) (make-tank (- (tank-x (game-tank g)) TANK-SPEED) (- (tank-dir (game-tank g))) ) (game-time g))]
                [ (< 0 (tank-x (game-tank g)))
                   (make-game (game-invaders g) (game-missiles g) (game-tank g) (game-time g))])]
        [(key=? ke "right") 
         (cond  [ (positive? (tank-dir (game-tank g)))
                  (make-game (game-invaders g) (game-missiles g) (make-tank (+ (tank-x (game-tank g)) TANK-SPEED) (tank-dir (game-tank g)))(game-time g) )]  
                [(negative? (tank-dir (game-tank g)))
                 (make-game (game-invaders g) (game-missiles g) (make-tank (+ (tank-x (game-tank g)) TANK-SPEED) (- (tank-dir (game-tank g))))(game-time g) )]             
                [(> WIDTH  (tank-x (game-tank g)))
                 (make-game (game-invaders g) (game-missiles g) (game-tank g) (game-time g))])]
        [else g]))
              
        



; ;;; Render Functions




;; game -> Image
;; render current state of game 
;; tests apply from below




 (define (render-game g)
   (render-tank (game-tank g)
                   (render-missiles (game-missiles g)
                                    (render-time (game-time g)
                                       (render-invaders (game-invaders g)
                                                                    )))))


;; renderInvaders
;; loi -> Image
;; renders all the invaders in the list
;; tests apply from below


(define (render-invaders loi)
  (cond [(empty? loi) MTS]
        [else
         (place-invader (first loi ) 
                        (render-invaders (rest loi)))]))



;; i  img-> img
;; consumes invader and img and renders invaders on that image in appropriate position

(check-expect (place-invader I1 MTS) (place-image INVADER 150 100 MTS))


;;(define (place-invader i img) img


(define (place-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))


;; renderMissiles
;; lom img -> Image
;; renders all the missiles in the list
;;test applies from place missile


(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (place-missile (first lom )
                        (render-missiles (rest lom) img))]))


;; placeMissile
;; consumes missile and img and renders missiles on that image in appropriate position
;;m img -> image

(check-expect (place-missile (make-missile 1 2) MTS)  (place-image MISSILE 1 2 MTS))


;(define (place-missile m img) img)

(define (place-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))



;;t -> img
;; consumes game-tank and renders image on MTS


(check-expect (render-tank (make-tank 1  10)  MTS) (place-image TANK 1 (- HEIGHT 5) MTS))

;(define (render-tank t img) img)
(define (render-tank t img)
  (place-image TANK (tank-x t) (- HEIGHT 5) img))




;; game time-> img
;; consumes game time and renders time on top left of screen
;(define render-time g) img


(check-expect (render-time  10 MTS) (place-image (text (number->string 10) 18 "black") 20 15 MTS)) 


(define (render-time n img)
  (place-image (text (number->string  n) 18 "black") 20 15 img)) 




