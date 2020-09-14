;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 2)   ; if increased, game will be more difficult

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


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 1))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;;ListOfInvader is one of:
;; - empty
;; - (cons invader ListOfInvader)
;; interp. a list of invaders

(define LOI1 empty)
(define LOI2 (cons I1 empty))
(define LOI3 (cons I1 (cons I2 empty)))

#;
(define (fn-for-loi loi)
  (cond[(empty? loi) (...)]
       [else
        (... (fn-for-invader (first loi))
             (fn-for-loi (rest loi)))]))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1) 5)))   ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)

(define LOM1 empty)
(define LOM2 (cons (make-missile 150 300) (cons M2 empty)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

(define START (make-game empty empty (make-tank (/ WIDTH 2) 1)))

;; =================
;; Functions:

;; Game -> Game
;; start the world with (main START)
(define (main s)
  (big-bang s                                 ; Game
    (on-tick   advance-game)                  ; Game -> Game
    (stop-when game-over game-over-screen)    ; Game -> Boolean
    (to-draw   render-game)                   ; Game -> Image
    (on-key    control-tank)))                ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next game by:
;; - advancing invaders, removing the ones that were hit and creating new ones
;; - advancing missiles, removing the ones past the screen
;; - advancing the tank

;(define (advance-game s) s) ;stub

(define (advance-game s)
  (make-game (remaining-invaders (game-missiles s) (create-invaders (advance-invaders (game-invaders s))))
             (advance-missiles (game-missiles s))
             (advance-tank (game-tank s))))


;; ListOfInvader -> ListOfInvader
;; produce list of invaders ticked in the direction they are travelling

(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders (cons (make-invader 150 HEIGHT -1) empty))
              (cons (make-invader (+ 150 (- INVADER-X-SPEED)) (+ HEIGHT INVADER-Y-SPEED) -1) empty))

;(define (advance-invaders loi) loi) ;stub

(define (advance-invaders loi)
  (cond[(empty? loi) empty]
       [else
        (cons (advance-singleinvader (first loi))
              (advance-invaders (rest loi)))]))

;; Invader -> Invader
;; advance invader x and y by dx; bounces off edges

(check-expect (advance-singleinvader (make-invader 150 100 -1))                       ; on screen, moving left
              (make-invader (+ 150 (- INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) -1))
(check-expect (advance-singleinvader (make-invader 150 100 1))                        ; on screen, moving right
              (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1))


(check-expect (advance-singleinvader (make-invader 0 100 -1))                         ; on left edge, changes direction
              (make-invader 0 (+ 100 INVADER-Y-SPEED) 1))
(check-expect (advance-singleinvader (make-invader (+ 0 (- INVADER-X-SPEED)) 100 -1)) ; is going to pass left edge, x coordinate will be set to 0 and will change direction
              (make-invader 0 (+ 100 INVADER-Y-SPEED) 1))

(check-expect (advance-singleinvader (make-invader WIDTH 100 1))                      ; on right edge, changes direction
              (make-invader WIDTH (+ 100 INVADER-Y-SPEED) -1))
(check-expect (advance-singleinvader (make-invader (+ WIDTH INVADER-X-SPEED) 100 1))  ; is going to pass right edge, x coordinate will be set to WIDTH and will change direction
              (make-invader WIDTH (+ 100 INVADER-Y-SPEED) -1))
                                   
;(define (advance-singleinvader i) i) ;stub

(define (advance-singleinvader i)
  (cond [(<= (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) 0)
         (make-invader 0 (+ (invader-y i) INVADER-Y-SPEED) (- (invader-dx i)))]
        [(>= (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) WIDTH)
         (make-invader WIDTH (+ (invader-y i) INVADER-Y-SPEED) (- (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))]))

;; ListOfInvader -> ListOfInvader
;; produce new list of invaders with created invaders at an INVADE-RATE
;; No tests as random is used for 2 different intervals and produces different results

;(define (create-invaders loi) loi) ;stub

(define (create-invaders loi)
  (if (< (random 100) INVADE-RATE)
      (cons (make-invader (random WIDTH) 0 (random-invader-direction 1)) loi)
      loi))

;; Natural -> Natural
;; given a natural number, produce either a postive or negative value

(define (random-invader-direction n)
  (if (> (random 10) 5)
      n
      (- n)))

;; ListOfMissile ListOfInvader -> ListOfInvader
;; produce a new list of invaders eliminating the ones that were destroyed by missiles 

(check-expect (remaining-invaders empty empty) empty)                           
(check-expect (remaining-invaders LOM2 empty) empty)
(check-expect (remaining-invaders empty LOI2) LOI2)                             
(check-expect (remaining-invaders (cons (make-missile 150 (- WIDTH 5)) empty)   ; removes ListOfInvader as they were hit
                                  (cons (make-invader 150 300 1) empty))
              empty)
(check-expect (remaining-invaders (cons (make-missile 100 100) empty)           ; keeps ListOfInvader as they were not hit 
                                  (cons I1 empty))
              (cons I1 empty))

;(define (remaining-invaders lom loi) loi) ;stub

(define (remaining-invaders lom loi)
  (cond[(empty? lom) loi]
       [(empty? loi) empty]
       [else
        (if (hit? (first loi) lom)
            (rest loi)
            (cons (first loi) (remaining-invaders lom (rest loi))))]))

;; Invader ListOfMissile -> Boolean
;; returns true if missile is in a HIT-RANGE radius of the invader (both horizontally and vertically)

(check-expect (hit? (make-invader 150 300 1) LOM2) true)
(check-expect (hit? (make-invader 140 70 -1) LOM2) false)

;(define (hit? i lom) true) ;stub

(define (hit? i lom)
  (cond [(empty? lom) false]
        [else
         (if (and (< (- (invader-x i) HIT-RANGE) (missile-x (first lom)) (+ (invader-x i) HIT-RANGE))
                  (< (- (invader-y i) HIT-RANGE) (missile-y (first lom)) (+ (invader-y i) HIT-RANGE)))
             true
             (hit? i (rest lom)))]))


;; ListOfMissile -> ListOfMissile
;; produce a new list of ticked and removed missiles

(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (cons (make-missile 150 300) empty))                                  ; advances missile y coordinate at MISSILE-SPEED
              (cons (make-missile 150 (- 300 MISSILE-SPEED)) empty))
(check-expect (advance-missiles (cons (make-missile 150 300) (cons (make-missile 100 -10) empty)))    ; removes a missile that passed the screen
              (cons (make-missile 150 (- 300 MISSILE-SPEED)) empty))

;(define (advance-missiles lom) lom);stub

(define (advance-missiles lom)
  (onscreen-only (next-missiles lom)))
  
                      
;; ListOfMissile -> ListOfMissile
;; advances the missiles at MISSILE-SPEED

(check-expect (next-missiles empty) empty)
(check-expect (next-missiles (cons (make-missile 150 300) empty))
              (cons (make-missile 150 (- 300 MISSILE-SPEED)) empty))
(check-expect (next-missiles (cons (make-missile 150 300)
                                   (cons (make-missile 100 100) empty)))
              (cons (make-missile 150 (- 300 MISSILE-SPEED))
                    (cons (make-missile 100 (- 100 MISSILE-SPEED)) empty)))


;(define (next-missiles lom) lom) ;stub

(define (next-missiles lom)
  (cond[(empty? lom) empty]
       [else
        (cons (next-singlemissile (first lom))
              (next-missiles (rest lom)))]))

;; Missile -> Missile
;; advances a single missile

(check-expect (next-singlemissile (make-missile 150 300))
              (make-missile 150 (- 300 MISSILE-SPEED)))
(check-expect (next-singlemissile (make-missile 100 100))
              (make-missile 100 (- 100 MISSILE-SPEED)))

;(define (next-singlemissile m) m) ;stub

(define (next-singlemissile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))
               

;; ListOfMissile -> ListOfMissile
;; produce a list of the missiles on screen only

(check-expect (onscreen-only empty) empty)
(check-expect (onscreen-only (cons (make-missile 150 300) empty))
              (cons (make-missile 150 300) empty))
(check-expect (onscreen-only (cons (make-missile 150 300)
                                   (cons (make-missile 100 -10) empty)))
              (cons (make-missile 150 300) empty))

;(define (onscreen-only lom) lom) ;stub

(define (onscreen-only lom)
  (cond[(empty? lom) empty]
       [else
        (if (on-screen? (first lom))
            (cons (first lom) (onscreen-only (rest lom)))
            (onscreen-only (rest lom)))]))

;; Missile -> Boolean
;; produce true if missile is on screen

(check-expect (on-screen? (make-missile 150 300)) true)
(check-expect (on-screen? (make-missile 150 -2)) false)

;(define (on-screen? m) true) ;stub

(define (on-screen? m)
  (<= 0 (missile-y m)))


;; Tank -> Tank
;; Produce the next tank, by advancing its x TANK-SPEED pixels in the direction it is moving, stop when edge is reached

(check-expect (advance-tank (make-tank 100 -1))                 ; on screen, going left
              (make-tank (+ 100 (- TANK-SPEED)) -1))

(check-expect (advance-tank (make-tank 100 1))                  ; on screen, going right
              (make-tank (+ 100 TANK-SPEED) 1))

(check-expect (advance-tank (make-tank (- WIDTH TANK-SPEED) 1)) ; reaches edge, stops moving
              (make-tank WIDTH 0))

(check-expect (advance-tank (make-tank TANK-SPEED -1))          ; reaches edge, stops moving
              (make-tank 0 0))

;(define (advance-tank t) t)

(define (advance-tank t)
  (cond  [(<= (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) 0) (make-tank 0 0)]
         [(>= (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) WIDTH) (make-tank WIDTH 0)]
         [else
          (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))


;; ===============  

;; Game -> Boolean
;; returns true when at least one invader has reached the bottom of the screen
;; tests done on helper function

;(define (game-over s) true) ;stub

(define (game-over s)
  (cond [(empty? (game-invaders s)) false]
        [else
         (invaded? (game-invaders s))]))

;; ListOfInvader -> Boolean
;; returns true if an invader has successfully invaded (passed the bottom of the screen)

(check-expect (invaded? empty) false)
(check-expect (invaded? (cons (make-invader 40 (+ HEIGHT 10) 1) empty))
              true)
(check-expect (invaded? (cons (make-invader 40 (- HEIGHT 50) -1) empty))
              false)
              

;(define (invaded? loi) true) ;stub

(define (invaded? loi)
  (cond[(empty? loi) false]
       [else
        (if (>= (invader-y (first loi)) HEIGHT)
            true
            (invaded? (rest loi)))]))

;; Game -> Boolean
;; shows "Game Over" on the screen
              
;(define (game-over-screen s) BACKGROUND)

(define (game-over-screen s)
  (place-image (text (string-append "GAME" " " "OVER") 30 "red")
               (/ WIDTH 2) (/ HEIGHT 2)
               (empty-scene WIDTH HEIGHT "black")))
               

;; ===============  

;; Game -> Image
;; render a game with invaders, missiles and tank at their positions on BACKGROUND

;(define (render-game s) BACKGROUND) ;stub

(define (render-game s)
  (render-invaders (game-invaders s)
                   (render-missiles (game-missiles s)
                                    (render-tank (game-tank s)))))


;; Tank -> Image
;; render the image of the tank x coordinate and y coordinate (- HEIGHT TANK-HEIGHT/2) onto the BACKGROUND
(check-expect (render-tank (make-tank (/ WIDTH 2) 1))
              (place-image TANK
                           (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))

;(define (render-tank t) BACKGROUND) ;stub

(define (render-tank t)
  (place-image TANK
               (tank-x t) (- HEIGHT TANK-HEIGHT/2)
               BACKGROUND))


;; ListOfMissile Image -> Image
;; given an image, renders a list of missiles

(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (cons M1 (cons (make-missile 20 20) empty)) BACKGROUND)
              (place-image MISSILE
                           150 300
                           (place-image MISSILE
                                        20 20
                                        BACKGROUND)))                
                               
;(define (render-missiles lom img) BACKGROUND) ;stub

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (render-singlemissile (first lom) (render-missiles (rest lom) img))]))

;; Missile Image -> Image
;; given an image render a single missile

(check-expect (render-singlemissile (make-missile 20 20) BACKGROUND)
              (place-image MISSILE
                           20 20
                           BACKGROUND))

;(define (render-singlemissile m img) BACKGROUND) ;stub

(define (render-singlemissile m img)
  (place-image MISSILE
               (missile-x m) (missile-y m)
               img))


;; ListOfInvader Image -> Image
;; given an image, renders a list of invaders

(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (cons I1 (cons I2 empty)) BACKGROUND)
              (place-image INVADER
                           150 100
                           (place-image INVADER
                                        150 HEIGHT
                                        BACKGROUND)))                
                               
;(define (render-invaders loi img) BACKGROUND) ;stub

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (render-singleinvader (first loi) (render-invaders (rest loi) img))]))

;; Invader Image -> Image
;; given an image render a single invader

(check-expect (render-singleinvader (make-invader 40 200 1) BACKGROUND)
              (place-image INVADER
                           40 200
                           BACKGROUND))
              
;(define (render-singleinvader i img) BACKGROUND) ;stub

(define (render-singleinvader i img)
  (place-image INVADER
               (invader-x i) (invader-y i)
               img))


;; ==================

;; Game KeyEvent -> Game
;; - left and right arrows control the direction of the tank
;; - space bar fires missiles

;(define (control-tank s ke) s) ;stub

(define (control-tank s ke)
  (cond [(key=? ke "left")  (make-game (game-invaders s) (game-missiles s) (turn-left (game-tank s)))]
        [(key=? ke "right") (make-game (game-invaders s) (game-missiles s) (turn-right (game-tank s)))]
        [(key=? ke " ")     (make-game (game-invaders s) (fire-missile (game-missiles s) (tank-x (game-tank s))) (game-tank s))]
        [else s]))


;; Tank -> Tank
;; change direction to negative (to go left)

(check-expect (turn-left (make-tank 20 1))    ; tank was going right, now going left
              (make-tank 20 -1))  

(check-expect (turn-left (make-tank 20 -1))   ; tank was already going left
              (make-tank 20 -1)) 

;(define (turn-left t) t) ;stub

(define (turn-left t)
  (if (= (tank-dir t) -1)
      (make-tank (tank-x t) (tank-dir t))
      (make-tank (tank-x t) -1)))


;; Tank -> Tank
;; change direction to positive (to go right)

(check-expect (turn-right (make-tank 20 -1))  ; tank was going left, now going right
              (make-tank 20 1))  
(check-expect (turn-right (make-tank 20 1))   ; tank was already going right
              (make-tank 20 1))  

;(define (turn-right t) t) ;stub

(define (turn-right t)
  (if (= (tank-dir t) 1)
      (make-tank (tank-x t) (tank-dir t))
      (make-tank (tank-x t) 1)))


;; ListOfMissile Natural -> Missile
;; fires a missile

(check-expect (fire-missile empty 150)                                          ; makes missile with x coordinate and constant y coordinate (firing from tank)
              (cons (make-missile 150 (- HEIGHT TANK-HEIGHT/2)) empty))

;(define (fire-missile lom x) lom) ;stub

(define (fire-missile lom x)
  (cond [(empty? lom) (cons (make-missile x (- HEIGHT TANK-HEIGHT/2)) empty)]
        [else
         (cons  (make-missile x (- HEIGHT TANK-HEIGHT/2)) lom)]))