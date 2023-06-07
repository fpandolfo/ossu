;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 4)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

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

(define TANK-Y-POSITION (- HEIGHT TANK-HEIGHT/2))
(define MIN-X-TANK (+ 0 (/ (image-width TANK) 2)))
(define MAX-X-TANK (- WIDTH (/ (image-width TANK) 2)))

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

;; ====== Tank ======

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

;; ====== Invader ======

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


;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI1 empty)
(define LOI2 (cons (make-invader 100 200 -10) empty)) ;1 invader (100, 200) position, moving left
(define LOI3 (list (make-invader 150 100 12) (make-invader 150 HEIGHT -10))) ;first invader (150, 100) position, moving right / second invader (150, HEIGHT) position, exctly landed, moving left

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; Template rules used:
;; one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Invader ListOfInvader)
;; - reference: (first loi) is Invader
;; - self-reference: (rest loi) is ListOfInvader


;; ====== Missile ======

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. a list of missiles

(define LOM1 empty)
(define LOM2 (cons (make-missile 150 300) empty))
(define LOM3 (list (make-missile 150 300) (make-missile (invader-x I1) (+ (invader-y I1) 10))))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template rules used:
;; one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Missile ListOfMissile)
;; - reference: (first lom) is Missile
;; - self-reference: (rest lom) is ListOfMissile


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; =================
;; Functions:

;; Game -> Game
;; start the world with initial state Game
(define (main c)
  (big-bang c                   ; Game
    (on-tick   tick-game)       ; Game -> Game
    (to-draw   render-game)     ; Game -> Image
    (on-key    handle-key)      ; Game KeyEvent -> Game  
    (stop-when has-landed?)))   ; Game -> Boolean    

;; Game -> Game
;; tick the next game state, calculating all its components coordinates position
;;!! Have to finish the Invader, i think i will need to do a separated collision function to operate over invaders and missiles

(define (tick-game s)
  (make-game (generate-loi (game-invaders s)) (tick-lom (game-missiles s)) (tick-tank (game-tank s))))

;; Game KeyEvent -> Game
;; handle all keys to produce next gamestate

(define (handle-key s ke)
  (make-game (game-invaders s) (fire-missile (game-missiles s) (game-tank s) ke) (tank-direction (game-tank s) ke)))


;; Game -> Image
;; produce the image of game state, for every Invader in ListOfInvader, Missile in ListOfMissile and Tank
;;!! Need to finish Invader
(check-expect (render-game (make-game LOI1 LOM1 (make-tank 100 0))) (place-image TANK 100 TANK-Y-POSITION BACKGROUND))
(check-expect (render-game (make-game LOI2 LOM2 (make-tank 100 0))) (place-image INVADER 100 200
                                                                                 (place-image MISSILE 150 300
                                                                                              (place-image TANK 100 TANK-Y-POSITION BACKGROUND))))
;<Template from Game>
(define (render-game s)
  (render-loi (game-invaders s) (render-lom (game-missiles s) (render-tank (game-tank s)))))

;; Tank -> Tank
;; consumes a tank (make-tank x dir) at x coordinates and produce a tank at new x coordinate, based on its direction
(check-expect (tick-tank (make-tank 50  0)) (make-tank 50 0)) ;not moving
(check-expect (tick-tank (make-tank 50  1)) (make-tank (+ 50 TANK-SPEED)  1)) ;not moving
(check-expect (tick-tank (make-tank 50 -1)) (make-tank (- 50 TANK-SPEED) -1)) ;not moving

;(define (tick-tank t) (make-tank 50 0));stub

(define (tick-tank t)
  (make-tank (next-x-tank t) (tank-dir t)))


;; Tank -> Image
;; Consumes a tank and produce its image, based on its x position in the moment
(check-expect (render-tank (make-tank 50 0)) (place-image TANK 50 TANK-Y-POSITION BACKGROUND))

(define (render-tank t)
  (place-image TANK
               (tank-x t)
               TANK-Y-POSITION
               BACKGROUND))

;; Tank -> Integer[0, WIDTH]
;; consumes a Tank and produce its next x coordinate by TANK-SPEED, based on its direction, where x[0, WIDHT]
(check-expect (next-x-tank (make-tank 50  0)) 50) ;not moving, returns the its own x
(check-expect (next-x-tank (make-tank 50  1)) (+ 50 TANK-SPEED)) ;moving right by SPEED pixels per tick
(check-expect (next-x-tank (make-tank 50 -1)) (- 50 TANK-SPEED)) ;moving left by SPEED pixels per tick

;(define (position-of-tank t) 50);stub

;<Template from Tank>
(define (next-x-tank t)
  (cond [(= (tank-dir t) 0) (tank-x t)]             ;not moving
        [(and (> (tank-dir t) 0) (can-tank-move? t)) (+ (tank-x t) TANK-SPEED)]   ;moving right
        [(and (< (tank-dir t) 0) (can-tank-move? t)) (- (tank-x t) TANK-SPEED)]   ;moving left
        [(false? (can-tank-move? t)) (tank-x t)])) 

;; Tank KeyEvent -> Tank
;; returns a tank with tank-dir, depending on the key pressed
;; -1: means going left
;;  0: means its not moving
;;  1: means going right
(check-expect (tank-direction (make-tank 50 0) " ")  (make-tank 50 0)) ;not moving
(check-expect (tank-direction (make-tank 50 0) "right") (make-tank 50 1)) ;moving right
(check-expect (tank-direction (make-tank 50 0)  "left") (make-tank 50 -1)) ;moving left

;(define (tank-direction t ke) 0);stub

(define (tank-direction t ke)
  (cond [(key=? ke "right") (make-tank (tank-x t) 1)]
        [(key=? ke "left")  (make-tank (tank-x t) -1)]
        [else
         (make-tank (tank-x t) (tank-dir t))]))

;; Tank -> Boolean
;; return true if next tank x coordinate is between 0 and WIDHT (inside screen), if not return false 
(check-expect (can-tank-move? (make-tank MIN-X-TANK  0))  true) ;tank at x 0 position, no moving
(check-expect (can-tank-move? (make-tank MIN-X-TANK -1)) false) ;tank next x position is < 0, so cant go left
(check-expect (can-tank-move? (make-tank MIN-X-TANK  1))  true) ;tank next x position is > 0, so can go right
(check-expect (can-tank-move? (make-tank 100  -1)) true) ;tank next x position is > 0, so can go left
(check-expect (can-tank-move? (make-tank 100   1)) true) ;tank next x position is > 0, so can go right
(check-expect (can-tank-move? (make-tank MAX-X-TANK -1)) true) ;tank next x position is < WIDTH, so can go left
(check-expect (can-tank-move? (make-tank MAX-X-TANK  1)) false);tank next x position is > WIDTH, so cant go right

;(define (can-tank-move? t) true);stub

(define (can-tank-move? t)
  (cond [(and (> (+ (tank-x t) TANK-SPEED) MAX-X-TANK)
              (= (tank-dir t)  1))
         false] ;can move right?
        [(and (< (- (tank-x t) TANK-SPEED) MIN-X-TANK)
              (= (tank-dir t) -1))
         false] ;can move left?
        [else
         true]));else true   

;; ListOfMissile Tank KeyEvent -> ListOfMissile
;; Consumes a list of missiles and add a new missile to the list, at tank-x and (- HEIGHT (image-height tank)) position position 
(check-expect (fire-missile empty (make-tank 50 0) "a") empty) ;basecase
(check-expect (fire-missile empty (make-tank 50 0) " ") (cons (make-missile 50 (- HEIGHT (image-height TANK))) empty))
(check-expect (fire-missile (cons (make-missile 70 40) empty) (make-tank 50 0) " ") (cons (make-missile 50 (- HEIGHT (image-height TANK))) (cons (make-missile 70 40) empty)))

;(define (fire-missile lom t ke) empty);stub

(define (fire-missile lom t ke)
  (cond [(and (empty? lom) (key=? ke " ")) (cons (make-missile (tank-x t) (- HEIGHT (image-height TANK))) empty)]
        [(and (not (empty? lom)) (key=? ke " ")) (cons (make-missile (tank-x t) (- HEIGHT (image-height TANK))) (cons (first lom) (rest lom)))]
        [else
         lom]))

;; ListOfMissiles Tank -> Image
;; Consumes a list of missiles and a tank, produce a image for each missile in the list
;; when the list is empty call for render tank image
(check-expect (render-lom (cons (make-missile 100 100) empty) (place-image TANK
                                                                           50 TANK-Y-POSITION
                                                                           BACKGROUND)) (place-image MISSILE
                                                                                                     100 100
                                                                                                     (place-image TANK
                                                                                                                  50 TANK-Y-POSITION
                                                                                                                  BACKGROUND)))
;<Template form ListOfMissiles>
(define (render-lom lom image)
  (cond [(empty? lom) image]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-lom (rest lom) image))]))

;; ListOfMissiles -> ListOfMissiles
;; produce a list of ticked missiles, by y position minus MISSILE-SPEED, from down to up the screen
;; if the missile reaches the top of the screen, its removed from the list
(check-expect (tick-lom empty) empty) ;basecase
(check-expect (tick-lom (cons (make-missile 50 10) empty)) empty)
(check-expect (tick-lom (cons (make-missile 50  0) empty)) empty)
(check-expect (tick-lom (cons (make-missile 50 11) empty)) (cons (make-missile 50 (- 11 MISSILE-SPEED)) empty))

;(define (tick-lom lom) empty);stub

;<Template from ListOfMissiles>
(define (tick-lom lom)
  (cond [(empty? lom) empty]
        [else
         (filtered-missiles (cons (next-y-missile (first lom))
                                  (tick-lom (rest lom))))]))

;; Missile -> Missile
;; consume a missile at x and y position, and produce a new missile with same x position and y - MISSILE-SPEED position
(check-expect (next-y-missile (make-missile 50 20)) (make-missile 50 (- 20 MISSILE-SPEED)))
(check-expect (next-y-missile (make-missile 50 40)) (make-missile 50 (- 40 MISSILE-SPEED)))


;(define (next-y-missile m) (make-missile 50 10));stub

;<Template from Missile>
(define (next-y-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ListOfMissiles -> ListOfMissiles
;; Consume a list of missiles and remove the ones that reaches the top border of the screen
(check-expect (filtered-missiles empty) empty)
(check-expect (filtered-missiles (cons (make-missile 50 10) empty)) (cons (make-missile 50 10) empty))
(check-expect (filtered-missiles (cons (make-missile 50 0) empty)) empty)
(check-expect (filtered-missiles (cons (make-missile 50 10) (cons (make-missile 50  0) empty))) (cons (make-missile 50 10) empty))
(check-expect (filtered-missiles (cons (make-missile 50 10) (cons (make-missile 50 20) empty))) (cons (make-missile 50 10) (cons (make-missile 50 20) empty)))

;(define (filtered-missiles lom) empty);stub

;<Template from ListOfMissiles>
(define (filtered-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (<= (missile-y (first lom)) 0)
             (filtered-missiles (rest lom))
             (cons (first lom) (filtered-missiles (rest lom))))]))


;; ListOfMissiles -> Image
;; consume a list of missiles and produce a image of each one
(check-expect (render-missile empty) BACKGROUND)
(check-expect (render-missile (cons (make-missile 50 100) empty)) (place-image MISSILE 50 100 BACKGROUND))
(check-expect (render-missile (cons (make-missile 50 100) (cons (make-missile 20 150) empty))) (place-image MISSILE 50 100
                                                                                                            (place-image MISSILE 20 150 BACKGROUND)))

;<Template from ListOfMissile>
(define (render-missile lom)
  (cond [(empty? lom) BACKGROUND]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom))
                      (render-missile (rest lom)))]))

;; ListOfInvaders Boolean -> ListOfInvaders
;; add a new invader to the list of invaders case random-generate true, based on INVADE-RATE, if false return the original list of invader
(define (generate-loi loi)
  (cond [(spawn? INVADE-RATE) (spawn-invader loi)]
        [else
         (tick-loi loi)]))

;; ListOfInvaders Image -> Image
;; consume a list of invaders and a image
;; produce image of each list of invader in the list
;; when the list is empty, produce all invaders image with Image
(check-expect (render-loi empty (place-image TANK
                                             50 TANK-Y-POSITION
                                             BACKGROUND)) (place-image TANK
                                                                       50 TANK-Y-POSITION
                                                                       BACKGROUND)) 
(check-expect (render-loi (list (make-invader 100 100 -10)) (place-image TANK
                                                                         50 TANK-Y-POSITION
                                                                         BACKGROUND)) (place-image INVADER
                                                                                                   100 100
                                                                                                   (place-image TANK
                                                                                                                50 TANK-Y-POSITION
                                                                                                                BACKGROUND)))
(check-expect (render-loi (list (make-invader 100 100 -10)) (place-image MISSILE
                                                                         100 100
                                                                         (place-image TANK
                                                                                      50 TANK-Y-POSITION
                                                                                      BACKGROUND))) (place-image INVADER
                                                                                                                 100 100
                                                                                                                 (place-image MISSILE
                                                                                                                              100 100
                                                                                                                              (place-image TANK
                                                                                                                                           50 TANK-Y-POSITION
                                                                                                                                           BACKGROUND))))
(check-expect (render-loi (list (make-invader 200 200 -10)) (place-image INVADER
                                                                         100 100
                                                                         (place-image TANK
                                                                                      50 TANK-Y-POSITION
                                                                                      BACKGROUND))) (place-image INVADER
                                                                                                                 200 200
                                                                                                                 (place-image INVADER
                                                                                                                              100 100
                                                                                                                              (place-image TANK
                                                                                                                                           50 TANK-Y-POSITION
                                                                                                                                           BACKGROUND))))

;<Template from Invaders>
(define (render-loi loi image)
  (cond [(empty? loi) image]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-loi (rest loi) image))]))

;; ListOfInvaders Timer -> ListOfInvaders
;; consumes a list of invaders and tick every invader-x position by INVADER-X-SPEED and invader-y position by INVADER-Y-SPEED in the list
;; and produce a new invader every INVADE-RATE seconds
(check-expect (tick-loi  empty) empty);basecase
(check-expect (tick-loi (list (make-invader 200 150 1))) (list (make-invader (+ 200 INVADER-X-SPEED) (+ 150 INVADER-Y-SPEED) 1)))
(check-expect (tick-loi (list (make-invader 200 150 1) (make-invader 100 200 -1))) (list (make-invader (+ 200 INVADER-X-SPEED) (+ 150 INVADER-Y-SPEED) 1)
                                                                                         (make-invader (- 100 INVADER-X-SPEED) (+ 200 INVADER-X-SPEED) -1)))
(check-expect (tick-loi (list (make-invader 200 150 1) (make-invader 0 200 -1))) (list (make-invader (+ 200 INVADER-X-SPEED) (+ 150 INVADER-Y-SPEED) 1)         ; going left and touchs the border
                                                                                       (make-invader (+ 0 INVADER-X-SPEED) (+ 200 INVADER-X-SPEED) 1)))
(check-expect (tick-loi (list (make-invader 200 150 1) (make-invader WIDTH 200 1))) (list (make-invader (+ 200 INVADER-X-SPEED) (+ 150 INVADER-Y-SPEED) 1)      ; going right and touchs the border  
                                                                                          (make-invader (- WIDTH INVADER-X-SPEED) (+ 200 INVADER-X-SPEED) -1)))

;(define (tick-loi loi) empty);stub

;<Template from ListOfInvaders>
(define (tick-loi loi)
  (cond [(empty? loi) empty]
        [else
         (cons (tick-invader (first loi))
               (tick-loi (rest loi)))]))

;; Invader -> Invader
;; consume a invader at x and y position, returns it ticket invader-x position by INVADER-X-SPEED and invader-y position by INVADER-Y-SPEED
(check-expect (tick-invader (make-invader 100 100 1))     (make-invader (+ 100 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1))
(check-expect (tick-invader (make-invader 0  100 -1))     (make-invader (+ 0 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1))      ; going left and touchs the border
(check-expect (tick-invader (make-invader WIDTH  100 1))  (make-invader (- WIDTH INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) -1)) ; going right and touchs the border

;(define (tick-invader invader) (make-invader 100 100 1));stub

;<Template from Invader>
(define (tick-invader invader)
  (cond [(and (<= (invader-x invader) 0 ) (= (invader-dx invader) -1)) (make-invader (+ (invader-x invader) INVADER-X-SPEED) (+ (invader-y invader) INVADER-Y-SPEED) 1)]
        [(and (>= (invader-x invader) WIDTH ) (= (invader-dx invader) 1)) (make-invader (- (invader-x invader) INVADER-X-SPEED) (+ (invader-y invader) INVADER-Y-SPEED) -1)]
        [else
         (make-invader (+ (invader-x invader) (* (invader-dx invader) INVADER-X-SPEED)) (+ (invader-y invader) INVADER-Y-SPEED) (invader-dx invader))]))

;; ListOfInvader -> ListOfInvader
;; Consume a list of invaders and remove the ones that reaches the bottom border of the screen
(check-expect (filtered-invaders empty) empty);basecase
(check-expect (filtered-invaders (cons (make-invader 100 100 1) empty)) (cons (make-invader 100 100 1) empty))
(check-expect (filtered-invaders (cons (make-invader 100 100 1) (cons (make-invader 100 HEIGHT 1) empty))) (cons (make-invader 100 100 1) empty))
(check-expect (filtered-invaders (cons (make-invader 100 HEIGHT 1) (cons (make-invader 100 100 -1) empty))) (cons (make-invader 100 100 -1) empty))

;;(define (filtered-invaders loi) empty);stub

;;<Template from ListOfInvaders>
(define (filtered-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             (filtered-invaders (rest loi))
             (cons (first loi) (filtered-invaders (rest loi))))]))

;; Game -> Boolean
;; produce true case a invader from list of invaders reach the bottom of the screen
(check-expect (has-landed? (make-game empty empty (make-tank 50 0))) false)
(check-expect (has-landed? (make-game (cons (make-invader 100 230 -1) empty) empty (make-tank 50 0))) false)
(check-expect (has-landed? (make-game (cons (make-invader 100 HEIGHT -1) empty) empty (make-tank 50 0))) true)
(check-expect (has-landed? (make-game (cons (make-invader 100 230 -1) (cons (make-invader 100 HEIGHT -1) empty)) empty (make-tank 50 0))) true)

;(define (has-landed? loi) false);

;<Template from ListOfInvaders>
(define (has-landed? s)
  (cond [(empty? (game-invaders s)) false]
        [(>= (invader-y (first (game-invaders s))) HEIGHT ) true]
        [else
         (has-landed? (make-game (rest (game-invaders s)) (game-missiles s) (game-tank s)))]))

;; ListOfInvaders Timer -> ListOfInvaders
;; consumes a list of invaders and produce a new invader at the beginning of the list, at a random invader-x[0, WIDTH] and random invader-dx[-1, 1] direction (-1 or 1)
(check-random (spawn-invader empty) (cons (make-invader (random WIDTH) 0 (random-one-or-minus-one 2)) empty))  ;spawn an invader going left, at 127 x
(check-random (spawn-invader (cons (make-invader 237 120 -1) empty)) (cons (make-invader (random WIDTH) 0 (random-one-or-minus-one 2))
                                                                           (cons (make-invader 237 120 -1) empty)))  ;spawn an invader going right, at 15 x

;(define (spawn-invader loi) empty);stub

;<Template from ListOfInvaders>
(define (spawn-invader loi)
  (cond [(empty? loi) (cons (make-invader (random WIDTH) 0 (random-one-or-minus-one 2)) empty)]
        [else
         (cons (make-invader (random WIDTH) 0 (random-one-or-minus-one 2))
               (cons (first loi)
                     (rest loi)))]))

;; Number[0, 2( -> Number[1, -1]
;; consumes a random number between 0 (inclusive) and 2 (exclusive), and produce:
;; 1 case >= 0.5
;; -1 case < 0.5

(define (random-one-or-minus-one x)
  (if (< (random x) 1)
      -1
      1))

;; Number[0, 10( -> Boolean
;; consumes a random number between 0 (inclusive) and 10 (exclusive), and produce:
;; true: case >= 5
;; false: case < 5

(define (spawn? invade-rate)
  (< (random invade-rate) 2))

;; Game -> Game
;; consumes a list of invaders and a list of missiles from Game, check it's invaders x and y position, and missile x and y position
;; if the positions match by HIT-RANGE distance, then remove invader from list of invaders
;; and remove missile from list of missiles
(check-expect (has-collided (make-game empty empty (make-tank 50 0))) (make-game empty empty (make-tank 50 0)))
(check-expect (has-collided (make-game (cons (make-invader 100 150 -1) empty)
                                       (cons (make-missile 100 50) empty)
                                       (make-tank 50 0)))
              (make-game (cons (make-invader 100 150 -1) empty)
                         (cons (make-missile 100 50) empty)
                         (make-tank 50 0)))

(check-expect (has-collided (make-game (cons (make-invader 100 150 -1) (cons (make-invader 200 270 1) empty))
                                       (cons (make-missile 100 50) (cons (make-missile 200 270) empty))
                                       (make-tank 50 0)))
              (make-game (cons (make-invader 100 150 -1) empty)
                         (cons (make-missile 100 50) empty)
                         (make-tank 50 0)))

(check-expect (has-collided (make-game (cons (make-invader 100 150 -1) (cons (make-invader 300 270 1) empty))
                                       (cons (make-missile 100 150) (cons (make-missile 200 270) empty))
                                       (make-tank 50 0)))
              (make-game (cons (make-invader 300 270 1) empty)
                         (cons (make-missile 200 270) empty)
                         (make-tank 50 0)))
(check-expect (has-collided (make-game (cons (make-invader 100 150 -1) (cons (make-invader 300 270 1) (cons (make-invader 50 160 1) empty)))
                                       (cons (make-missile 50 160) (cons (make-missile 200 270) empty))
                                       (make-tank 50 0)))
              (make-game (cons (make-invader 100 150 -1) (cons (make-invader 300 270 1) empty))
                         (cons (make-missile 200 270) empty)
                         (make-tank 50 0)))

;(define (has-collided g) (make-game empty empty (make-tank 50 0)));stub

;<Template from Game>
#;
(define (has-collided s)
  (cond [(or (empty? (game-invaders s)) (empty? (game-missiles s))) s]
        [(and (= (invader-x (first (game-invaders s))) (missile-x (first (game-missiles s))))
              (= (invader-y (first (game-invaders s))) (missile-y (first (game-missiles s)))))
         (make-game (rest (game-invaders s)) (rest (game-missiles s)) (game-tank s))]
        [(and (= (invader-x (first (game-invaders (has-collided s)))) (first (missile-x (game-missiles (has-collided s)))))
              (= (invader-y (first (game-invaders (has-collided s)))) (first (missile-y (game-missiles (has-collided s))))))
         (make-game (rest (game-invaders s)) (rest (game-missiles s)) (game-tank s))]))