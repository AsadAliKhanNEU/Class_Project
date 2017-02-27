
;ASAD KHAN
;;=============Constants============
(define GRID-WIDTH 25)
(define GRID-HEIGHT 40)
(define CELL-SIZE 15)
(define BG (empty-scene (* CELL-SIZE GRID-WIDTH) (* CELL-SIZE GRID-HEIGHT)))
 
(define PLAYER (square CELL-SIZE 'solid 'black))
(define PLAYER-Y ( - (* GRID-HEIGHT CELL-SIZE) (/ CELL-SIZE 2)))
(define BULLET (rectangle 3 8 'solid 'orange))
(define CENTIPEDE-CELL (square CELL-SIZE 'solid 'green))
(define TONGUE (triangle 5 'solid 'red))
(define LEFT-HEAD (overlay/align "left" "middle" (rotate 90 TONGUE) CENTIPEDE-CELL))
(define RIGHT-HEAD (overlay/align "right" "middle" (rotate 30 TONGUE) CENTIPEDE-CELL))


(define MUSHROOM-RADIUS (/ CELL-SIZE 2))
(define MUSHROOM-1-C 'LightSalmon)
(define MUSHROOM-2-C 'Salmon)
(define MUSHROOM-3-C 'OrangeRed)
(define MUSHROOM-4-C 'DarkRed)
(define MUSHROOM-5-C 'DarkRed)
(define BULLET-SPEED 30)

(define WINNER (text "WINNER" 72 'black))
(define LOSER  (text "LOSER" 72 'black))
;;=============Data dafinitions===========
; Number Number -> Posn
; Make a Posn at the given grid position.
(define (grid-posn x y)
  (make-posn (* x GRID-WIDTH) (* y GRID-HEIGHT)))
(check-expect (grid-posn 1 1) (make-posn 25 40))

(define-struct player [x dir])

;A player is a struct: (make-struct Number String)
;intrepretation the x-coordinates of the player the direction of the player
#;(define (player-temp p)
    (...(player-x p)...(player-dir p)...))
;A dir is one of:
;-left
;-right
;-space
#;(define (dir-temp d)
    (cond
      [(string=? "left" d)....]
      [(string=? "right" d)...]
      [(string=? "space" d)...]))
(define player-left (make-player 40 "left"))
(define player-right (make-player 50  "right"))
(define player-hit-left (make-player 7.5 "left"))
(define player-hit-right (make-player 367.5  "right"))
(define player0 (make-player 15 "space"))

  
(define-struct bullet [posn fire])
;A bullet is a strcut: (make-bullet Posn Boolean)
;interpretation the coordinate of the bullet true for firing the bullet false for not firing the bullet
#;(define (bullet-temp b)
    (...(bullet-posn b)....(bullet-fire b)...))
(define bullet- (make-bullet (make-posn 10 10) true))
(define bullet-reload (make-bullet (make-posn 10 -7.5) true))

(define-struct centi [segs dir])
;A centi is a strcut: (make-centi los String)
;interpreation: the list of segements the direction of the centipede 
#;(define (centi-temp c)
    (...(centi-segs c)...(centi-dir c)...))
;A los is one of:
;-empty
;-(cons posn los)
#;(define (los-temp los)
    (cond
      [(empty? los)...]
      [(cons? los)...(first los)...
                  (los-temp (rest los))...]))
;A dir is one of:
;-left
;-right
;-down
#;(define (dir-temp d)
    (cond
      [(string=? "left" d)....]
      [(string=? "right" d)...]
      [(string=? "down" d)]))
(define centi-left (make-centi (list (make-posn 30 40)
                                     (make-posn 40 40)) "left"))
(define centi-right (make-centi (list (make-posn 40 40)
                                      (make-posn 30 40)) "right"))
(define centi-hit-left (make-centi (list (make-posn 7.5 40)
                                         (make-posn 22.5 40)
                                         (make-posn 37.5 40)) "left"))
(define centi-hit-right (make-centi (list (make-posn 367.5 40)
                                          (make-posn 352.5 40)
                                          (make-posn 337.5 40)) "right"))

;A mushroom is a strcut: (make-bullet Posn number)
;interpretation the coordinate of the mushroom number is the hp telling us which image to use
(define-struct mushroom [posn color])
#;(define (mushroom-temp m)
    (...(mushroom-posn m)....(mushroom-hp m)...))
(define mushroom- (make-mushroom (make-posn 10 10) 'DarkRed))
(define mushroom-dead (make-mushroom (make-posn 0 1000) 0))


(define-struct world [centi player bullet mushroom])
;A world is a struct: (make-world centi player bullet)
;interpreation: the centipede the player the bullet
#;(define (world-temp w)
    (...(world-centi w)...(world-player w)...(world-bullet w)...(world-mushroom w))) 
;(define world-left (make-world centi-left player-left bullet-))
;(define world-right (make-world centi-right player-right bullet-reload))

;============big bang============
;world --> world
;to lunch the program 
(define (main centi-length)
  (big-bang (convert-world centi-length)
            (on-tick   world->world 0.1)
            (on-key    player-controls)
            (to-draw   world->scene)
            (stop-when end? result)))
(define MUSHROOMS (list(make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)
                       (make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)(make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)
                       (make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)(make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)
                       (make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)
                       (make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)
                       (make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)
                       (make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)
                       (make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)
                       (make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)
                       (make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)
                       (make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)
                       (make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)
                       (make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)
                       (make-mushroom (make-posn (* (/ CELL-SIZE 2) (random 50)) (+  (/ CELL-SIZE 2) (* 15 (random 39)))) MUSHROOM-4-C)
                       ))
; World -> Mush
; Adds a mushroom to the list of mushroom where the bullet hits the centi
(define (new-mush w)
  (list*
 (make-mushroom (make-posn (posn-x (bullet-posn (world-bullet w)))
                           (posn-y (bullet-posn (world-bullet w)))) MUSHROOM-5-C)
 (world-mushroom w)))

;; Number -> World
;; Takes in the initial length of a centipede and gives us an initial world with that length in segs
(define (convert-world centi-length)
  (make-world
   (make-centi (make-list centi-length (make-posn (/ CELL-SIZE 2) (+ 0 (/ CELL-SIZE 2)))) "right")
   (make-player
               (/ (* GRID-WIDTH CELL-SIZE) 2)
               "left")
   (make-bullet (make-posn (/ (* GRID-WIDTH CELL-SIZE) 2)
                                       PLAYER-Y)
                                       #false)
   MUSHROOMS))



         #;(check-expect (convert-world 5) (make-world
 (make-centi (list (make-posn 7.5 7.5) (make-posn 7.5 7.5) (make-posn 7.5 7.5) (make-posn 7.5 7.5) (make-posn 7.5 7.5)) "right")
 (make-player 187.5 "left")
 (make-bullet (make-posn 187.5 592.5) #false))) 

;world --> world
;take a world and return the next world
(define (world->world w)
    (cond [(hit-mush? (world-mushroom w) (bullet-posn (world-bullet w)))
           (make-world
         (centi-slither (mush-hit w))
         (return-player (world-player w))
         (make-bullet (make-posn (player-x (world-player w)) PLAYER-Y) false)
         (hurt-mush (world-mushroom w) (world-bullet w)))]
      [(and (hit-centi? (centi-segs (world-centi w)) (bullet-posn (world-bullet w)))
             (boolean=? (bullet-fire (world-bullet w)) true))
        (make-world
         (centi-slither (centi-drop w))
         (return-player (world-player w))
         (world-bullet (reload w))
         (new-mush w))]
        [else(make-world 
          (centi-slither (mush-hit w))
         (return-player (world-player w))
         (if (boolean=? (bullet-fire (world-bullet w)) true)
             (world-bullet (reload (fire-bullet w)))
             (make-bullet (make-posn (player-x (world-player w)) PLAYER-Y) (bullet-fire (world-bullet w))))
         (world-mushroom w))]))
;hit-mush?
;has the bullet hit any mushroom
; lom posn -> boolean
(define (hit-mush? los B)
  (cond [(empty? los) false]
        [else (or (hitm! (first los)  B)
                   (hit-mush? (rest los) B))]))
(define (hit-mush?1 los B)
  (cond [(empty? los) false]
        [else (or (hitm!1 (first los)  B)
                   (hit-mush?1 (rest los) B))]))
(check-expect (hit-mush? (list (make-mushroom (make-posn 200 200) 'DarkRed)
                               (make-mushroom (make-posn 100 400) 'DarkRed)
                               (make-mushroom (make-posn 400 2) 'DarkRed)
                               (make-mushroom (make-posn 500 100) 'DarkRed))
                         (make-posn 200 195)) true)
(check-expect (hit-mush? (list (make-mushroom (make-posn 200 200) 'DarkRed)
                               (make-mushroom (make-posn 100 400) 'DarkRed)
                               (make-mushroom (make-posn 100 200) 'DarkRed)
                               (make-mushroom (make-posn 500 100) 'DarkRed))
                         (make-posn 100 195)) true)
(check-expect (hit-mush? (list (make-mushroom (make-posn 200 200) 'DarkRed)
                               (make-mushroom (make-posn 100 400) 'DarkRed)
                               (make-mushroom (make-posn 400 2) 'DarkRed)
                               (make-mushroom (make-posn 400 10) 'DarkRed))
                         (make-posn 400 5)) true)

;mush posn -> boolean
(define (hitm! M B)
  (if (and
       (>= (- (posn-x B) 1.5) (- (posn-x (mushroom-posn M)) CELL-SIZE))
       (<=  (+ (posn-x B) 1.5) (+ (posn-x (mushroom-posn M)) CELL-SIZE))
      (>= (+ (posn-y B) CELL-SIZE) (- (posn-y (mushroom-posn M)) CELL-SIZE))
      (<=  (-  (posn-y B) CELL-SIZE) (+ (posn-y (mushroom-posn M)) CELL-SIZE))) 
           true false))
(check-expect (hitm! (make-mushroom (make-posn 200 200) 'DarkRed) (make-posn 200 195)) true)
(check-expect (hitm! (make-mushroom (make-posn 100 200) 'DarkRed) (make-posn 100 195)) true)
(check-expect (hitm! (make-mushroom (make-posn 400 10) 'DarkRed) (make-posn 400 5)) true)


;mush posn -> boolean
;  checks if bullet hit a mushroom
(define (hitm!1 M B)
  (if (and
       (>= (+ (posn-x B) (/ CELL-SIZE 2)) (- (posn-x (mushroom-posn M)) (/ CELL-SIZE 2)))
       (<=  (- (posn-x B) (/ CELL-SIZE 2)) (+ (posn-x (mushroom-posn M)) (/ CELL-SIZE 2)))
      (>= (- (posn-y B) (/ CELL-SIZE 2)) (- (posn-y (mushroom-posn M)) (/ CELL-SIZE 2)))
      (<=  (+  (posn-y B) (/ CELL-SIZE 2)) (+ (posn-y (mushroom-posn M)) (/ CELL-SIZE 2)))) 
           true false))
(check-expect (hitm! (make-mushroom (make-posn 200 200) 'DarkRed) (make-posn 200 195)) true)
(check-expect (hitm! (make-mushroom (make-posn 100 200) 'DarkRed) (make-posn 100 195)) true)
(check-expect (hitm! (make-mushroom (make-posn 400 10) 'DarkRed) (make-posn 400 5)) true)

;hit-hp
;world -> mushroom
;change the color of the centi which is hit
;(define (hit-hp w)
; (make-mushroom (hurt-mush (world-mushroom w) (world-bullet w))))

;; lom bullet -> mushroom
;; hurt mushrooms which are hit
(define (hurt-mush m b)
 (cond [(empty? m) empty]
        [else (cons (lower-hp (first  m) (bullet-posn b))
              (hurt-mush (rest  m) b))]))

;; mushroom posn -> mushroom
;; change mushroom color if hit and changed colors
(define (lower-hp mush bullet)
  (cond 
       [(and (symbol=? (mushroom-color mush) MUSHROOM-4-C)(hitm! mush bullet))        
        (make-mushroom (make-posn (posn-x (mushroom-posn mush))
                                  (posn-y (mushroom-posn mush)))
                       MUSHROOM-3-C)]
       [(and (symbol=? (mushroom-color mush) MUSHROOM-3-C)(hitm! mush bullet))        
        (make-mushroom (make-posn (posn-x (mushroom-posn mush))
                                  (posn-y (mushroom-posn mush)))
                       MUSHROOM-2-C)]
       [(and (symbol=? (mushroom-color mush) MUSHROOM-2-C)(hitm! mush bullet))        
        (make-mushroom (make-posn (posn-x (mushroom-posn mush))
                                  (posn-y (mushroom-posn mush)))
                       MUSHROOM-1-C)]
       [(and (symbol=? (mushroom-color mush) MUSHROOM-1-C)(hitm! mush bullet))        
        (make-mushroom (make-posn 0
                                  10000)
                       MUSHROOM-1-C)]
       
       [(or (symbol=? (mushroom-color mush) MUSHROOM-3-C)(hitm! mush bullet))        
        (make-mushroom (make-posn (posn-x (mushroom-posn mush))
                                  (posn-y (mushroom-posn mush)))
                       MUSHROOM-3-C)]
       [(or (symbol=? (mushroom-color mush) MUSHROOM-2-C)(hitm! mush bullet))        
        (make-mushroom (make-posn (posn-x (mushroom-posn mush))
                                  (posn-y (mushroom-posn mush)))
                       MUSHROOM-2-C)]
       [(or (symbol=? (mushroom-color mush) MUSHROOM-1-C)(hitm! mush bullet))        
        (make-mushroom (make-posn (posn-x (mushroom-posn mush))
                                  (posn-y (mushroom-posn mush)))
                       MUSHROOM-1-C)]
       [else (make-mushroom (make-posn (posn-x (mushroom-posn mush))
                                       (posn-y (mushroom-posn mush)))
                            (mushroom-color mush))]))


;centi --> centi
;return injured centi if its been hit by bullet before
;(define (injured? w)
 ; (if (hurt? (centi-segs (world-centi w))) (centi-drop w)
  ;    (world-centi w))) 

;lop ->  boolean
;centi hit by bullet?
(define (hurt? lop)
  (cond [(empty? lop) false]
        [(cons? lop) (or (>= (posn-y (first lop)) 1000)
              (hurt? (rest lop)))]
        [else false]))
(check-expect (hurt? (list
   (make-posn 307.5 37.5)
   (make-posn 292.5 37.5)
   (make-posn 277.5 37.5)
   (make-posn 262.5 37.5)
   (make-posn 247.5 37.5)
   (make-posn 232.5 37.5)
   (make-posn 217.5 37.5)
   (make-posn 202.5 37.5)
   (make-posn 0 1000)
   (make-posn 0 1000))) true)
(check-expect (hurt? (list (make-posn 7.5 7.5)
                           (make-posn 200 7.5))) false)

;World -> Image
; Takes in a world and produces and image of it
(define (world->scene w)
     (player+scene (world-player w)
                (mushrooms+scene (world-mushroom w)
                 (bullet+scene (world-bullet w)
                              (head+scene (world-centi w)
                                          (centi+scene (world-centi w)
                                           BG))))))
#;(check-expect (world->scene world0)
              (place-image PLAYER  10 592.5
                           (place-image BULLET 10 10
                                        (place-image-on-grid LEFT-HEAD 10 10
                                                                 (place-image-on-grid LEFT-HEAD 11 10
                                                                  BG))))) 

;world keyevent --> world
;take a keyevent and a world to return a world
(define (player-controls w a-key)
    (cond
      [(key=? a-key "left")
       (make-world (world-centi w)
                   (make-player (- (player-x (world-player w)) CELL-SIZE) a-key)
                   (world-bullet w)
                   (world-mushroom w))]
	     [(key=? a-key "right")
         (make-world (world-centi w)
                     (make-player (+ (player-x (world-player w)) CELL-SIZE) a-key)
                     (world-bullet w)(world-mushroom w))]
             [(key=? a-key " ")
              (make-world (world-centi w)
                          (world-player w)
                          (make-bullet (make-posn (posn-x (bullet-posn (world-bullet w)))
                                                  (posn-y (bullet-posn (world-bullet w)))) true)
                          (world-mushroom w))]
	[else w]))

;world --> boolean
;to determine if the game is end
(define (end? w)
  (or (winner? w)
      (loser? w)))

;world --> image
;to display the final result of the game
(define (result w)
  (cond
    [(winner? w) WINNER]
    [(loser? w) LOSER]))

;============on tick============
;--------normal world----------------
;centi --> centi
;take a centi and return a new centi  
(define (centi-slither s)
  (cond [(and (centi-right-wall-collide? (first (centi-segs s))) (string=? "right" (centi-dir s)))
         (make-centi 
	      (cons (seg-movement (first (centi-segs s)) (centi-dir s))
                    (all-but-last (centi-segs s))) "left")]
        [(and (centi-left-wall-collide? (first (centi-segs s))) (string=? "left" (centi-dir s)))
         (make-centi 
	      (cons (seg-movement (first (centi-segs s)) (centi-dir s))
                    (all-but-last (centi-segs s))) "right")]
        [else (make-centi (cons (seg-movement (first (centi-segs s)) (centi-dir s))
                                (all-but-last (centi-segs s)))
                          (centi-dir s))]))

;world->centi
;take a world and return a new world
(define (mush-hit s)
  (cond [(and (hit-mush?1 (world-mushroom s) (first (centi-segs (world-centi s))) ) (string=? "right" (centi-dir (world-centi s))))
         (make-centi 
	      (cons (seg-movement1 (first (centi-segs (world-centi s))) (centi-dir (world-centi s)) (world-mushroom s))
                    (all-but-last (centi-segs (world-centi s)))) "left")
                     
                     
                     ]
        [(and (hit-mush?1 (world-mushroom s) (first (centi-segs (world-centi s))) ) (string=? "left" (centi-dir (world-centi s))))
         (make-centi 
           (cons (seg-movement1 (first (centi-segs (world-centi s))) (centi-dir (world-centi s)) (world-mushroom s))
                 (all-but-last (centi-segs (world-centi s)))) "right")]
        [else (world-centi s)]))
(check-expect (centi-slither centi-hit-left) (make-centi (list (make-posn 7.5 55)
                                                               (make-posn 7.5 40)
                                                               (make-posn 22.5 40)) "right"))
(check-expect (centi-slither centi-hit-right) (make-centi (list (make-posn 367.5 55)
                                                                (make-posn 367.5 40)
                                                                (make-posn  352.5 40)) "left"))
(check-expect (centi-slither centi-left) (make-centi (list (make-posn 15 40)
                                                             (make-posn 30 40)) "left")) 
(check-expect (centi-slither centi-right) (make-centi (list (make-posn 55 40)
                                                              (make-posn 40 40)) "right"))5

;posn dir--> posn
;move one segment in specific direciton
(define (seg-movement p d)
    (cond
       [(or (and (centi-right-wall-collide? p) (string=? "right" d))
            (and (centi-left-wall-collide? p) (string=? "left" d)))
       (make-posn (posn-x p) (+ (posn-y p) CELL-SIZE))]
       [(string=? "left" d) (make-posn (- (posn-x p) CELL-SIZE) (posn-y p))]
       [(string=? "right" d) (make-posn (+ (posn-x p) CELL-SIZE) (posn-y p))]))
(check-expect (seg-movement (make-posn 367.5 10) "right") (make-posn 367.5 25))
(check-expect (seg-movement(make-posn 7.5 10) "left") (make-posn 7.5 25))
(check-expect (seg-movement (make-posn 20 10) "left") (make-posn 5 10))
(check-expect (seg-movement (make-posn 10 10) "right") (make-posn 25 10))


(define (seg-movement1 p d m)
    (cond
       [(or  (string=? "right" d)
             (string=? "left" d))
       (make-posn (posn-x p) (+ (posn-y p) CELL-SIZE))]
       [(string=? "left" d) (make-posn (- (posn-x p) CELL-SIZE) (posn-y p))]
       [(string=? "right" d) (make-posn (+ (posn-x p) CELL-SIZE) (posn-y p))]))
; los -> los
; drops the last seg
(define (all-but-last seg)
  (cond [(empty? (rest seg)) empty]
        [else (cons (first seg)
		    (all-but-last (rest seg)))]))
(check-expect (all-but-last (list (make-posn 10 10)
                                           (make-posn 20 20)))
              (list (make-posn 10 10)))
(check-expect (all-but-last (list empty)) empty)

;posn -> Boolean
;Has the posn collided with the right wall?
(define (centi-right-wall-collide? p)
     (= (posn-x p)  (- (* GRID-WIDTH CELL-SIZE) (/ CELL-SIZE 2))))
(check-expect (centi-right-wall-collide? (make-posn 367.5 10)) true)
(check-expect (centi-right-wall-collide? (make-posn 367 10)) false)
;posn -> Boolean
;Has the posn collided with the left wall?
(define (centi-left-wall-collide? p)
  (=  (posn-x p) (/ CELL-SIZE 2)))
(check-expect (centi-left-wall-collide? (make-posn 7.5 10)) true)
(check-expect (centi-left-wall-collide? (make-posn 367 10))false)
     
;player -> player
;stops player from leaving world
(define (return-player p)
  (cond [(<=  (player-x p) CELL-SIZE)
         (make-player (/ CELL-SIZE 2) (player-dir p))]
        [(>=  (player-x p) (- (* GRID-WIDTH CELL-SIZE) (/ CELL-SIZE 2)))
         (make-player  (- (* GRID-WIDTH CELL-SIZE) (/ CELL-SIZE 2)) (player-dir p))]
        [else p]))

(check-expect (return-player player-hit-left) (make-player 7.5 "left"))
(check-expect (return-player player-hit-right) (make-player 367.5 "right"))
(check-expect (return-player player-left) (make-player 40 "left"))

;world --> world
;movement of bullet which includes moving upwards and reload
(define (fire-bullet b)
  (make-world (centi-slither (world-centi b))
              (return-player (world-player b))
              (make-bullet (make-posn (posn-x (bullet-posn (world-bullet b)))
                          (- (posn-y (bullet-posn (world-bullet b))) BULLET-SPEED))
                           true)
              (world-mushroom b)))
#;(check-expect (fire-bullet world-left) (make-world
                                        (make-centi (list (make-posn 15 40) (make-posn 30 40)) "left")
                                        (make-player 40 "left")
                                        (make-bullet (make-posn 10 -20) #true)))

;world --> world
;check if the bullet has left the scene and place it back
(define (reload w)
  (cond [(and (= (posn-y (bullet-posn (world-bullet w))) (- 0 (/ CELL-SIZE 2)))
              (boolean=? (bullet-fire (world-bullet w)) true))
             
          (make-world (world-centi w)
                      (world-player w)
                      (make-bullet (make-posn (player-x (world-player w)) PLAYER-Y) false)
                      (world-mushroom w))]
        [else w]))
#;(check-expect (reload world-left) (make-world  (make-centi (list (make-posn 30 40) (make-posn 40 40)) "left") (make-player 40 "left") (make-bullet (make-posn 10 10) #true)))
#;(check-expect (reload world-right) (make-world (make-centi (list (make-posn 40 40) (make-posn 30 40)) "right") (make-player 50 "right") (make-bullet (make-posn 50 592.5) #false)))


;--------------------collision----------------
; posn posn -> boolean
; bullet hit segment?
(define (hit! C B)
  (if (and
       (>= (- (posn-x B) 1.5) (- (posn-x C) (/ CELL-SIZE 2)))
       (<=  (+ (posn-x B) 1.5) (+ (posn-x C) (/ CELL-SIZE 2)))
      (>= (+ (posn-y B) 8) (- (posn-y C) (/ CELL-SIZE 2)))
      (<=  (-  (posn-y B) 8) (+ (posn-y C) (/ CELL-SIZE 2))))
           true false))
(check-expect (hit! (make-posn 100 7.5) (make-posn 100 11.5)) true)
(check-expect (hit! (make-posn 100 7.5) (make-posn 2 11.5)) false)

;los posn -> booolean
;bullet hit centi?
(define (hit-centi? los B)
  (cond [(empty? los) false]
        [else (or (hit! (first los)  B)
                   (hit-centi? (rest los) B))]))
(check-expect (hit-centi? (list (make-posn 2 10)
                                (make-posn 5 10)
                                (make-posn 8 10)
                                (make-posn 10 10))
                          (make-posn 12 17.5)) true)
(check-expect (hit-centi? (list (make-posn 2 10)
                                (make-posn 5 10)
                                (make-posn 8 10)
                                (make-posn 10 10))
                          (make-posn 40 7)) false)
;-------------------drop centi---------------
;; world -> centi 
;; drops the centi which the bullet
(define (centi-drop w)
 (make-centi (drop? (world-centi w) (world-bullet w))
                         (centi-dir(world-centi w))))

#;(check-expect (centi-drop (make-world (make-centi (list (make-posn 7.5 10)
                                                        (make-posn 22.5 10)
                                                        (make-posn 37.5 10)
                                                        (make-posn 52.5 10))
                                                  "right")
                                      (make-player
                                       (/ (* GRID-WIDTH CELL-SIZE) 2)
                                       'left)
                                      (make-bullet (make-posn 12 17.5)
                                                   #false))) (make-centi (list 
                                                                                      (make-posn 0 1000)
                                                                                      (make-posn 22.5 10)
                                                                                      (make-posn 37.5 10)
                                                                                      (make-posn 52.5 10))
                                                                                     "right"))
#;(check-expect (centi-drop (make-world (make-centi (list (make-posn 7.5 10)
                                                        (make-posn 22.5 10)
                                                        (make-posn 37.5 10)
                                                        (make-posn 52.5 10))
                                                  "left")
                                      (make-player
                                       (/ (* GRID-WIDTH CELL-SIZE) 2)
                                       'left)
                                      (make-bullet (make-posn 22.5 10)
                                                   #false))) (make-centi (list (make-posn 7.5 10)
                                                                                           (make-posn 0 1000)
                                                                                           (make-posn 0 1000)
                                                                                           (make-posn 0 1000))
                                                                                     "left"))
; Centi Bullet -> list of posns
; checks and drops segments which are hit
(define (drop? c b)
  (cond [(string=? (centi-dir c) "right") (filter-r (centi-segs c) b)]
        [(string=? (centi-dir c) "left") (filter-l (centi-segs c) b)]))
;direction -> direction
(define (change-dir c)
  (cond [(string=? c "right") "left"]
        [(string=? c "left") "right"]
        [else c]))
  
;;; lop bullet -> lop
;;; drops posn in list which don't satisfy our conditions
(define (filter-r c b)
 (cond [(empty? c) empty]
        [else (cons (drop-r (first  c) (bullet-posn b))
              (filter-r (rest c) b))]))
(define (filter-l c b)
 (cond [(empty? c) empty]
        [else (cons (drop-l (first  c) (bullet-posn b))
              (filter-l (rest c) b))]))
#;(check-expect (filter-l (list (make-posn 7.5 10)
                            (make-posn 22.5 10)
                            (make-posn 37.5 10)
                            (make-posn 52.5 10))
                      (make-bullet (make-posn 22.5 10) 'true))
              (list (make-posn 0 0)
                    (make-posn 0 0)
                    (make-posn 37.5 10)
                    (make-posn 52.5 10)))
              

;;;posn posn -> posn
;;; return posn which is less or greater than the bullet posn
(define (drop-l centi bullet)
  (cond [(< (posn-x centi) (posn-x bullet))       ;drop left
      (make-posn (posn-x centi) (posn-y centi))]
        [else (make-posn (posn-x centi) (+ CELL-SIZE (posn-y centi)))]))
;;;posn posn -> posn
;;; return posn which is less or greater than the bullet posn
(define (drop-r centi bullet)
  (cond    [(> (posn-x centi) (posn-x bullet))       ;drop right
      (make-posn (posn-x centi) (posn-y centi))]
        [else (make-posn (posn-x centi) (+ CELL-SIZE (posn-y centi)))]))



;===========to draw =============

;;;Image Number Number --> Image
;;; Just like PLACE-IMAGE, but use grid coordinates.
(define (place-images-on-grid img posn scn)
  (place-image img
             posn
               scn))

(define (mushrooms+scene m scn)
  (cond
    [(empty? m) scn]
	[else (mushroom+scene (first m)
			     (mushrooms+scene (rest m) scn))]))

(define (mushroom+scene m scn)
  (place-image-on-grid (circle MUSHROOM-RADIUS 'solid (mushroom-color m))
                       (posn-x (mushroom-posn m))
                       (posn-y (mushroom-posn m))
                       scn))

; lop number -> lop
; add posn to list until list length is n
(define (make n lop)
  (cond [(= (length lop) n) lop]
        [(cons? lop) (make n (add-posn lop))]))

; add posn to list until list length is n
;list->
;lop -> lop
(define (add-posn lop)
  (list* (make-posn (random (* CELL-SIZE GRID-WIDTH)) (random (* CELL-SIZE GRID-HEIGHT)))lop))
;(check-expect (make 20 (list (make-posn (random (* CELL-SIZE GRID-WIDTH)) (random (* CELL-SIZE GRID-HEIGHT)))))10)


;;;Image Number Number --> Image
;;; Just like PLACE-IMAGE, but use grid coordinates.
(define (place-image-on-grid img x y scn)
  (place-image img
             x
             y
               scn))
(check-expect (place-image-on-grid LEFT-HEAD 10 10 BG)
              (place-image LEFT-HEAD 10 10 BG))

;;; Player Scene -> Scene 
;;; Add image of player to the given scene.
(define (player+scene p scn)
  (place-image-on-grid PLAYER (player-x p) PLAYER-Y scn))
(check-expect (player+scene (make-player 10 'left) BG)
              (place-image PLAYER  10 592.5 BG))

;;;Bullet Image --> Image
;;;Add image of Bullet to the given scene
(define (bullet+scene b scn)
    (place-image-on-grid BULLET (posn-x (bullet-posn b)) (posn-y (bullet-posn b)) scn))
(check-expect (bullet+scene (make-bullet (make-posn 10 10) true) BG)
              (place-image BULLET 10 10 BG))

;; centi Image -> Image
;; Add an head on the scene
(define (head+scene c scn)
  (cond
    [(string=? "right" (centi-dir c))
     (place-image RIGHT-HEAD
                  (posn-x (first (centi-segs c)))
                  (posn-y (first (centi-segs c)))
                  scn)]
    [(string=? "left" (centi-dir c))
     (place-image LEFT-HEAD
                  (posn-x (first (centi-segs c)))
                  (posn-y (first (centi-segs c)))
                  scn)]
    [else (place-image CENTIPEDE-CELL
                        (posn-x (first (centi-segs c)))
                        (posn-y (first (centi-segs c)))
                        scn)]))


;;; centi Image -> Image
;;; Add an image of the centi to the scene.
(define (centi+scene c scn)
  (segments+scene (centi-segs c) scn))
(check-expect (centi+scene (make-centi (list (make-posn 10 10)
                                             (make-posn 20 20))'left)  BG)
              (place-image-on-grid CENTIPEDE-CELL 10 10
                                   (place-image-on-grid CENTIPEDE-CELL 20 20
                                                        BG))) 

;;; Symbol los Image -> Image 
;;; Add an image of the centipede segments to the scene.
(define (segments+scene segs scn)
  (cond
    [(empty? segs) scn]
	[else (segment+scene (first segs)
			     (segments+scene (rest segs) scn))]))
(check-expect (segments+scene  (list (make-posn 10 10)
                                           (make-posn 20 20)) BG)
              (place-image-on-grid CENTIPEDE-CELL 10 10
                                   (place-image-on-grid CENTIPEDE-CELL 20 20
                                                        BG))) 

;;;Posn Image -> Image
;;; Add one snake segment to a scene.
(define (segment+scene seg scn)
  (place-image-on-grid CENTIPEDE-CELL (posn-x seg) (posn-y seg) scn))
(check-expect (segment+scene (make-posn 10 10) BG) (place-image-on-grid CENTIPEDE-CELL 10 10 BG))

;===========stop when===========
;world -->boolean
;to determine if the player lose
(define (loser? w)
    (and (or (= (+ (posn-x (first (centi-segs (world-centi w)))) (/ CELL-SIZE 2))
                (- (player-x (world-player w)) ( / CELL-SIZE 2)))
             (= (- (posn-x (first (centi-segs (world-centi w)))) (/ CELL-SIZE 2))
                (+ (player-x (world-player w)) ( / CELL-SIZE 2))))
        (= (posn-y (first (centi-segs (world-centi w)))) PLAYER-Y)))
  

;world --> boolean
;to determine if the player win
(define (winner? w)
  (if (and (hit! (first (centi-segs (world-centi w))) (bullet-posn (world-bullet w)))
           (boolean=? (bullet-fire (world-bullet w)) true))
      true false))

;lop --> boolean
;have all the segs been hit>?
(define (all-hit? lop)
  (cond [(empty? lop) empty]
        [(cons? lop) (and (>= (posn-y (first lop)) 1000)
              (all-hit? (rest lop)))]))


(main 10); GIVE THE LENGTH OF YOUR CENTIPEDE 

       
       
