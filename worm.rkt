#lang htdp/bsl+
(require 2htdp/image)
(require 2htdp/universe)

(define DIAMETER 10)
(define RADIUS (/ DIAMETER 2))
(define WORM (circle RADIUS "solid" "red"))
(define WORLD_SIZE 20)
(define WORLD (empty-scene (* WORLD_SIZE DIAMETER) (* WORLD_SIZE DIAMETER)))
(define MIDDLE (floor (/ WORLD_SIZE 2)))

(define-struct worm-state [posn dir])

; Posn -> Posn
; Logical position to physical position
(check-expect (physical-pos (make-posn 0 0))
              (make-posn RADIUS RADIUS))
              
(define (physical-pos pp)
  (make-posn (+ RADIUS (* (posn-x pp) DIAMETER))
             (+ RADIUS (* (posn-y pp) DIAMETER))))

; WormState -> Image
; Render the worm state
(check-expect (render (make-worm-state (make-posn 1 0) "right"))
              (place-image WORM (+ RADIUS DIAMETER) RADIUS WORLD))

(define (render ws)
  (place-image WORM
               (posn-x (physical-pos (worm-state-posn ws)))
               (posn-y (physical-pos (worm-state-posn ws)))
               WORLD))

; WormState -> WormState
; Advances worm state according to direction
(check-expect (advance (make-worm-state (make-posn 1 1) "up"))
              (make-worm-state (make-posn 1 0) "up"))
(check-expect (advance (make-worm-state (make-posn 0 0) "down"))
              (make-worm-state (make-posn 0 1) "down"))
(check-expect (advance (make-worm-state (make-posn 1 1) "left"))
              (make-worm-state (make-posn 0 1) "left"))
(check-expect (advance (make-worm-state (make-posn 0 1) "right"))
              (make-worm-state (make-posn 1 1) "right"))
(check-expect (advance (make-worm-state (make-posn 0 0) "up"))
              (make-worm-state (make-posn 0 0) "up"))
(check-expect (advance (make-worm-state (make-posn (- WORLD_SIZE 1) 0) "right"))
              (make-worm-state (make-posn (- WORLD_SIZE 1) 0) "right"))
(check-expect (advance (make-worm-state (make-posn 1 (- WORLD_SIZE 1)) "down"))
              (make-worm-state (make-posn 1 (- WORLD_SIZE 1)) "down"))

(define (advance ws)
  (cond
    [(and (string=? (worm-state-dir ws) "up")
          (> (posn-y (worm-state-posn ws)) 0))
     (make-worm-state (make-posn (posn-x (worm-state-posn ws))
                                 (- (posn-y (worm-state-posn ws)) 1))
                      "up")]
    [(and (string=? (worm-state-dir ws) "down")
          (< (posn-y (worm-state-posn ws)) (- WORLD_SIZE 1)))
     (make-worm-state (make-posn (posn-x (worm-state-posn ws))
                                 (+ (posn-y (worm-state-posn ws)) 1))
                      "down")]
    [(and (string=? (worm-state-dir ws) "left")
          (> (posn-x (worm-state-posn ws)) 0))
     (make-worm-state (make-posn (- (posn-x (worm-state-posn ws)) 1)
                                 (posn-y (worm-state-posn ws)))
                      "left")]
    [(and (string=? (worm-state-dir ws) "right")
          (< (posn-x (worm-state-posn ws)) (- WORLD_SIZE 1)))
     (make-worm-state (make-posn (+ (posn-x (worm-state-posn ws)) 1)
                                 (posn-y (worm-state-posn ws)))
                      "right")]
    [else ws]))

; WormState, Keyevent -> WormState
; Key handler. Returns a worm state with the direction updated based on key value.
(check-expect (set-direction (make-worm-state (make-posn 1 2) "right") "left")
              (make-worm-state (make-posn 1 2) "left"))
(check-expect (set-direction (make-worm-state (make-posn 1 2) "down") "up")
              (make-worm-state (make-posn 1 2) "up"))
(check-expect (set-direction (make-worm-state (make-posn 1 2) "down") "down")
              (make-worm-state (make-posn 1 2) "down"))
(check-expect (set-direction (make-worm-state (make-posn 1 2) "down") "left")
              (make-worm-state (make-posn 1 2) "left"))
(check-expect (set-direction (make-worm-state (make-posn 1 2) "right") "right")
              (make-worm-state (make-posn 1 2) "right"))
(check-expect (set-direction (make-worm-state (make-posn 1 2) "left") "up")
              (make-worm-state (make-posn 1 2) "up"))
(check-expect (set-direction (make-worm-state (make-posn 1 2) "left") "c")
              (make-worm-state (make-posn 1 2) "left"))

(define (set-direction ws ke)
  (make-worm-state (worm-state-posn ws)
                   (if (or (string=? "up" ke) (string=? "down" ke) (string=? "left" ke) (string=? "right" ke))
                        ke
                        (worm-state-dir ws))))

; WormState -> Bool
; Returns true if the worm hit a wall
(check-expect (worm-wall? (make-worm-state (make-posn 0 0) "up")) #t)
(check-expect (worm-wall? (make-worm-state (make-posn 0 0) "left")) #t)
(check-expect (worm-wall? (make-worm-state (make-posn (- WORLD_SIZE 1) (- WORLD_SIZE 1)) "down")) #t)
(check-expect (worm-wall? (make-worm-state (make-posn (- WORLD_SIZE 1) (- WORLD_SIZE 1)) "right")) #t)
(check-expect (worm-wall? (make-worm-state (make-posn 1 1) "up")) #f)
(check-expect (worm-wall? (make-worm-state (make-posn 1 1) "left")) #f)
(check-expect (worm-wall? (make-worm-state (make-posn 1 1) "down")) #f)
(check-expect (worm-wall? (make-worm-state (make-posn 1 1) "right")) #f)

(define (worm-wall? ws)
  (or (and (string=? (worm-state-dir ws) "up") (= (posn-y (worm-state-posn ws)) 0))
      (and (string=? (worm-state-dir ws) "left") (= (posn-x (worm-state-posn ws)) 0))
      (and (string=? (worm-state-dir ws) "down") (= (posn-y (worm-state-posn ws)) (- WORLD_SIZE 1)))
      (and (string=? (worm-state-dir ws) "right") (= (posn-x (worm-state-posn ws)) (- WORLD_SIZE 1)))))

; WormState -> Image
; Renders game over scene
(define (game-over-scene ws)
  (overlay/align "left" "bottom" (text " GAME OVER! Worm hit border" 14 "red") (render ws)))

(define (worm-main rate)
  (big-bang (make-worm-state (make-posn MIDDLE MIDDLE) "up")
    [on-tick advance rate]
    [to-draw render]
    [on-key set-direction]
    [stop-when worm-wall? game-over-scene]))

(worm-main (/ 1 4))