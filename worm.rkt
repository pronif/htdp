#lang htdp/bsl+
(require 2htdp/image)
(require 2htdp/universe)

(define DIAMETER 10)
(define RADIUS (/ DIAMETER 2))
(define SEGMENT (circle RADIUS "solid" "red"))
(define WORLD_SIZE 20)
(define WORLD (empty-scene (* WORLD_SIZE DIAMETER) (* WORLD_SIZE DIAMETER)))
(define MIDDLE (floor (/ WORLD_SIZE 2)))

; Example worm body for testing
(define BODY (list (make-posn 1 1) (make-posn 1 2) (make-posn 1 3)))

; A Direction is one of:
; - "up"
; - "down"
; - "left"
; - "right"

; A Body is a List-of-Posn
; Posn are expressed in logical coordinates from 0 to WORLD_SIZE
; where (0, 0) is the upper left corner.
; The head of the worm is the tail of the List-of-Posn

; A WormState is a structure
;  (make-world-state Body Direction)
(define-struct worm-state [body dir])

; Posn -> Posn
; Converts a logical position to physical position
(check-expect (physical-pos (make-posn 0 0))
              (make-posn RADIUS RADIUS))
              
(define (physical-pos pp)
  (make-posn (+ RADIUS (* (posn-x pp) DIAMETER))
             (+ RADIUS (* (posn-y pp) DIAMETER))))

; List-of-Posns -> List-of-Posns
; Converts a list of position from logical to physical coordinates
(check-expect (physical-body BODY)
              (list (make-posn (+ RADIUS (* 1 DIAMETER)) (+ RADIUS (* 1 DIAMETER)))
                    (make-posn (+ RADIUS (* 1 DIAMETER)) (+ RADIUS (* 2 DIAMETER)))
                    (make-posn (+ RADIUS (* 1 DIAMETER)) (+ RADIUS (* 3 DIAMETER)))))

(define (physical-body body)
  (cond
    [(empty? body) '()]
    [else (cons (physical-pos (first body))
                (physical-body (rest body)))]))

; WormState -> Body
; Extract worm body as list of segments
(define (segments-from-body ws)
  (cond
    [(empty? ws) '()]
    [else (cons SEGMENT (segments-from-body (rest ws)))]))

; WormState -> Image
; Render the worm state
(define (render ws)
  (place-images (segments-from-body (worm-state-body ws))
                (physical-body (worm-state-body ws))
                WORLD))

; WormState -> WormState
; Advances worm body according to direction
(define (advance ws)
  (make-worm-state
   (append (rest (worm-state-body ws))
           (list
            (make-posn
             (cond
               [(string=? "left" (worm-state-dir ws)) (- (posn-x (last (worm-state-body ws))) 1)]
               [(string=? "right" (worm-state-dir ws)) (+ (posn-x (last (worm-state-body ws))) 1)]
               [else (posn-x (last (worm-state-body ws)))])
             (cond
               [(string=? "up" (worm-state-dir ws)) (- (posn-y (last (worm-state-body ws))) 1)]
               [(string=? "down" (worm-state-dir ws)) (+ (posn-y (last (worm-state-body ws))) 1)]
               [else (posn-y (last (worm-state-body ws)))]))))
   (worm-state-dir ws)))

; List-of-Elem -> Elem
; Returns last element in a list
(define (last l)
  (first (reverse l)))

; WormState, Keyevent -> WormState
; Key handler. Returns a worm state with the direction updated based on key value.
(check-expect (set-direction (make-worm-state BODY "right") "left")
              (make-worm-state BODY "left"))
(check-expect (set-direction (make-worm-state BODY "down") "up")
              (make-worm-state BODY "up"))
(check-expect (set-direction (make-worm-state BODY "down") "down")
              (make-worm-state BODY "down"))
(check-expect (set-direction (make-worm-state BODY "down") "left")
              (make-worm-state BODY "left"))
(check-expect (set-direction (make-worm-state BODY "right") "right")
              (make-worm-state BODY "right"))
(check-expect (set-direction (make-worm-state BODY "left") "up")
              (make-worm-state BODY "up"))
(check-expect (set-direction (make-worm-state BODY "left") "c")
              (make-worm-state BODY "left"))

(define (set-direction ws ke)
  (make-worm-state (worm-state-body ws)
                   (if (or (string=? "up" ke) (string=? "down" ke) (string=? "left" ke) (string=? "right" ke))
                        ke
                        (worm-state-dir ws))))

; WormState -> Bool
; Returns true if the worm hit a wall
(check-expect (worm-wall? (make-worm-state (list (make-posn 0 0)) "up")) #t)
(check-expect (worm-wall? (make-worm-state (list (make-posn 0 0)) "left")) #t)
(check-expect (worm-wall? (make-worm-state (list (make-posn (- WORLD_SIZE 1) (- WORLD_SIZE 1))) "down")) #t)
(check-expect (worm-wall? (make-worm-state (list (make-posn (- WORLD_SIZE 1) (- WORLD_SIZE 1))) "right")) #t)
(check-expect (worm-wall? (make-worm-state (list (make-posn 1 1)) "up")) #f)
(check-expect (worm-wall? (make-worm-state (list (make-posn 1 1)) "left")) #f)
(check-expect (worm-wall? (make-worm-state (list (make-posn 1 1)) "down")) #f)
(check-expect (worm-wall? (make-worm-state (list (make-posn 1 1)) "right")) #f)

(define (worm-wall? ws)
  (or (and (string=? (worm-state-dir ws) "up") (= (posn-y (last (worm-state-body ws))) 0))
      (and (string=? (worm-state-dir ws) "left") (= (posn-x (last (worm-state-body ws))) 0))
      (and (string=? (worm-state-dir ws) "down") (= (posn-y (last (worm-state-body ws))) (- WORLD_SIZE 1)))
      (and (string=? (worm-state-dir ws) "right") (= (posn-x (last (worm-state-body ws))) (- WORLD_SIZE 1)))))

; WormState -> Image
; Renders game over scene
(define (game-over-scene ws)
  (overlay/align "left" "bottom" (text " GAME OVER! Worm hit border" 14 "red") (render ws)))

(define (worm-main rate)
  (big-bang (make-worm-state BODY "down")
    [on-tick advance rate]
    [to-draw render]
    [on-key set-direction]
    [stop-when worm-wall? game-over-scene]))

(worm-main (/ 1 4))