#lang racket

(require "../board.rkt"
         "../pieces.rkt"
         "../render.rkt"
         "../pt.rkt")

(current-glyphs (hash 'Queen "♛"))

; Try to place n queens on an nxn chessboard such that none are threatened
(define (queens n #:all? [generate-all? #f])
  ; Create a new nxn board
  (define-piece Queen (move 'n '*))
  (define b (make-board n #:pieces (hash 'Queen Queen)))
  
  ; Try to place a queen in each row
  (let place-queen ([board b] [x 0] [y 0])
    (cond
      ; Done with all of the rows, we have a valid configuration
      [(>= y n) (if generate-all? (list board) board)]
      ; Done with the current row, if we haven't placed a queen yet, bail out
      [(>= x n) (if generate-all? (list)       #f)]
      ; Otherwise, try to place the queen at this location
      ; Use the row as the player ID so they can all attack one another
      [else
       (define new-board (board-set board (pt x y) `(,y Queen)))
       (cond
         ; We attack a current queen, try the next square on the old board
         [(for/first ([target-pt (moves-from new-board (pt x y))]
                      #:when (board-ref new-board target-pt))
            #t)
          (place-queen board (+ x 1) y)]
         ; We do not attack anything (yet), try this solution
         ; If that fails, fall through (or short circuits) 
         ; If we're generating all solutions, do both
         ; (we cannot do ((if generate-all? append or) ...) because or is a macro
         [generate-all?
          (append (place-queen new-board 0 (+ y 1))
                  (place-queen board     (+ x 1) y))]
         [else
          (or (place-queen new-board 0 (+ y 1))
              (place-queen board     (+ x 1) y))])])))

; Examples:
; (for/list ([n (in-range 10)]) (list n (length (queens n #:all? #t))))
; (map (λ (b) (render b #:player-colors (const "black"))) (queens 6 #:all? #t))

