#lang racket

(require racket/generator
         2htdp/image
         2htdp/universe
         "../board.rkt"
         "../pieces.rkt"
         "../render.rkt"
         "../pt.rkt")

(current-glyphs (hash 'Knight "♞"
                      'Invalid "✗"
                      'Visited "♘"))

(define-piece Knight (move 1 (leaper 1 2)))
(define-piece Invalid '())
(define-piece Visited '())

; Solve a knights tour on a given board (optionally with some pieces removed)
(define (knights-tour width [height width] #:removed [removed '()] #:generator? [generator? #f])
  (define g 
    (generator ()
      ; Create a new board by removing 
      (define b
        (for/fold ([b (make-board width height #:pieces (list Knight Invalid Visited))])
          ([p (in-list removed)])
          (board-set b p '(Black Invalid))))
      
      ; A board is completed if there are no empty squares
      ; All squares are either the Knight, Invalid, or Visited
      (define (completed? b)
        (not (for*/first ([x (in-range width)]
                          [y (in-range height)]
                          #:when (not (board-ref b (pt x y))))
               #t)))
      
      ; Move the knight from the given point to each possible next point in turn
      ; Return a list of valid moves to finish the tour if such a list exists, #f otherwise
      (define (move-knight b p)
        (when generator?
          (yield b p))
        
        (cond
          ; Done, we have a tour and there are no more valid moves necessary
          [(completed? b) '()]
          ; Otherwise, try all possible moves from this point
          ; Since all pieces are the same color, only valid moves are in the moves-from list
          [else
           (for*/first ([next (in-list (moves-from b p))]
                        [recur (in-value (let* ([b (board-move b p next)]
                                                [b (board-set b p '(Black Visited))])
                                           (move-knight b next)))]
                        #:when recur)
             (cons p recur))]))
      
      ; Try each possible initial location until we find one
      (for*/first ([x (in-range width)]
                   [y (in-range height)]
                   [solution (in-value (move-knight (board-set b (pt x y) '(Black Knight)) (pt x y)))]
                   #:when solution)
        solution)))
  
  (if generator? g (g)))

; Render the search for a knights tour into an animation
(define (knights-tour/animate-search width [height width] #:removed [removed '()])
  ; Create an initial board including the missing tiles
  (define board
    (for/fold ([b (make-board width height #:pieces (list Knight Invalid Visited))])
              ([p (in-list removed)])
      (board-set b p '(Black Invalid))))
  
  ; Prerender the board once so we know how large of a scene to create
  (define r (render board #:player-colors (const "black")))
  (define render-width (send r get-width))
  (define render-height (send r get-height))
  
  ; Set the last board, which will be updated on each yield
  (define last-board board)
  (define last-point (pt 0 0))
  (define g (knights-tour width height #:removed removed #:generator? #t))
  
  ; Animate the tour, rendering each frame once
  (big-bang #t
    [on-tick 
     (λ (running?)
       (and running? 
            (with-handlers ([exn? (const #f)]) ; Stop when the generator returns 1 value
              (define-values (board point) (g))
              (set! last-board board)
              (set! last-point point)
              #t)))]
    [to-draw 
     (λ (_) 
       (place-image 
        (render last-board)
        (/ render-width 2)
        (/ render-height 2)
        (empty-scene render-width render-height)))]
    [stop-when (negate identity)]
    [record? #t]))

; Render a knights tour into an animation
(define (knights-tour/animate-solution width [height width] #:removed [removed '()])
  (define tour (knights-tour width height #:removed removed))

  ; Create an initial board including the missing tiles
  (define board
    (for/fold ([b (make-board width height #:pieces (list Knight Invalid Visited))])
              ([p (in-list removed)])
      (board-set b p '(Black Invalid))))
  
  ; Prerender the board once so we know how large of a scene to create
  (define r (render board #:player-colors (const "black")))
  (define render-width (send r get-width))
  (define render-height (send r get-height))

  ; Animate the tour, rendering each frame once
  (big-bang tour
    [on-tick cdr]
    [to-draw (λ (tour) 
               (place-image 
                (render (if (null? tour)
                            board
                            (board-set board (car tour) '(Black Knight))))
                (/ render-width 2)
                (/ render-height 2)
                (empty-scene render-width render-height)))]
    [stop-when null?]
    [record? #t]))
  
(define (knights-tour/render-solution width [height width] #:removed [removed '()])
  (define tour (knights-tour width height #:removed removed))
  (define board
    (for/fold ([b (make-board width height #:pieces (list Knight Invalid Visited))])
              ([p (in-list removed)])
      (board-set b p '(Black Invalid))))
  (render board #:highlights (for/hash ([p (in-list tour)]
                                        [i (in-naturals)])
                               (define g (quotient (* i 256) (length tour)))
                               (values p (list 0 g 0)))))