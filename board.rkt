#lang racket/base

(provide (struct-out board)
         board-ref
         board-move!
         moves-from)

(require racket/match
         racket/list
         racket/set
         "constants.rkt"
         "pieces.rkt"
         "pt.rkt")

; Stored as a vector of vectors where every element is either:
; (player, piece) if there is a piece there
; #f if empty 
(struct board (pieces data) #:transparent)

; Test if a point is on the given board
(define (on-board? b p)
  (match-define (board pieces data) b)
  (match-define (pt x y) p)
  (and (<= 0 x (- (vector-length (vector-ref data 0)) 1))
       (<= 0 y (- (vector-length data) 1))))

; Get the current player/piece at a square
(define (board-ref b p)
  (match-define (board pieces data) b)
  (match-define (pt x y) p)
  (cond
    [(on-board? b p)
     (vector-ref (vector-ref data y) x)]
    [else
     #f]))

; Move a piece from one square to another, overwriting whatever is in the destination square
(define (board-move! b src dst)
  (match-define (board pieces data) b)
  (match-define (pt src-x src-y) src)
  (match-define (pt dst-x dst-y) dst)
  (define piece (board-ref data src))
  (vector-set! (vector-ref data dst-y) dst-x piece)
  (vector-set! (vector-ref data src-y) src-x #f))

; Return a list of moves that a piece can make on the given board given it's origin point
; TODO: Implement locusts
(define (moves-from b origin #:initial [initial #f])
  (match-define (board pieces data) b)
  (cond
    [(board-ref b origin)
     => (λ (ls)
          (match-define (list player name) ls)
          (match-define (piece _ moves) (hash-ref pieces name))
          
          ; White has inverse moves since they're moving 'up'
          ; TODO: Generalize to more players
          ; TODO: This moves the wrong way if you have 'left only' pieces
          (define player-multiplier
            (case player
              [(BLACK Black black)  1]
              [(WHITE White white) -1]
              [else     1]))
          
          (define move-sublists
           (for*/list ([move-seq (in-list moves)])
             (match-define (move-sequence tags original-offset*) move-seq)
             (define offset* (map (λ (offset) (pt* player-multiplier offset)) original-offset*))
             
             ; Find the first target
             (define first-target
               (for/first ([i (in-naturals)]
                           [offset (in-list offset*)]
                           #:when (board-ref b (pt+ origin offset)))
                 (list i (board-ref b (pt+ origin offset)))))
             
             ; If the first target belongs to the owner, remove it (no self captures)
             ; TODO: Add an option for self-captures
             (define self-capture
               (and first-target
                    (eq? (first (second first-target)) player)))
             
             (map (λ (offset) (pt+ origin offset))
                  (cond
                    ; Bail out if we're initial only but not on the initial move
                    [(and (set-member? tags 'initial-only) (not initial))
                     (list)]
                    ; If we're capturing only, can only move if we have a target and to that square
                    [(set-member? tags 'capture-only)
                     (if (and first-target (not self-capture))
                         (list (list-ref offset* (first first-target)))
                         (list))]
                    ; If we're not capturing, get everything up until the target (or everything if no target)
                    [(set-member? tags 'non-capture)
                     (if (and first-target (> (first first-target) 0))
                         (take offset* (- (first first-target) 1))
                         offset*)]
                    ; If we're a locust, we have to check the space after the self target is empty
                    [(set-member? tags 'as-locust)
                     (cond
                       [(and first-target (not self-capture) (> (length offset*) (+ (first first-target) 1)))
                        (define next-target (board-ref b (list-ref offset* (+ (first first-target) 1))))
                        (if (not next-target)
                            (list (list-ref offset* (+ 1 (first first-target))))
                            (list))]
                       [else
                        (list)])]
                    ; Otherwise, if the target is a piece but we own it, don't land there
                    [(and first-target self-capture)
                     (if (> (first first-target) 0)
                         (take offset* (- (first first-target) 1))
                         (list))]
                    ; Otherwise, include the entire range
                    [else
                     offset*]))))
          
          ; Stick all the lists together since we no longer care how they got there
          ; And remove all moves that jump off of the board somehow
          (filter (λ (p) (on-board? b p)) (apply append move-sublists)))]
    [else
     '()]))
