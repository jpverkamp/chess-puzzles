#lang racket/base

(provide (struct-out move-sequence)
         move
         leaper
         alternatives
         and-then 
         on-initial
         on-capture
         on-non-capture
         as-locust
         (struct-out pt))

(require racket/match
         racket/list
         racket/set
         "pt.rkt")

; Points, with associated methods; can also be used as offsets
(struct pt (x y) #:transparent)

(define (pt+ p1 p2)
  (match-define (pt x1 y1) p1)
  (match-define (pt x2 y2) p2)
  (pt (+ x1 x2) (+ y1 y2)))

(define (pt* s p)
  (match-define (pt x y) p)
  (pt (* s x) (* s y)))

; Move sequences for a piece; used for special flags (such as initial, capturing, etc)
(struct move-sequence (tag moves) #:transparent)

; Parameters for the current board
(define current-board-size (make-parameter 8))

; Return a sequence of all possible offsets for a given direction
(define (offsets-by-direction direction)
  `(,@(if (member direction '(*                  X X>   )) (list (pt -1  1)) '()) ; forward left
    ,@(if (member direction '(* + >   <>   >=           )) (list (pt  0  1)) '()) ; forward
    ,@(if (member direction '(*                  X X>   )) (list (pt  1  1)) '()) ; forward right
    ,@(if (member direction '(* +        = >= <=        )) (list (pt -1  0)) '()) ; left
    ,@(if (member direction '(* +        = >= <=        )) (list (pt  1  0)) '()) ; right
    ,@(if (member direction '(*                  X    X<)) (list (pt -1 -1)) '()) ; backward left
    ,@(if (member direction '(* +   < <>      <=        )) (list (pt  0 -1)) '()) ; backward
    ,@(if (member direction '(*                  X    X<)) (list (pt  1 -1)) '()) ; backward right
    ,@(if (pt? direction)   (list direction) '())
    ,@(if (list? direction) direction        '())))

; A sequence of moves along a specific direction
; Distance is either not specified (exactly 1), a number (exactly that number), n (unlimited), or a range (min/max inclusive)
; Direction is either from the list '(* + > < <> = >= <= X X> X<) or a list of possible single offsets
(define move 
  (case-lambda
    [(direction)
     (move 1 1 direction)]
    [(distance direction) 
     (if (eq? distance 'n)
         (move 1        +inf.0   direction)
         (move distance distance direction))]
    [(minimum-distance maximum-distance direction) 
     (for/list ([offset (in-list (offsets-by-direction direction))])
       (move-sequence
        (set)
        (for*/list ([distance (in-range minimum-distance (+ maximum-distance 1))]
                    [p (in-value (pt* distance offset))])
                    #:break (or (> (abs (pt-x p)) (current-board-size))
                                (> (abs (pt-y p)) (current-board-size)))
          p)))]))

; Make a leaper from a given offset
(define (leaper xΔ yΔ)
  (set->list
   (list->set
    `(,(pt    xΔ     yΔ )
      ,(pt (- xΔ)    yΔ )
      ,(pt    xΔ  (- yΔ))
      ,(pt (- xΔ) (- yΔ))
      ,(pt    yΔ     xΔ )
      ,(pt (- yΔ)    xΔ )
      ,(pt    yΔ  (- xΔ))
      ,(pt (- yΔ) (- xΔ))))))

; Merge multiple move lists by allowing any of them
(define (alternatives . list*)
  (apply append list*))

; Merge multiple move lists by doing one and then the next, each relative to the previous endpoint
(define (and-then first* rest*)
  (for*/list ([first (in-list first*)]
              [rest  (in-list rest*)])
    (match-define (move-sequence first-flags first-moves) first)
    (match-define (move-sequence rest-flags  rest-moves)  rest)
    (define offset (last first-moves))
    (move-sequence
     (set-union first-flags rest-flags)
     (append first-moves (map (λ (each) (pt+ offset each)) rest-moves)))))

; Set special flags for move lists
(define (set-flag flag movelist*)
  (for/list ([movelist (in-list movelist*)])
    (match-define (move-sequence flags moves) movelist)
    (move-sequence (set-add flags flag) moves)))

(define (on-initial     movelist*) (set-flag 'initial-only movelist*))
(define (on-capture     movelist*) (set-flag 'capture-only movelist*))
(define (on-non-capture movelist*) (set-flag 'non-capture  movelist*))
(define (as-locust      movelist*) (set-flag 'locust       movelist*))
    


