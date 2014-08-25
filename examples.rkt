#lang racket/base

(provide (all-defined-out))

(require "board.rkt"
         "pieces.rkt"
         "pt.rkt"
         "render.rkt")

; ===== Standard chess pieces =====
(define-piece King   (move 1 '*))
(define-piece Queen  (move 'n '*))
(define-piece Rook   (move 'n '+))
(define-piece Bishop (move 'n 'X))
(define-piece Knight (move  1 (leaper 1 2)))
(define-piece Pawn   
  (alternatives
   (on-non-capture (on-initial (move 2 '>)))
   (on-non-capture             (move 1 '>))
   (on-capture                 (move 1 'X>))))

(define standard-pieces (list King Queen Rook Bishop Knight Pawn))

(define (make-standard-board)
  (board (hash 'Rook   Rook
               'Knight Knight
               'Bishop Bishop
               'Queen  Queen
               'King   King
               'Pawn   Pawn)
         '#(#((Black Rook) (Black Knight) (Black Bishop) (Black Queen) (Black King) (Black Bishop) (Black Knight) (Black Rook))
            #((Black Pawn) (Black Pawn) (Black Pawn) (Black Pawn) (Black Pawn) (Black Pawn) (Black Pawn) (Black Pawn))
            #(#f #f #f #f #f #f #f #f)
            #(#f #f #f #f #f #f #f #f)
            #(#f #f #f #f #f #f #f #f)
            #(#f #f #f #f #f #f #f #f)
            #((White Pawn) (White Pawn) (White Pawn) (White Pawn) (White Pawn) (White Pawn) (White Pawn) (White Pawn))
            #((White Rook) (White Knight) (White Bishop) (White Queen) (White King) (White Bishop) (White Knight) (White Rook)))))

(define (set-standard-glyphs)
  (current-glyphs (hash 'Rook   "♜" 
                        'Knight "♞"
                        'Bishop "♝"
                        'Queen  "♛"
                        'King   "♚"
                        'Pawn   "♟")))

(set-standard-glyphs)

; ===== Fairy chess pieces =====

; Move one square like a rook, followed by any number of spaces diagonally outwards
(define-piece Aanca
  (alternatives 
   (and-then (move 1 '>) (move 'n 'X>))
   (and-then (move 1 (pt 1 0))  (alternatives (move 'n (pt  1 -1)) (move 'n (pt  1  1))))
   (and-then (move 1 '<) (move 'n 'X<))
   (and-then (move 1 (pt -1 0)) (alternatives (move 'n (pt -1 -1)) (move 'n (pt -1  1))))))

(define-piece Adjutant     (move  1 '<>))
(define-piece Advisor      (move  1 'X))
(define-piece Alfil        (move  1 (leaper 2 2)))
(define-piece Afilrider    (move 'n (leaper 2 2)))
(define-piece Alibaba      (alternatives (move 'n (leaper 2 2)) (move 'n (leaper 0 2))))
(define-piece Amazon       (alternatives (move 1 (leaper 1 2)) (move 'n '*)))
(define-piece Antelope     (move  1 (leaper 3 4)))
(define-piece Archbishop   (alternatives (move 'n 'X) (move 1 (leaper 1 2))))
(define-piece ArrowPawn    (alternatives (on-non-capture (move 1 '+)) (on-capture (move 1 'X))))
(define-piece Backslider   (move  1 '<))
(define-piece Banshee      (alternatives (move 'n 'X) (move 'n (leaper 1 2))))
(define-piece Bede         (alternatives (move 'n 'X) (move 'n (leaper 0 2))))
(define-piece BerolinaPawn (alternatives (on-non-capture (on-initial (move 2 'X>))) (on-non-capture (move 1 'X>)) (on-capture (move 1 '>))))

(define-piece Knightrider (move 'n (leaper 1 2)))

; ===== Other random pieces =====
(define-piece Checker-Pawn (alternatives (move 1 'X>) (as-locust (move 2 'X>))))
(define-piece Checker-King (alternatives (move 1 'X)  (as-locust (move 2 'X ))))