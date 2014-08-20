#lang racket/base

(require "moves.rkt"
         "pt.rkt")

; ===== Standard chess pieces =====
(define King   (move  1 '*))
(define Queen  (move 'n '*))
(define Rook   (move 'n '+))
(define Bishop (move 'n 'X))
(define Knight (move  1 (leaper 1 2)))
(define Pawn   
  (alternatives
   (on-non-capture (on-initial (move 2 '>)))
   (on-non-capture             (move 1 '>))
   (on-capture                 (move 1 'X>))))

; ===== Fairy chess pieces =====

; Move one square like a rook, followed by any number of spaces diagonally outwards
(define Aanca (alternatives (and-then (move 1 '>) (move 'n 'X>))
                            (and-then (move 1 (pt 1 0))  (alternatives (move 'n (pt  1 -1)) (move 'n (pt  1  1))))
                            (and-then (move 1 '<) (move 'n 'X<))
                            (and-then (move 1 (pt -1 0)) (alternatives (move 'n (pt -1 -1)) (move 'n (pt -1  1))))))

(define Adjutant     (move  1 '<>))
(define Advisor      (move  1 'X))
(define Alfil        (move  1 (leaper 2 2)))
(define Afilrider    (move 'n (leaper 2 2)))
(define Alibaba      (alternatives (move 'n (leaper 2 2)) (move 'n (leaper 0 2))))
(define Amazon       (alternatives (move 1 (leaper 1 2)) (move 'n '*)))
(define Antelope     (move  1 (leaper 3 4)))
(define Archbishop   (alternatives (move 'n 'X) (move 1 (leaper 1 2))))
(define ArrowPawn    (alternatives (on-non-capture (move 1 '+)) (on-capture (move 1 'X))))
(define Backslider   (move  1 '<))
(define Banshee      (alternatives (move 'n 'X) (move 'n (leaper 1 2))))
(define Bede         (alternatives (move 'n 'X) (move 'n (leaper 0 2))))
(define BerolinaPawn (alternatives (on-non-capture (on-initial (move 2 'X>))) (on-non-capture (move 1 'X>)) (on-capture (move 1 '>))))

(define Knightrider (move 'n (leaper 1 2)))

; ===== Other random pieces =====
(define Checker-Pawn (alternatives (move 1 'X>) (as-locust (move 2 'X>))))
(define Checker-King (alternatives (move 1 'X)  (as-locust (move 2 'X ))))