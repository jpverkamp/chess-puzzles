#lang racket

(provide (struct-out board)
         board-ref
         board-move!)

(require "constants.rkt"
         "pieces.rkt"
         "pt.rkt")

; Stored as a vector of vectors where every element is either:
; (player, piece) if there is a piece there
; #f if empty 
(struct board (pieces data) #:transparent)

; Get the current player/piece at a square
(define (board-ref b p)
  (match-define (board pieces data) b)
  (match-define (pt row col) p)
  (vector-ref (vector-ref data row) col))

; Move a piece from one square to another, overwriting whatever is in the destination square
(define (board-move! b src dst)
  (match-define (board pieces data) b)
  (match-define (pt src-row src-col) src)
  (match-define (pt dst-row dst-col) dst)
  (define piece (board-ref data src))
  (vector-set! (vector-ref data dst-row) dst-col piece)
  (vector-set! (vector-ref data src-row) src-col #f))
