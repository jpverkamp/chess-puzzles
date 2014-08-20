#lang racket/base

(require racket/match)

; Points, with associated methods; can also be used as offsets
(struct pt (x y) #:transparent)

(define (pt+ p1 p2)
  (match-define (pt x1 y1) p1)
  (match-define (pt x2 y2) p2)
  (pt (+ x1 x2) (+ y1 y2)))

(define (pt* s p)
  (match-define (pt x y) p)
  (pt (* s x) (* s y)))