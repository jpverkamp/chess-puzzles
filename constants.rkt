#lang racket

(provide current-board-size
         current-render-scale)

; Parameters for the current board
(define current-board-size (make-parameter 8))

; Current rendering scale (the size of each tile in pixels)
(define current-render-scale (make-parameter 20))
