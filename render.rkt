#lang racket/base

(provide current-glyphs
         render/pict)

(require pict
         racket/format
         racket/match
         "board.rkt"
         "pieces.rkt")

; The hash associating piece names with glyphs 
(define current-glyphs (make-parameter #f))

; A single glyph containing a string and the rotation to render it at (default to upright)
(struct glyph (character rotation) #:transparent)
(define (make-glyph character [rotation 0])
  (glyph (~a character) rotation))

; Render a single character with some sanity checking to allow character/sting glyphs
(define (render-glyph/pict g tile-size)
  (cond 
    [(glyph? g)
     (match-define (glyph character rotation) g)
     (rotate (text character 'default (* 3/4 tile-size)) rotation)]
    [else
     (render-glyph/pict (make-glyph g) tile-size)]))

; Render a board to a pict with the given tile size
(define (render/pict b [tile-size 20])
  ; Sanity check and unpack, we don't care about the actual pieces at this point
  (when (not (current-glyphs)) (error 'render/pict "must specify (current-glyphs) as a hash of name -> glyph"))
  (match-define (board _ board-data) b)
 
  ; Render each tile into a list of lists
  (define tiles
    (for/list ([row-index (in-naturals)]
               [row       (in-vector board-data)])
      (for/list ([col-index (in-naturals)]
                 [col       (in-vector row)])
        ; Get the background tile
        ; TODO: Paramaterize the tile colors
        ; TODO: Figure out how to do borders
        (define tile (colorize (filled-rectangle 20 20) (if (even? (+ row-index col-index)) "LightGray" "DarkGray")))
        (cond
          ; If there is a piece here, render the piece, colorize based on the player, and overlay on the tile
          [col
           (match-define (list player piece) col)
           (define glyph (render-glyph/pict (hash-ref (current-glyphs) piece) tile-size))
           (cc-superimpose tile (colorize glyph (~a player)))]
          ; If not, just return the empty tile
          [else
           tile]))))
  
  ; Smash together all the lists of lists
  (apply vc-append
         (for/list ([row (in-list tiles)])
           (apply hc-append row))))
