#lang racket/base

(provide current-glyphs
         render)

(require pict
         racket/format
         racket/function
         racket/match
         "board.rkt"
         "pieces.rkt"
         "pt.rkt")

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
; highlights is a hash of point to color to allow for special colorings
; player-colors is either a hash of player to color names or a function of the same
(define (render b 
                #:tile-size [tile-size 20] 
                #:highlights [special-tiles (hash)] 
                #:mode [mode 'bitmap]
                #:player-colors [player-colors (hash)])
  ; Sanity check and unpack, we don't care about the actual pieces at this point
  (when (not (current-glyphs)) (error 'render "must specify (current-glyphs) as a hash of name -> glyph"))
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
        (define tile-color
          (cond
            [(hash-ref special-tiles (pt col-index row-index) #f)
             => identity]
            [(even? (+ row-index col-index))
             "LightGray"]
            [else
             "DarkGray"]))
        (define tile (colorize (filled-rectangle 20 20) tile-color))
        
        (cond
          ; If there is a piece here, render the piece, colorize based on the player, and overlay on the tile
          [col
           (match-define (list player piece) col)
           (define glyph (render-glyph/pict (hash-ref (current-glyphs) piece) tile-size))
           (define player-color 
             (cond
               [(hash? player-colors)
                (hash-ref player-colors player (~a player))]
               [(procedure? player-colors)
                (player-colors player)]
               [else
                (~a player)]))
           
           (cc-superimpose tile (colorize glyph player-color))]
          ; If not, just return the empty tile
          [else
           tile]))))
  
  ; Smash together all the lists of lists
  ; Make it a bitmap for performance reasons
  ((case mode
     [(bitmap) pict->bitmap]
     [(pict)   identity])
   (apply vc-append
          (for/list ([row (in-list tiles)])
            (apply hc-append row)))))

