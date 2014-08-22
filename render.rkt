#lang racket/base

(provide current-glyphs
         render/pict)

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
; Movelists is a hash of point to color to allow for special colorings
; If origin is specified, generate a movelist directly from that point
(define (render/pict b #:tile-size [tile-size 20] #:highlights [special-tiles (hash)] #:move-from [origin #f])
  (cond
    ; Get moves from an origin
    [origin
     (define move-hash
       (hash-set
        (for/hash ([move (in-list (moves-from b origin))])
          (values move "green"))
        origin
        "blue"))
     (displayln move-hash)
     (render/pict b #:tile-size tile-size #:highlights move-hash)]
    ; Main rendering function
    [else
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
              (cc-superimpose tile (colorize glyph (~a player)))]
             ; If not, just return the empty tile
             [else
              tile]))))
     
     ; Smash together all the lists of lists
     (apply vc-append
            (for/list ([row (in-list tiles)])
              (apply hc-append row)))]))

