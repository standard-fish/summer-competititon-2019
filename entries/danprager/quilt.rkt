#lang racket

(require racket/generic
         racket/random
         threading
         (prefix-in im: 2htdp/image)
         "konas.rkt")

(provide units            ; parameters
         show-outline
         separate-blocks

         rectangle        ; basic shapes
         square
         hst
         hrt
         qst

         width            ; queries
         height

         rotate-right     ; operators
         rotate-left
         flip-vertical
         flip-horizontal
         scale
         
         above            ; combinators
         above/n
         beside
         beside/n
         reflect
         beside/reflect
         block
         grid
         checkerboard
         
         add-strip
         add-strips
         4-square
         pinwheel
         surround

         draw             ; image generation

         A B L R H V      ; shorthand
         )


;; UTILITIES

; Pre-condition check
; pre/f : Boolean Symbol Any Any -> Argument-Error | Void
(define (pre/f name test? expected v)
  (unless test?
    `p(raise-argument-error name expected v)))

; Specify multiple pre-conditions for a single function
; pre : Symbol (ListOf Boolean Any Any) -> Argument-Error | Void
(define-syntax-rule (pre name args (test? expected v) ...)
  (begin
    (pre/f name test? expected v) ...))

; Define a function without arguments and cache the result
; Useful for pre-conditions
; define/once: -> Any
(define-syntax-rule (define/once (name) (body ...))
  (define name
    (let ([result 'dummy])
      (λ ()
        (when (equal? result 'dummy)
          (set! result (body ...)))
        result))))


;; PARAMETERS
(define units (make-parameter 10))
(define show-outline (make-parameter #f))
(define separate-blocks (make-parameter #f))

#;(define palette-colors (make-parameter '(black medium-grey red blue green pink)))

; palette-color: Integer -> Symbol | Color
#;(define (palette-color i)
    (list-ref (palette-colors) (modulo i (length (palette-colors)))))

;; Kona Colors

(define (draw-rectangle w h mode color)
  (im:rectangle w h mode (name->kona color)))

(define (draw-square w mode color)
  (draw-rectangle w mode color))

(define (draw-right-triangle w h mode color)
  (im:right-triangle w h mode (name->kona color)))


;; SHAPES

(define-generics shape
  (shape-width shape)
  (shape-height shape)
  (shape-rotate-right shape)
  (shape-rotate-left shape)
  (shape-flip-vertical shape)
  (shape-flip-horizontal shape)
  (shape-scale mag shape)
  (shape-draw shape)
  (shape-draw-outline shape))

; A rectangle of width w, height h, and color c
(struct rect (width height color)
  #:transparent
  #:methods gen:shape
  [(define (shape-width r) (rect-width r))
   (define (shape-height r) (rect-height r))
   (define/match (rotate r) [((rect w h c)) (rect h w c)])
   (define shape-rotate-right rotate)
   (define shape-rotate-left rotate)
   (define shape-flip-vertical identity)
   (define shape-flip-horizontal identity)
   (define/match (shape-scale mag r) [(mag (rect w h c)) (rect (* mag w) (* mag h) c)])
   
   (define (draw w h mode c) (draw-rectangle  (* (units) w) (* (units) h) mode c))
   (define/match (shape-draw r) [((rect w h c)) (draw w h 'solid c)])
   (define/match (shape-draw-outline r) [((rect w h _)) (draw w h 'outline 'medium-grey)])])
   

(struct hrt-rect rect (color2 slope)
  #:transparent
  #:methods gen:shape
  [(define (shape-width r) (rect-width r))
   (define (shape-height r) (rect-height r))
   (define/match (shape-rotate-right r)
     [((hrt-rect w h c1 c2 'forward)) (hrt-rect h w c2 c1 'backward)]
     [((hrt-rect w h c1 c2 'backward)) (hrt-rect h w c1 c2 'forward)])
   (define/match (shape-rotate-left r)
     [((hrt-rect w h c1 c2 'forward)) (hrt-rect h w c1 c2 'backward)]
     [((hrt-rect w h c1 c2 'backward)) (hrt-rect h w c2 c1 'forward)])
   (define/match (shape-flip-horizontal r)
     [((hrt-rect w h c1 c2 'forward)) (hrt-rect w h c2 c1 'backward)]
     [((hrt-rect w h c1 c2 'backward)) (hrt-rect w h c2 c1 'forward)])
   (define/match (shape-flip-vertical r)
     [((hrt-rect w h c1 c2 'forward)) (hrt-rect w h c1 c2 'backward)]
     [((hrt-rect w h c1 c2 'backward)) (hrt-rect w h c1 c2 'forward)])
   (define/match (shape-scale mag r)
     [(mag (hrt-rect w h c1 c2 slope)) (hrt-rect (* mag w) (* mag h) c1 c2 slope)])
   
   (define (draw w h mode c1 c2)
     (im:overlay
      (draw-right-triangle  (* (units) w) (* (units) h) mode c1)
      (im:rotate 180 (draw-right-triangle  (* (units) w) (* (units) h) mode c2))))
   (define/match (shape-draw r)
     [((hrt-rect w h c1 c2 'forward)) (im:rotate -90 (draw h w 'solid c1 c2))]
     [((hrt-rect w h c1 c2 'backward)) (draw w h 'solid c1 c2)])
   (define/match (shape-draw-outline r)
     [((hrt-rect w h c1 c2 'forward)) (im:rotate 90 (draw h w 'outline 'medium-grey 'medium-grey))]
     [((hrt-rect w h c1 c2 'backward)) (draw w h 'outline 'medium-grey 'medium-grey)])])

; rectangle: Integer Integer (Opt Color) -> rect
(define (rectangle w h [c 'black])
  (pre 'rectangle
       [(exact-positive-integer? w) "positive-integer? w" w]
       [(exact-positive-integer? h) "positive-integer? h" h]
       [(symbol? c) "symbol? c" c])
  (rect w h c))

; square: (Opt Integer) (Opt Color) -> rect
(define (square [w 1] [c 'black])
  (pre 'square
       [(exact-positive-integer? w) "positive-integer? w" w]
       [(symbol? c) "symbol? c" c])
  (rect w w c))


; rectangular block made out of two half-rectangle triangles
; hrt:  Integer Integer (Opt Color) (Opt Color) (Opt 'forward|'backward) -> hrt-rect
(define (hrt w h [c1 'black] [c2 'white] [slope 'forward])
  (pre 'hrt
       [(exact-positive-integer? w) "width is a positive-integer" w]
       [(exact-positive-integer? h) "height is as positive-integer" h]
       [(symbol? c1) "symbol? c1" c1]
       [(symbol? c2) "symbol? c2" c2]
       [(member slope '(forward backward)) "slope is 'forward or 'backward" slope])
  (hrt-rect w h c1 c2 slope))

; square block made out of two half-square triangles
; hrt:  Integer  (Opt Color) (Opt Color) (Opt 'forward|'backward) -> hrt-rect
(define (hst w [c1 'black] [c2 'white] [slope 'forward])
  (hrt w w c1 c2 slope))



(struct qst-rect (width l t b r)
  #:transparent
  #:methods gen:shape
  [(define (shape-width r) (qst-rect-width r))
   (define (shape-height r) (qst-rect-width r))
   (define/match (shape-rotate-right r)
     [((qst-rect w l t r b)) (qst-rect w b l t r)])
   (define/match (shape-rotate-left r)
     [((qst-rect w l t r b)) (qst-rect w t r b l)])
   (define/match (shape-flip-vertical r)
     [((qst-rect w l t r b)) (qst-rect w l b r t)])
   (define/match (shape-flip-horizontal r)
     [((qst-rect w l t r b)) (qst-rect w r t l b)])
   (define/match (shape-scale mag r)
     [(mag (qst-rect w l t r b)) (qst-rect (* mag w) l t r b)])
   
   (define (draw w mode c)
     (let ([side (* w (units) (sqrt 1/2))])
       (draw-right-triangle side side mode c)))
   (define (arrange l t r b)
     (im:overlay (im:beside (im:rotate 135 l) (im:rotate -45 r))
                 (im:above (im:rotate 45 t) (im:rotate 225 b))))
   (define/match (shape-draw r)
     [((qst-rect w l t r b)) (arrange (draw w 'solid l) (draw w 'solid t)
                                      (draw w 'solid r) (draw w 'solid b))])
   (define/match (shape-draw-outline r)
     [((qst-rect w l t r b)) (let ([q (draw w 'outline 'medium-grey)])
                               (arrange q q q q))])])

; square block made out of four half-rectangle triangles
; qst:  Integer (Opt Color) (Opt Color) (Opt Color) (Opt Color) -> qst-rect
(define (qst w [left 'black] [top 'white] [right left] [bottom top])
  (pre 'qst
       [(exact-positive-integer? w) "width is a positive-integer" w]
       [(symbol? left) "symbol? c1" left]
       [(symbol? top) "symbol? c2" top]
       [(symbol? right) "symbol? c3" right]
       [(symbol? bottom) "symbol? c4" bottom])
  (qst-rect w left top right bottom))


;; COMBINATORS

; Stack equal width rectangles and composite shapes
; above: Shapes ... -> Shape
(define (above . rs)
  (define/once (widths) (map width rs))
  (define/once (equal-widths?) (= (apply min (widths)) (apply max (widths))))
  #;(when (and (not (null? rs)) (not (equal-widths?)))
    (displayln (list 'above
                     (apply im:above/align 'left
                            (~> (map draw rs)
                                (add-between (im:square 3
                                                        'solid
                                                        (im:color 0 0 0 0))))))))

  ;(displayln (length rs))
      
  (pre 'above
       [(not (null? rs)) "Non-empty list" rs]
       [(equal-widths?) "Equal widths" (widths)])

  (match rs
    [(list a) a]
    [(list rs ...) (cons 'above rs)]))

(define (above/n n . rs)
  (apply above
         (for/list ([i (in-range n)]
                    [s (in-cycle rs)])
           s)))

; Combine equal height rectangles and composite shapes from left to right
; beside: Shapes ... -> Shape
(define (beside . rs)
  ;(newline)
  ;(displayln (cons 'beside (map draw rs)))
  
  (define/once (heights) (map height rs))
    
  (pre 'beside
       [(not (null? rs)) "Non-empty list" rs]
       [(= (apply min (heights)) (apply max (heights)))
        "Equal heights" (heights)])

  (define result
    (match rs
      [(list a) a]
      [(list rs ...) (cons 'beside rs)]))

  ;(displayln (draw result))
  result)

(define (beside/n n . rs)
  (apply beside
         (for/list ([i (in-range n)]
                    [s (in-cycle rs)])
           s)))

(define (beside/reflect center . rs)
  (define strip (apply beside rs))
  (if center
      (beside (H strip) center strip)
      (beside (H strip) strip)))

(define (reflect a [center #f])
  (if center
      (beside a center (H a))
      (beside a (H a))))

(define (checkerboard rows cols a b)
  (define r1 (beside/n cols a b))
  (define r2 (beside/n cols b a))
  (above/n rows r1 r2))

(define (my-ref xs n [randomize? #f])
  (cond [(list? xs) (if randomize?
                        (random-ref xs)
                        (list-ref xs (modulo n (length xs))))]
        [else xs]))

(define (sash-and-post rows cols centre h-sash post [v-sash (R h-sash)])
  (define top-left (A (B post h-sash)
                      (B v-sash centre)))
  (define top-right (B top-left (A post v-sash)))
  (define bottom-left (A top-left (B post h-sash)))
  (define bottom-right (B bottom-left (A post v-sash post)))
  (define top-row (B (beside/n (- cols 1) top-left) top-right))
  (define bottom-row (B (beside/n (- cols 1) bottom-left) bottom-right))

  (A (above/n (- rows 1) top-row) bottom-row))
           

; Tag a Shape for later construction as a cohesive block
; block: Shape -> Shape
(define (block s)
  (match s
    [(list 'block t) s]       ; (block (block t)) -> (block t)
    [else (list 'block s)])) 

; Combine a list of list of blocks using above and beside
; in a kind of matrix notation.
; E.g. (blocks [A B]     (above (beside A B)
;              [C D]) ->        (beside C D))
;
; blocks: (ListOf (ListOf Shape)) -> Shape
(define-syntax-rule (grid [s ...] ...)
  (above (beside s ...) ...))

; 4-square: Shape (Opt (Shape -> Shape)) -> Shape
(define (4-square b1 [op identity])
  (define b2 (op b1))
  (define b3 (op b2))

  (grid [b1 b2]
        [(op b3) b3]))

(define (pinwheel n a b)
  (4-square (hst (/ n 2) a b 'backward) R))

; Surround a center piece with a border built out of a side and
; corner pieces.
;
; surround: Shape Shape Shape -> Shape
(define (surround center side      corner)
  (grid [corner   side             (R corner)]
        [(L side) center           (R side)]
        [(L corner)   (R (R side)) (R (R corner))]))


; Add a strip of width w and color c to an existing shape
; add-strip: Shape '(left|right|top|bottom) Color -> Shape
(define (add-strip shape where c [w 1])
  (define (v) (rectangle w (height shape) c))
  (define (h) (rectangle (width shape) w c))

  (match where
    ['left (beside (v) shape)]
    ['right (beside shape (v))]
    ['top (above (h) shape)]
    ['bottom (above shape (h))]))

; Add a strip of width 1 and color c to an existing shape
; add-strip: Shape Color Color Color Color -> Shape
(define (add-strips s left [top left] [right left] [bottom top])
  (~> s
      (add-strip 'left left)
      (add-strip 'top top)
      (add-strip 'right right)
      (add-strip 'bottom bottom)))


;; QUERIES

; width: shape -> Integer
(define (width shape)
  (match shape
    [(list 'block s) (width s)]
    [(list 'above a ss ...) (width a)]
    [(list 'beside ss ...) (apply + (map width ss))]
    [else (shape-width shape)]))

; height: shape -> Integer
(define (height shape)
  (match shape
    [(list 'block s) (height s)]
    [(list 'above ss ...) (apply + (map height ss))]
    [(list 'beside a ss ...) (height a)]
    [else (shape-height shape)]))


;; OPERATORS

; Geometric rotation of a possibly composite shape
; rotate-right: Shape -> Shape
(define (rotate-right shape)
  (match shape
    [(list 'block s) (list 'block (rotate-right s))]
    [(list 'above ss ...) (cons 'beside (reverse (map rotate-right ss)))]
    [(list 'beside ss ...) (cons 'above (map rotate-right ss))]
    [else (shape-rotate-right shape)]))

; Geometric rotation of a possibly composite shape
; rotate-left: Shape -> Shape
(define (rotate-left shape)
  (match shape
    [(list 'block s) (list 'block (rotate-left s))]
    [(list 'above ss ...) (cons 'beside (map rotate-left ss))]
    [(list 'beside ss ...) (cons 'above (reverse (map rotate-left ss)))]
    [else (shape-rotate-left shape)]))

; Mirror image swapping left and right
; flip-horizontal: Shape -> Shape
(define (flip-horizontal shape)
  (match shape
    [(list 'block s) (list 'block (flip-horizontal s))]
    [(list 'above ss ...) (cons 'above (map flip-horizontal ss))]
    [(list 'beside ss ...) (cons 'beside (map flip-horizontal (reverse ss)))]
    [else (shape-flip-horizontal shape)]))

; Mirror image swapping top and bottom
; flip-vertical: Shape -> Shape
(define (flip-vertical shape)
  (match shape
    [(list 'block s) (list 'block (flip-vertical s))]
    [(list 'above ss ...) (cons 'above (map flip-vertical (reverse ss)))]
    [(list 'beside ss ...) (cons 'beside (map flip-vertical ss))]
    [else (shape-flip-vertical shape)]))


; scale : Integer Shape -> Shape
(define (scale mag shape)
  (pre 'scale
       [(exact-positive-integer? mag)
        "Positive integer magnification" mag])
  (define (S s) (scale mag s))
  (match shape
    [(list 'block s) (list 'block (scale mag s))]
    [(list 'above ss ...) (cons 'above (map S ss))]
    [(list 'beside ss ...) (cons 'beside (map S ss))]
    [else (shape-scale mag shape)]))


;; INTERPRETER - drawing, etc.

; Optionally add some space around the Shape
; draw-block: Shape -> Image
(define (draw-block s)
  (let ([im (draw s)])
    (if (separate-blocks)
        (im:overlay im
                    (draw-rectangle (+ (im:image-width im) (* 2 (units)))
                                    (+ (im:image-height im) (* 2 (units)))
                                    'solid 'white))
        im)))

; draw: Shape -> Image
(define (draw shape)
  (match shape
    [(list 'block s) (draw-block s)]
    [(list 'above shapes ...) (apply im:above (map draw shapes))]
    [(list 'beside shapes ...) (apply im:beside (map draw shapes))]
    [else (define r (shape-draw shape))
          (if (show-outline)
              (im:overlay (shape-draw-outline shape) r)
              r)]))


;; SHORTHAND for common operators and combinators

(define A above)
(define B beside)
(define L rotate-left)
(define R rotate-right)
(define H flip-horizontal)
(define V flip-vertical)






(module+ test
  (draw
   (~> (checkerboard 5 5
                     (pinwheel 6 'ash 'black)
                     (square 6 'ash))
       (surround (beside/n 10
                           (hst 3 'ash 'red 'forward)
                           (hst 3 'red 'ash 'backward))
                 ;(hst 3 'ash 'black 'forward)
                 ;(hst 3 'black 'ash 'backward))
                 (square 3 'ash))
       (add-strips 'black)))


  (newline)

  (parameterize ([show-outline #t])
    (draw
     (sash-and-post 5 5
                    (rectangle 7 4 'ash)
                    (A (rectangle 7 1 'red)
                       (rectangle 7 2 'ash)
                       (rectangle 7 1 'red))
                    (rectangle 3 4 'ash)
                    (B (rectangle 1 4 'red)
                       (rectangle 1 4 'ash)
                       (rectangle 1 4 'red)))))


  (parameterize ([show-outline #t])
    (draw
     (~> (sash-and-post 5 5
                        (qst 5 'chartreuse 'candy-pink)
                        (rectangle 5 1 'snow)
                        (square 1 'candy-pink))
         (add-strips 'coal))))

  (define (s-and-p center post sash)
    (define r (rectangle (width center) 1 sash))
    (A (B (square 1 post) r)        
       (B (R r) center)))

  (define qst-block (qst 5 'chartreuse 'candy-pink))

  (define right-strip (above/n 10
                               (square 1 'ash)
                               (rectangle 1 5 'snow)
                               (square 1 'candy-pink)
                               (rectangle 1 5 'snow)))

  (define bottom-strip (B (L right-strip) (square 1 'candy-pink)))

  (parameterize ([show-outline #t])
    (draw
     (~> (checkerboard 5 5
                       (s-and-p qst-block 'candy-pink 'snow)
                       (s-and-p qst-block 'ash 'snow))
         (B right-strip)
         (A bottom-strip)
         (add-strips 'coal)))))
                
          




        

                

