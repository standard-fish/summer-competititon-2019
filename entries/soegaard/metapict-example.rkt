#lang racket
(require metapict)

; bitmap size
(set-curve-pict-size  800 800)

; logical coordinatesystem  xmin=-2, xmax=2, ymin=-2, ymax=2
(curve-pict-window (window -2 2 -2 2))

(for/draw ([i 100])
  (color (color-med (* 0.01 i) "red" "blue")
         (fill
          (rotated (* 0.54 i)
                   (scaled (* 0.01 (- 100 i))
                           unitsquare)))))
