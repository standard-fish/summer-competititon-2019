#lang racket
(require json racket/draw math/base)

(define (lat-lon->map-point coordinates)
  (match-define (list lon lat _ ...) coordinates)
  (define-values (x y) (values (degrees->radians lon) (asinh (tan (degrees->radians lat)))))
  (list (/ (+ 1 (/ x pi)) 2) (/ (- 1 (/ y pi)) 2)))

(define (draw-polygon dc polygons)
  (define path
    (for/fold ([path #f]) ([polygon (in-list polygons)] #:unless (null? polygon))
      (define sub-path (new dc-path%))
      (send/apply sub-path move-to (lat-lon->map-point (car polygon)))
      (for ([point (in-list (cdr polygon))])
        (send/apply sub-path line-to (lat-lon->map-point point)))
      (send sub-path close)
      (if path (begin (send path append sub-path) path) sub-path)))
  (and path (send dc draw-path path 0 0)))

(define (make-geojson-bitmap gjdata width height)
  (define dc (new bitmap-dc% [bitmap (make-object bitmap% width height)]))
  (send* dc
    (set-scale width height)
    (set-smoothing 'smoothed)
    (set-pen (send the-pen-list find-or-create-pen "black" (* 0.5 (/ 1 width)) 'solid)))
  ;; Iterate over each feature (timezone) and render it
  (for ([feature  (in-list (hash-ref gjdata 'features))])
    (send dc set-brush (send the-brush-list find-or-create-brush "black" 'transparent))
    (let* ([geometry (hash-ref feature 'geometry (lambda () (hash)))]
           [data (hash-ref geometry 'coordinates (lambda () null))])
      (case (hash-ref geometry 'type #f)
        (("Polygon") (draw-polygon dc data))
        (("MultiPolygon") (for ([polygon (in-list data)]) (draw-polygon dc polygon)))
        (else (printf "Skipping ~a geometry" (hash-ref geometry 'type #f))))))
  (send dc get-bitmap))

(define world-data (call-with-input-file "./data/world-map.json" read-json))
(define bm (make-geojson-bitmap world-data 800 500))
(send bm save-file "world.png" 'png 100 #:unscaled? #t)
