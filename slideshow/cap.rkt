#lang racket

; baseball-cap.rkt
;
; Draw baseball caps using Racket's pict library
; Created for Stephen De Gabrielle's Summer 2019 standard-fish competition
;
; Modification History
; -------------------------------------------------
; 08/12/2019   Justin Zamora   Initial creation

(provide baseball-cap)

(require racket/contract)
(require racket/draw)
(require pict)

;-------------------------------------------------------

; Size of logo area on cap
(define LOGOWIDTH 160)
(define LOGOHEIGHT 80)

; Create a pict of a baseball cap.  Accept a color object or a color name.
(define/contract (baseball-cap color [logo (blank 1)] [scale? #t])
  (->* ((or/c string? (is-a?/c color%))) (pict? boolean?) pict?)

  ; Pict of the cap
  (define cap
    (dc (lambda (dc dx dy)
          ; Save the current pen and brush
          (define old-brush (send dc get-brush))
          (define old-pen (send dc get-pen))

          ; Set a new brush with the specified color
          (send dc set-brush (new brush% [style 'solid] [color color]))
          (send dc set-pen "Black" 2 'solid)

          ; Bill
          (let ([path (new dc-path%)])
            (send path move-to 68.96 143.2)
            (send path curve-to 68.96 143.2 -4.742 213.2 1.358 233.2)
            (send path curve-to 7.258 263.2 93.96 273.2 93.96 273.2)
            (send path curve-to 116.8 276 179 283.2 239 243.2)
            (send path curve-to 279 213.2 289 193.2 289 193.2)
            (send dc draw-path path dx dy))
      
          ; Crown
          (let ([path (new dc-path%)])
            (send path move-to 47.96 163)
            (send path curve-to 65.96 25.3 90.96 3.699 209 3.969)
            (send path curve-to 309 4.999 330.9 84.93 338.6 142.7)
            (send path curve-to 344 182.3 329 183.2 289 193.2)
            (send path curve-to 254.5 188.3 119 143.2 47.96 163.2)
            (send dc draw-path path dx dy))

          ; Center Seam
          (let ([path (new dc-path%)])
            (send path move-to 209 4.229)
            (send path curve-to 159 -0.4011 149 54.3 149 62.3)
            (send dc draw-path path dx dy))

          ; Left Seam
          (let ([path (new dc-path%)])
            (send path move-to 209 4.229)
            (send path curve-to 249 7.899 269 36.3 279 66.3)
            (send dc draw-path path dx dy))

          ; Button
          (send dc draw-ellipse 184 1 20 10)   

          ; Eyelets
          (send dc draw-ellipse 120 22 9 8)   
          (send dc draw-ellipse 198 28 9 8)   
          (send dc draw-ellipse 268 25 9 8)   

          ; Restore the original pen and brush
          (send dc set-brush old-brush)
          (send dc set-pen old-pen))
        342 278))

  ; Sanity check the size of the logo
  (when (or (zero? (pict-width logo))
            (zero? (pict-height logo)))
    (error 'baseball-cap "Logo has zero width or height"))

  ; Scale the logo and superimpose it in the center of the cap
  (let ([w (pict-width logo)]
        [h (pict-height logo)])
    (let ([factor (cond
                    [(false? scale?) 1] ; Don't scale if the argument was false
                    [(>= (/ w h) 1) (/ LOGOWIDTH w)] ; Logo is wider than high
                    [else (/ LOGOHEIGHT h)])]) ; Logo is higher than wide
      ; Center the logo in the logo box
      (let ([h-amt (/ (- LOGOWIDTH (* w factor)) 2)]
            [v-amt (/ (- LOGOHEIGHT (* h factor)) 2)])
        ; Shear and superimpose the logo, and return the result
        (cb-superimpose cap (shear (inset (inset (scale logo factor) h-amt v-amt)
                                          0 0 0 110)
                                   0 0.15))))))

; Test cases
(module+ main
  ; Create a blue cap with a "Racket" text logo
 (define racket-cap  (baseball-cap "LightSkyBlue" (text "Racket")))

  ; A cap with the standard-fish logo, scaled to fit the cap
  ;(baseball-cap "LightSalmon" (standard-fish 100 50))
  ; A standard-fish cap, with the logo unscaled
 (define fish-cap (baseball-cap "LightSalmon" (inset (standard-fish 100 50) 20) #f))

  (send (pict->bitmap racket-cap) save-file "racket-cap.png" 'png)	
  (send (pict->bitmap fish-cap) save-file "fish-cap.png" 'png)
  ; A little love for my alma mater. You need to install the iu-pict
  ; package using: raco pkg install iu-pict
 ; (require iu-pict)
  ;(baseball-cap (make-color 237 235 235) (iu-logo 80))
  )
