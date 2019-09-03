#lang racket
(require pict)
(require racket/gui/base)
(require racket/random)

(define (show tag thing) (print (cons tag thing)) thing)

; (list 'random (random 1 10) (random 1 2) (random 1 234))
; (list 'chosen (random-ref '(a b c d e f)))
; (list 'nope)
; (show 'showtest 'showdatum)


; TODO:

; Attributes need to have constraints -- such as a list of colours to choose from, possibly with possibilities.
; And then further bindings, instead of choosing, could further restrict the list.  Or adjust probabilities.

; And sizes, such as length and width, need to be different.
; I'd like to be able to pass in size ranges.  and let the details of an image depend on whether they fit,
;   with the possibility if not producing an image at all if it's not possible.

; There's a lot of inconsistency about whether to explicitly pass associationlists as parameters.

; Some onject drawers insist on receiving such a parameter.
; Others manipulate functions that do that without taking an asociation list as paramter itself.
; There may be a natural distinction here; but I cross the boundary too easily.
; The low-level drawing code needs to know what is to be drawn; it takes this from an association list.
; The higher-level combiners need to manipulate functions that take an association list and produce functions that take association lists.



; Attribute management

; ali is an environment.  It should contain all the parameters that are neede to make a typical whatever (at the moment, the whatever is a door.)
; At the moment, also, it allows attributes to be defined as functions
;   at lookup time the entire association list is passed to the function so that its value can depend on other (not necessarily previous) parameters.

(define (looker name ali succeed fail)
  (letrec (
      (lookup2 (lambda (name aa succeed fail)
        (if (eq? aa '())
          (fail)
          (if (eq? name (caar aa))
            (let ((value (cdar aa)))
              (if (procedure? value)
        	  (succeed (value ali))
         	  (succeed value)
        	)
              )
            (lookup2 name (cdr aa) succeed fail)
          )
        )
      )))
    (lookup2 name ali succeed fail)
  )
)

(define (lookup name ali fail)
  (looker name ali (lambda (result) result) fail)
)
(define (old lookup name ali fail)
  (letrec (
      (lookup2 (lambda (name aa fail)
        (if (eq? aa '())
          (fail)
          (if (eq? name (caar aa))
            (let ((value (cdar aa)))
              (if (procedure? value)
        	  (value ali)
         	  value
        	)
              )
            (lookup2 name (cdr aa) fail)
          )
        )
      )))
    (lookup2 name ali fail)
  )
)

(define (binda name value a) (cons (cons name value) a))

(define (bind name value object)
  (lambda (a)
    (object (binda name value a))
  )
)

; Freezing is a mechanism for choosing parameters to be passed down to more local objects
;   so, for example, we can set a style for all the windows in a house, but allow each house to have its own style for windows.
;   This is accomplished by having the window-style be a function instead of a value.  The freeze calls the function and rebids the name to the result of that function.  The resulting associon list is passed down to lower objects.

(define (freezea name a)
  (let ((value (lookup name a (lambda () '()))))
    (if (eq? value '()) a (binda name value a))))
    
; Got the wrong definition for lookup.  Need a way for lookup to indicate success as well as failure.
;   Returning '() isn't enough.  I wonder what the right definition is.
; TAt the moment I never use freeze; onlu its curried varsions freezeo.  And I suspect I do not need freezaa.

(define (freeze name f a)
  ( let (( value (lookup name a (lambda () '())) ))
    (if (eq? value '())
      f
      (lambda (a) (f (binda name value a)))
    )
  )
)

(define (freezeo name object)
  ( lambda (a)
    (let (( value (lookup name a (lambda () '())) ))
    (if (eq? value '())
      (object a)
      (object (binda name value a))
    )
  ))
)

(define (with a name value) (cons (cons name value) a))


;  Graphics combinators

(define ( hor l )
  (if (cons? l)
    (if
      (null? (cdr l))
      (car l)
      (let
        ( [rest (hor (cdr l))] )
	(lambda (a) (hc-append ((car l) a) (rest a)))
      )
    )
    (print "ERROR: null list in hor")
  )
)  

(define ( vert l )
  (if (cons? l)
    (if
      (null? (cdr l))
      (car l)
      (let
        ( [rest (vert (cdr l))] )
	(lambda (a) (vc-append ((car l) a) (rest a)))
      )
    )
    (print "ERROR:null list in vert")
  )
)  
	

(define (horsep count object spacer a) ; object and spacer are functions taking alists.
  (if (equal? count 1) (object a) ; TODO: zero case
    (ht-append (object a) (spacer a) (horsep ( - count 1 ) object spacer a))
  )
)

(define (horsepp count object spacer)
  (lambda (a) (horsep count object spacer a))
  )

(define (spacer a)
  (blank 40 40)
  ; (filled-rectangle 40 40 #:color "white")
  )


; Architectural primitives

(define (window a)
  ( let ( [width (lookup 'width a (lambda () 100))]
          ; 10 isn't meant to be realistic.  It's meant to be ridiculous as a way of showing that something is wrong in the picture.
          [height (lookup 'height a (lambda () 10))]
          [style (lookup 'style a (lambda () 'paned))]
	)
     ; (show 'framed style) (show 'width width) (show 'height height)
        (cond
          [ (eq? style 'framed)
	    ( pin-over
	      (filled-rectangle (* width 1.00) (* height 1.00 ) #:color "red")
	      ( * width 0.05 ) ( * height 0.05 )
	      (filled-rectangle (* width 0.90) (* height 0.90 ) #:color "black")
	  )]
          [ (eq? style 'paned)
            ( pin-over
                  (filled-rectangle (* width 1.00) (* height 1.00 ) #:color "red")
                  ( * width 0.05 ) ( * height 0.05 )
                  (let ((w (lambda () (lambda (a) (window (binda 'style 'framed (binda 'width (* width 0.45) (binda 'height (* height 0.45)  a))))))))
                    ((vert (list (hor (list (w) (w))) (hor (list (w) (w))))) a)
                  )
          )]
          [ (eq? style 'plain)
              (filled-rectangle (* width 1.00) (* height 1.00 ) #:color "black")
            
          ]
        )
  )
)

(define (door a)
	(let
	    (
	      [width
	        (lookup 'doorwidth a (lambda () 100))
	      ]
	      [height (lookup 'doorheight a (lambda () 200))]
	    )
	    (pin-over
	      (pin-over
		(begin
		   (filled-rectangle width height #:color (lookup 'colour a (lambda(a) "gray")))
		)
                (* width 0.05)	(* height 0.05)
		(window (binda 'style 'paned (binda 'width ( * width 0.9 ) (binda 'height (* height 0.45 ) a))))
	      )
	    (* width 0.85) (* height 0.6)
	    (disk ( * width 0.10) #:color "yellow"))
	)
)

(define (dww a)
  (let
      [
       (width
        (begin (lookup 'doorwidth a (lambda () 100)))
        )
       (height (lookup 'doorheight a (lambda () 200)))
       (wall (lookup 'wall a (lambda () "lightgreen")))
      ]
    (define fw (bind 'width ( * width 0.9 ) (bind 'height (* height 0.45 ) window)))
    (define facade (random-ref (list
                 (ht-append (spacer a) (door a) (spacer a) (fw a) (spacer a) (fw a) (spacer a))
                 (ht-append (spacer a) (fw a) (door a)(spacer a) (fw a))
                 (ht-append (fw a) (fw a) (spacer a) (door a))
                 )))
    (pin-over (filled-rectangle (pict-width facade) (pict-height facade) #:color wall)
              0 0
              facade)
    ;   (ht-append (door a) (window a) (door a))
  )
)

(define (4doors a)
  (horsep 4 door spacer a)
)

(define (8doors a)
  (horsep 8 door spacer a)
)

; Test cases

(define colours '( "white" ; "red" "orange" "yellow" "chartreuse"
                           "green" "lightgreen" "darkgreen" "turquoise"
                           "blue" "lightblue" "darkblue" "purple" "gray"
                           "lightgray:" "darkgray"
                           "brown" "lightbrown" "darkbrown"
                           "black"))



(define alist (list
      (cons 'doorwidth 100 )
      (cons 'doorheight 200 )
      (cons 'colour (lambda (a) (random-ref colours)))
      (cons 'highlight (lambda (a) (random-ref colours)))
      (cons 'wall (lambda (a) (random-ref colours)))
      (cons 'style (random-ref '(paned framed))) ; 'plain is also a window style, but I won't choose it.
    )
)

; (lookup 'colour alist (lambda(a) "gray"))

; (print 'yup)

; (show 'alist alist)

; (print 'bar)
; (print "\n")

(define (stackdoors a)
  (vc-append
    (4doors a)
    (8doors a)
    ((freezeo 'colour 8doors) a)
))



; (show-pict ((hor (list door window door)) alist))

; (show-pict (horsep 3 door window alist))

; (show-pict (scale (stackdoors alist) 0.5))

(show-pict (scale (dww alist) 2.0))


(define e1 0)
(define e2 0)
(define e3 0)

(define (rantest)
         (for ([i (in-range 1 10000)])
           (let ((c (random-ref '(u v w))))
             (if (eq? c 'u) (set! e1 (+ e1 1))
                 (if (eq? c 'v) (set! e2 (+ e2 1))
                     (if (eq? c 'w) (set! e3 (+ e3 1))
                         '()
                         ))))))

(rantest)

(print (list 'alpha e1 e2 e3 'omega))
