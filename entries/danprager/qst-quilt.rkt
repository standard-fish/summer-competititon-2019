#lang racket

(require threading
         "quilt.rkt")

;; EXAMPLES
;;

;; Build a super-duper quilt from rectangles, hsts, squares, and a
;; few qsts

(define qst_ (qst 1 'black 'white))
(define qst__ (qst 1 'black 'black 'black 'white))
(define hst_ (hst 1 'black 'white 'forward))

(define (around-the-world . ss)
  ; error on (< (length ss) 3)
  (define ts (map (λ (s)
                    (if (symbol? s)
                        (square 1 s)
                        s))
                  ss))
  
(let around ([n(- (* 2 (length ss)) 3)])
  (define half (/ (- n 1) 2))
  (if (= n 3)
      (apply surround (take ts 3))
      (let* ([ps (take (drop ts half) (min (+ half 1)
                                           (- (length ts) half)))]
             [center (first ps)]
             [corner (last ps)]
             [ts (rest (reverse (rest ps)))]
             [side (apply beside 
                          (append (make-list (- half (length ts) 1)
                                             corner)
                                  ts))])
        (surround (around (- n 2))
                  (reflect side center)
                  corner)))))

(define (diamond center [surround 'white])
  (4-square (hst 1 surround center 'forward) R))

(define hst-strip
  (~> (reflect (beside/n 4 hst_) qst__)
      (B (H hst_))
      reflect))
    
(define b1 (around-the-world 'black
                             qst_
                             'cerise
                             'lupine
                             'caribbean
                             hst_))

(define b2 (around-the-world 'black
                             qst_
                             'amber
                             'cactus
                             'maize
                             hst_))

(define diamond-strip
  (beside/n 11 (diamond 'black) (diamond 'caribbean)))

(define geese
  (beside/n 9 (A (V hst_) hst_)))

(define quilt
  (~> (grid [b1 (R geese) b2]
            [geese (diamond 'maize 'cactus) (H geese)]
            [b2 (L geese) b1])
      (surround hst-strip hst_)
      (surround diamond-strip (diamond 'caribbean))
      (add-strips 'cactus)
      (add-strips 'caribbean)
      (add-strips 'black)))


(parameterize ([units 20]
               [show-outline #t])
  (draw quilt))
