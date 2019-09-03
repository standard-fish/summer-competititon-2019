#lang slideshow

(require 2htdp/image)

(define (waffle img)
  
  (define two-p (hc-append img (rectangle 10 10 "solid" "PaleGoldenrod") img (rectangle 10 10 "solid" "PaleGoldenrod") img))
  
  (vc-append two-p (rectangle 10 10 "solid" "PaleGoldenrod") two-p (rectangle 10 10 "solid" "PaleGoldenrod") two-p))

(cc-superimpose

 (filled-rounded-rectangle 65 65 #:color "PaleGoldenrod" #:border-color "PaleGoldenrod")

 (waffle (rectangle 10 10 "solid" "Gold")))
 
