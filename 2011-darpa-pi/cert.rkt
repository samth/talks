#lang at-exp slideshow

(require slideshow/step slideshow/code slideshow/face 
         (only-in slideshow/slide title-size)
         (except-in "beamer.ss" title) "lib.ss" racket/gui  "thanks.ss" 
         "tslide.ss" "config.ss" "langs/main.rkt"
         "ts-intro.rkt" "stages.rkt" unstable/gui/slideshow
         unstable/gui/pict racket/runtime-path
         "vulns.rkt" "gnosys.rkt")
(provide cert)

(define-runtime-path cert.png "cert.png")
(define-runtime-path cert-fixed "cert-fixed.png")

(define (cert)  
  (slide/staged [one two] (cc-superimpose
                           (inset (bitmap cert.png) -20)
                           (pict-if (= two stage)
                                    (shadow-frame #:background-color "white" #:frame-color "red" 
						  #:frame-line-width 4
						  (scale (colorize (t "48 vulnerabilities") "red") 2))
                                    (blank))))
  
  (slide (inset (bitmap cert-fixed) -20)))