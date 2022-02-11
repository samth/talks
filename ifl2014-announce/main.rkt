#lang slideshow

(require unstable/gui/slideshow unstable/gui/ppict unstable/gui/pslide
         racket/draw images/compile-time (for-syntax racket/draw racket/class)
         "imgs.rkt")

(set-margin! 0)

(current-main-font "Ubuntu")

(define (bigt s) (scale (t s) 3))

(pslide #:go (coord .1 .1 'lt)
        (scale (t "IFL 2014") 3)
        (blank gap-size)
        (scale (t "Sam Tobin-Hochstadt") 2)
        
        #:alt
        (#:go (coord .1 .9 'lb) 
              (scale-to-fit nu-logo 300 400))
        #:go (coord .9 .9 'rb)
        (scale-to-fit iu-logo 300 300))

(pslide #:go (coord .1 .1 'lt)
        (scale (t "IFL 2014") 3)        
        #:go (coord .9 .9 'rb)
        (scale (t "?") 10))


(current-main-font (cons (make-object color% "white") "Ubuntu"))


(pslide 
 #:go (coord 0 0 'lt)
 (inset (scale-to-fit b (* 1.1 1280) (* 1.1 1024)) -150 0)
 #:go (coord .1 .1 'lt)
 (scale (t "IFL 2014") 3) 
 #:go (coord .9 .9 'rb)
 (scale (t "Boston, USA") 3))

(pslide 
 #:go (coord 0 0 'lt)
 (inset (scale-to-fit b2 (* 1.1 1280) (* 1.1 1024)) -150 0)
 #:go (coord .1 .1 'lt)
 (scale (t "IFL 2014") 3) 
 #:go (coord .9 .9 'rb)
 (scale (t "Boston, USA") 3))

(pslide 
 #:go (coord 0 0 'lt)
 (inset (scale-to-fit b3 (* 1.1 1280) (* 1.1 1024)) -150 0)
 #:go (coord .1 .1 'lt)
 (scale (t "IFL 2014") 3) 
 #:go (coord .9 .9 'rb)
 (scale (t "Boston, USA") 3))

(current-main-font (cons (make-object color% "black") "Ubuntu"))

(pslide 
 #:go (coord 0 0 'lt)
 (inset (scale-to-fit neu (* 1.1 1280) (* 1.1 1024)) 0 0)
 #:go (coord .1 .1 'lt)
 (scale (text "IFL 2014" (cons (make-object color% "white") "Ubuntu") (current-font-size)) 3) 
 #:go (coord .9 .9 'rb)
 (scale (t "Northeastern University") 2))

(current-main-font (cons (make-object color% "black") "Ubuntu"))

(pslide 
 #:go (coord 0 0 'lt)
 (inset (scale-to-fit wvh (* 1.1 1280) (* 1.1 1024)) 0 0)
 #:go (coord .1 .1 'lt)
 (scale (text "IFL 2014" (cons (make-object color% "white") "Ubuntu") (current-font-size)) 3) 
 #:go (coord .9 .9 'rb)
 (scale (t "Northeastern University") 2))

(pslide 
 #:go (coord .1 .1 'lt)
 (scale (text "IFL 2014" (cons (make-object color% "black") "Ubuntu") (current-font-size)) 3) 
 #:go (coord .8 .8 'rb)
 (scale (t "Tentative dates:") 1.8)
 (blank gap-size)
 (scale (t "Week of August 18th") 1.8))

(current-main-font (cons (make-object color% "white") "Ubuntu"))

(pslide 
 #:go (coord 0 0 'lt)
 (inset (scale-to-fit b4 (* 1.1 1280) (* 1.1 1024)) 0 0)
 #:go (coord .1 .1 'lt)
 (scale (t "IFL 2014") 3) 
 #:go (coord .9 .9 'rb)
 (scale (t "See you in Boston!") 3))

