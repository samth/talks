#lang slideshow/widescreen
(require ppict ppict/slideshow pict/shadow
         lang-slide "helper.rkt")
(provide title-slide #;quotes-slide)

(define (pic fname [r 1])
  (pslide #:go (coord .5 .5 'cc)
        (scale (bitmap fname) r)))
(define (title-slide [year 2020] [con "tenth"])
  
  (pslide
   #:go (coord .5 .5 'cc) 
   (cellophane (scale (bitmap "plt-back.title.1024x768.png") 1.5) .5)
   #:go (coord 0.05 .9 'lc)
   (t/quat "Sam Tobin-Hochstadt" size2)
   (t/inc (~a "(chaperone (" con " RacketCon))") size2)
   #:go (coord 0.95 .35 'rc)
   (t/cant "State of Racket" (+ 55 size1))
   (t/cant (~a year) (+ 55 size1))
   ))

(define (qte author . s)
  (shadow-frame
   (vr-append 10
              (apply vl-append (map (λ (s) (t/cant s (- size3 5))) s))
              (t/cant (~a "    — " author) (- size3 6)))))
(module+ main (title-slide))
