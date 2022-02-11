#lang slideshow
(require unstable/gui/ppict unstable/gui/pslide pict/shadow
         lang-slide "helper.rkt")
(provide title-slide quotes-slide)

(define (pic fname [r 1])
  (pslide #:go (coord .5 .5 'cc)
        (scale (bitmap fname) r)))
(define (title-slide)
  
  (pslide
   #:go (coord .5 .5 'cc) 
   (cellophane (bitmap "luddy.jpg") .2)
   #:go (coord 0.05 .95 'lc)
   (t/quat "Sam Tobin-Hochstadt" size2)
   #:go (coord 0.05 .35 'lc)
   (t/cant "Evolving with Language" (+ 5 size1))
   ))

(define (qte author . s)
  (shadow-frame
   (vr-append 10
              (apply vl-append (map (λ (s) (t/cant s (- size3 5))) s))
              (t/cant (~a "    — " author) (- size3 6)))))
(define (quotes-slide)
  (slide (qte "David Parnas"
              "People write programs without any expectation"
              "that they will be right the first time.")
         (blank 100)
         #;(qte "Ludwig Wittgenstein"
                "The limits of my language form the limits of my world.")
         ;(blank 10)
         (qte "Paul Hudak" "A domain specific language is the ultimate abstraction.")))


