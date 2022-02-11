#lang slideshow

;; change this to 1 to remove the bug
(define SCALE 2.5)

(slide (scale (t "P") SCALE))

(slide #:layout 'top
       (vc-append (titlet "AAAAAAAAA") 
                  (titlet "BBBBBBBBBBBBBBBBBBBBBBBBBBB") 
                  (t "M")))
(slide #:layout 'top
       (vc-append (titlet "AAAAAAAAA") 
                  (titlet "BBBBBBBBBBBBBBBBBBBBBBBBBBB") 
                  (t "M")
                  (t "M")))


