#lang slideshow

(require unstable/gui/ppict "config.rkt" "helper.rkt" "beamer.rkt"
         unstable/gui/pslide unstable/gui/pict/plt-logo pict/convert
         images/logos unstable/gui/pict)

(require ppict-slide-grid)
;(set-grid-base-pict!)

;; use this as the real title slide
#;
(pslide
 #:go (coord .5 .5 'cc) 
 (bitmap plt-background-path)
 #:go (coord 0.05 .85 'lc)
 (t/cant "Sam Tobin-Hochstadt" size2)
 #:go (coord 0.95 .95 'rc)
 (colorize (t/quat "Indiana University" size3)
           "midnightblue")
 #:go (coord 0.05 .15 'lc)
 (t/cant "Typed Racket" size1)
 #:go (coord 0.95 .25 'rc)
 (t/quat "A playground for language design" size2))

(define gap-small 0.125)
(define gap-big (+ gap-small 0.12))

(define (left n)
  (coord 0.025 (+ .05 (* (sub1 n) gap-big)) 'lc))
(define (right n [k 0.975])
  (coord (- k 0.025) (+ .05 gap-small (* (sub1 n) gap-big)) 'rc))

(pslide
 #:go (coord .5 .5 'cc) 
 (bitmap plt-background-path)
 #:go (left 1)
 (t/quat "Sam Tobin-Hochstadt" size1)
 #:go (left 3.5)
 (t/kau "Projects" size2)
 #:go (right 3.5)
 (t/quat "Typed Racket, Racket Contracts" size2)
 (t/quat "JIT Compilers, JS Modules" size2)
 #:go (left 2)
 (t/kau "Interests" size2)
 #:go (right 2)
 (t/quat "Gradual Types, Extensible Languages" size2)
 (t/quat "Shipping Code, Teaching Programming" size2)
)

(pslide
 #:go (left 1)
 (t/quat "Racket" size1)

 #:go (left 1.5)
 (t/kau "A programmable programming language" size2)

 #:go (right 2.)
 (t/quat "For building big and little languages" size3)
 #:go (right 2.5)
 (t/quat "Testbed for programming language research" size3)
 #:go (coord 0.2 .8 'cc) (bitmap (plt-logo))
 #:next
 #:go (coord .5 .5 'cc)
 (rotate (shadow-frame (t/quat "Shipping code!" size1)) (/ pi 6))
)

(pslide
 #:go (coord .5 .5 'cc)
 (bitmap "lastofus.jpg"))

(pslide
 #:go (left 1)
 (t/quat "Typed Racket" size1)

 #:go (left 1.5)
 (t/kau "A gradually-typed dialect of Racket" size2)

 #:go (right 2)
 (t/quat "Accomodates existing Racket programs" size2)
 (t/cant "[ICFP 2010, OOPSLA 2012, ESOP 2013]")
 #:go (right 3)
 (t/quat "Can use existing Racket libraries" size2)
 (t/cant "[DLS 2006, POPL 2008, OOPSLA 2012]")
 #:go (right 4)
 (t/quat "Uses types to optimize your program" size2)
 (t/cant "[IFL 2010, PLDI 2011, OOPSLA 2012]")

 )


(pslide
 #:go (left 1)
 (t/quat "Typed Racket" size1)

 #:go (left 1.5)
 (t/kau "A gradually-typed dialect of Racket" size2)

 #:go (right 2)
 (t/quat "The inspiration for Typed Clojure" size2)
 (t/cant "Developed by Ambrose Bonnaire-Sergeant")

 #:go (right 3)
 (t/quat "Extending to dependent types" size2)
 (t/cant "Work by Andrew Kent"))

(pslide 
 #:go (left 1)
 (t/quat "Pycket" size1)
 
 #:go (left 1.5)
 (t/kau "Do JIT compilers speed up functional languages?" size2)

 #:go (right 2)
 (t/quat "Using RPython to find out" size2)

 #:go (right 3)
 (t/quat "With Jeremy Siek and Spenser Bauman" size2)
 (t/cant "Joint with Kings College and HPI Potsdam")

 #:next 
 #:go (coord .5 .6 'cc)
 (scale (bitmap "current-norm-col-1.png") .7)
)


(pslide
 #:go (coord .5 .5 'cc) 
 (bitmap plt-background-path)

 #:go (coord .5 .4 'cc)
 (t/kau "Come talk to me!" size1)


 #:go (coord .5 .6 'cc)
 (t/inc "samth@cs.indiana.edu" size2)
 #:go (coord .5 .7 'cc)
 (t/inc "samth.github.io" size2)
 #:go (coord .5 .8 'cc)
 (t/inc "Lindley 230C" size2)
)
