#lang slideshow

(require slideshow/step slideshow/code slideshow/face 
         unstable/gui/ppict unstable/gui/pslide
         (only-in slideshow/slide title-size)
          "config.ss"
         (except-in "beamer.ss" title) "lib.ss" racket/gui  "thanks.ss" 
         "tslide.ss" lang-slide "contracts.rkt"
         "ts-intro.rkt" "stages.rkt"
         "helper.rkt"
         racket/runtime-path (except-in mzlib/etc identity) unstable/gui/slideshow)


(require ppict-slide-grid)
;(set-grid-base-pict!)

(define c (coord .61 .55 'cc))

(provide chart)
(define (chart)
(pslide ;/title "Compiler Strategies"

 #:go c
 (colorize (rectangle 650 400) "black")
              #:go c
              (colorize (hline 650 1) "black")
              #:go c
              (colorize (vline 1 400) "black")
 ;; #:go (coord .15 .55 'lc)
 ;; (hline 700 2)
 ;; #:go (coord .65 .15 'ct)
 ;; (vline 3 475)


 #:go (coord .2 .4 'cc)
        (t/quat "OO" size1)
        #:go (coord .2 .7 'cc)
        (t/quat "FP" size1)

        #:go (coord .4 .2 'lc)
        (t/quat "AOT" size1)
        #:go (coord .77 .2 'cc)
        (t/quat "JIT" size1)

        #:go (coord .45 .7 'cc)
        (t/cant "GHC, Gambit" size2)
        (t/cant "MLton, SBCL" size2)

        #:go (coord .77 .4 'cc)
        (t/cant "v8, Self" size2)
        (t/cant "HotSpot" size2)

        #:next
        #:go (coord .45 .4 'cc)
        (t/cant "GCJ" size2)
        (t/cant "..." size2)

        #:next
        #:go (coord .77 .7 'cc)
        (t/cant "???" size2)


        )
)
