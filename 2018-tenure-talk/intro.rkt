#lang slideshow
(require slideshow/step slideshow/code slideshow/face 
         unstable/gui/ppict unstable/gui/pslide
         (only-in slideshow/slide title-size)
          "config.ss"
         (except-in "beamer.ss" title) "lib.ss" racket/gui  "thanks.ss" 
         "tslide.ss" lang-slide "contracts.rkt"
         "ts-intro.rkt" "stages.rkt"
         "helper.rkt"
         racket/runtime-path (except-in mzlib/etc identity) 
         unstable/gui/slideshow)




#;
(tslide* (let ([pict (t/section "The Rise of Dynamic Languages")])
           (pin-line pict pict lc-find pict rc-find #:line-width 10)))


(parameterize ([pslide-base-pict
                (lambda () (blank 1024 768))])

(pslide #:go (coord .5 .5 'cc)
        (scale (bitmap "nyc.png") .9)
        #:next
        #:go (coord 3/4 3/4 'cc)
        (shadow-frame (t/cant "JavaScript" 50)
                      #:shadow-descent 5)))
(tslide* "The Rise of Dynamic Languages")

(pslide #:go (coord .5 .5 'cc)
        (scale (bitmap "angry-birds.png") 0.9)
        ;#:next
        #:go (coord 1/4 3/5 'cc)
        (shadow-frame (t/cant "Lua" 50)
                      #:shadow-descent 5))

(pslide #:go (coord .5 .5 'cc)
        (scale (bitmap "pymol.jpg") 0.9)
        ;#:next
        #:go (coord 1/4 3/5 'cc)
        (shadow-frame (t/cant "Python" 50)
                      #:shadow-descent 5))

#;
(pslide #:go (coord .5 .5 'cc)
        (scale (bitmap "epfl3.png") 1)
        ;#:next
        #:go (coord 3/4 2/5 'cc)
        (shadow-frame (t/cant "PHP" 50)
                      #:shadow-descent 5))


  
;; swedish pension
(pslide #:go (coord -0.05 -0.05 'lt)
        (scale (bitmap "sweden2.png") 0.7)
        ;#:next
        #:go (coord 3/4 3/4 'cc)
        (shadow-frame (t/cant "Perl" 50)
                      #:shadow-descent 5))

  
  
(slide #:layout 'center
        (scale (t "“whipitupitude” —  Larry Wall") 1.6))

 (tslide* "So what's the problem?")

 (slide #:layout 'center
        (scale (t "“whipitupitude” —  Larry Wall") 1.6))
  
  

