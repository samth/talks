#lang racket

(require slideshow/step slideshow/code slideshow/face 
         unstable/gui/ppict unstable/gui/pslide
         (only-in slideshow/slide title-size)
          "config.ss"
         (except-in "beamer.ss" title) "lib.ss" racket/gui  "thanks.ss" 
         "tslide.ss" lang-slide/hudak-quote "contracts.rkt"
         "ts-intro.rkt" "stages.rkt"
         "helper.rkt"
         racket/runtime-path (except-in mzlib/etc identity) unstable/gui/slideshow)
(require slideshow pdf-read)

(define (go start end [clip? #t] #:title [title #f])
  (for ([i (in-range (sub1 start) end 1)])
    (if title
        (pslide #:go (coord .5 .5 'cc)
            (page->pict (pdf-page "vanhorn-icfp2014.pdf" i))
            #:go (coord 1 0 'rt)
            ((if clip? values ghost) (filled-rectangle 200 65 #:color "white" #:draw-border? #f))
            #:go (coord 0.05 0.05 'lc)
            (t/quat title size2))
        (pslide #:go (coord .5 .5 'cc)
                (page->pict (pdf-page "vanhorn-icfp2014.pdf" i))
                #:go (coord 1 0 'rt)
                ((if clip? values ghost) (filled-rectangle 200 65 #:color "white" #:draw-border? #f))))))

(provide go)

