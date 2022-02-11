#lang slideshow

(require slideshow/step slideshow/code slideshow/face 
         ppict/2 pict/shadow
         (only-in slideshow/slide title-size)
         "config.ss"
         (except-in "beamer.ss" title) "lib.ss" "thanks.ss" 
         "tslide.ss" "stages.rkt"
         "helper.rkt" (prefix-in p: racket-poppler)
         racket/runtime-path rsvg
         (except-in mzlib/etc identity)
         unstable/gui/slideshow)
(require slideshow/play)

(define (pic fname [r 1])
  (pslide #:go (coord .5 .5 'cc)
          (scale (bitmap fname) r)))

(define (page->pict pth [page 0] #:scale [scl 1])
  (bitmap (scale (p:page->pict (p:pdf-page (p:open-pdf pth) page)) scl)))

(current-title-background-pict  (pin-over
                                 (current-background-pict)
                                 0 0
                                 (cellophane (bitmap
                                              (load-svg-from-file
                                               "Indiana_University_seal.svg" 6))
                                             0.1)))
                                 
(title `("Optimizing and Inferring""Gradual Types")
       `("The View from Indiana")
        '()
        "Boston, 2018")

(tslide "The project at IU")

(slide #:title "People"
       (ht-append 60
        (apply vl-append
         (map t/quat
              '("Matteo Cimini"
                "David Christiansen"
                "Spenser Bauman"
                "David Kempe"
                "Earl Dean")))
        (apply vl-append
         (map t/quat
              '("Ambrose Bonnaire-Sergeant"
                "Andrew Kent"
                "Andre Kuhlenschmit"
                "Deyaaeldeen Almahallawi"
                "Michael Vitousek"
                "Sarah Spall"
                "Caner Derici"
                )))))

(pic "leo.jpg")

(slide #:title "Papers"
       #:layout 'center
       'alts
       (map (compose list
                     bitmap
                     (lambda (x) (scale x 0.8))
                     page->pict
                     (lambda (x) (string-append "../../papers/" x)))
            '("pycket-papers/oopsla-2017/paper.pdf"
              "pycket-papers/icfp-2015/pycket.pdf"
              "esop16-short.pdf"
              "icfp17-main102.pdf"
              "1802.06375.pdf"
              "p762-vitousek.pdf"
              "p789-cimini.pdf"
              "trpp-pldi-2016/kkth-pldi-2016.pdf")))

(slide #:title "Software"
       (scale
        (apply vl-append
               (map t/quat
                    '("Typed Racket"
                      "Reticulated Python"
                      "Typed Clojure"
                      "Grift"
                      "Pycket"
                      "Gradualizer")))
        1.6))
        

(tslide "Brief Updates")


(slide #:title "Reticulated Python"
       (scale (bitmap "lattices.png") .7))

(slide #:title "Typed Clojure"
       (scale (bitmap "spec.png") .7))


(define (tr-slow)

(tslide "Typed Racket has a problem")

(define (quoted author ts)
  (vr-append (apply vl-append (for/list ([t ts])
                                (t/cant t size2)))
             (blank 30)
             (t/kau (string-append "" author) size2)))

(slide (quoted "Vincent St. Amour, Dec. 2015"
               '("Interfacing Typed Racket and Racket code"
                 "may involve a lot of dynamic checks, which"
                 "can have significant overhead, and cause"
                 "that kind of stuttering.")))

(slide (quoted "Neil Toronto, May 2015"
               '("It's very slow."
                 ""
                 "It looks like it has to do with a dc<%>"
                 "instance crossing from untyped to"
                 "typed code.")))


(slide (quoted "John Clements, January 2016"
               '("... the resulting dynamic checks"
                 "will probably cause them to be"
                 "unacceptably slow.")))


(pslide
 #:go (coord .5 .0 'ct)
 (page->pict "popl16-tfgnvf.pdf" #:scale 1.8)
 #:next
 #:go (coord .5 .5 'cc)
 (shadow-frame
  (vl-append
   (t/cant "The problem is that, according to our measurements,"
           size3)
   (t/cant "the cost of enforcing soundness is overwhelming."
           size3)))
 #:next
 #:go (coord .5 .5 'cc)
 (shadow-frame
  (scale (bitmap "synth.png") .8))
 #:go (coord .5 .5 'cc)
 #:next
 (shadow-frame
  (cc-superimpose
   (blank 1008 450)
   (apply vl-append
          (for/list ([t '("We find that Typed Racket’s cost of"
                          "soundness is not tolerable. If applying"
                          "our method to other gradual type system"
                          "implementations yields similar results,"
                          "then sound gradual typing is dead.")])
            
            (t/cant t size3)))))

 )




(pic "princess.jpg")

(tslide "Enter Pycket")

(pic "aggregate.png" .8)

(slide #:title "Synth, again"
       #:layout 'center
       (page->pict #:scale 1.4 "slowdown-synth-warmup-0.pdf"))

(slide #:title (t/quat "Two Key Ideas" size2)
 (item (t/cant "Tracing JIT Compilation" size2))
 (blank 125)
 (item (t/cant "Hidden Classes for Chaperones" size2)))

(tslide "Tracing to the rescue")

;(code-colorize-enabled #t)

(slide #:layout 'center
 (code (define f (cast #,(red-code (λ (x) (+ x 1)))
                       (-> Integer Integer))))
 'next!
 (arrow 30 (- (/ pi 2)))
 'alts
 (list
  (list
   (code (define (f x*)
           (cast (#,(red-code (λ (x) (+ x 1))) (cast x* Integer))
                 Integer))))
  (list
   (code
    (define f* (checked-fn #,(red-code (λ (x) (+ x 1)))
                          Integer Integer))
    (define (f x*)
      (cast ((checked-fn-op f)
             (cast x* (checked-fn-domain f)))
            (checked-fn-range f)))))))

(slide #:title (code (f x))
       (vl-append
        (code |var f_code = (checked-fn-op f);|)
        (code |var f_dom = (checked-fn-domain f);|)
        (code |var f_rng = (checked-fn-rng f);|)
        (code ||)
        (code |guard (integer? x)|)
        (code ||)
        (code |var res = x + 1;|)
        (code |guard (integer? res)|)
        (code ||)
        (code |return res;|)))

(slide #:title (code (f x))
       (vl-append
        (cellophane (code |var f_code = (checked-fn-op f);|) .3)
        (cellophane(code |var f_dom = (checked-fn-domain f);|) .3)
        (cellophane(code |var f_rng = (checked-fn-rng f);|) .3)
        (code ||)
        (code |guard (integer? x)|)
        (code ||)
        (code |var res = x + 1;|)
        (code |guard (integer? res)|)
        (code ||)
        (code |return res;|)))

(slide #:title (code (f x))
       (vl-append
        (cellophane (code |var f_code = (checked-fn-op f);|) .3)
        (cellophane(code |var f_dom = (checked-fn-domain f);|) .3)
        (cellophane(code |var f_rng = (checked-fn-rng f);|) .3)
        (code ||)
        (code |guard (integer? x)|)
        (code ||)
        (code |var res = x + 1;|)
        (cellophane (code |guard (integer? res)|) .3)
        (code ||)
        (code |return res;|)))

(define slowdowns
  '(("slowdown-gregor-warmup-0.pdf"
     "slowdown-kcfa-warmup-0.pdf"
     "slowdown-mbta-warmup-0.pdf")
    ("slowdown-morsecode-warmup-0.pdf"
     "slowdown-sieve-warmup-0.pdf"
     "slowdown-snake-warmup-0.pdf")
    ("slowdown-suffixtree-warmup-0.pdf"
     "slowdown-zordoz-warmup-0.pdf"
     "slowdown-tetris-warmup-0.pdf")))

(slide
 (apply vc-append 10
        (for/list ([s slowdowns])
          (apply
           hc-append 10
           (for/list ([f s])
             (page->pict
              (string-append "../../papers/pycket-papers/oopsla-2017/figs/" f)
             #:scale .5))))))


(pslide
 #:go (coord .5 .0 'ct)
 (page->pict #:scale 1.8 "us.pdf")
 #:next
 #:go (coord .5 .5 'cc)
 (shadow-frame
  (vl-append
   (item "Hidden Classes for Chaperones")
   (item "Benchmark impacts for all optimizations")
   (item "Loop finding & wrappers"))))

#;
(play-n #:layout 'center
        #:skip-first? #t
        #:steps (cons 1 30)
        (λ (start westley)
          (ppict-do full-page
                    #:go (coord .5 .5 'cc)
                    (cellophane (scale (bitmap "westley.jpg") .8) (* .8 westley))
                    #:go (coord .5 .3 'cc)
                    (colorize (t/kau "Sound Gradual Typing" size1)
                              (if (= westley 0) "black" "white"))
                    (blank 35)
                    (colorize (t/kau "Mostly Dead is Slightly Alive" size1)
                              (if (= westley 0) "black" "white"))
                    #:go (coord .5 .7 'cc)
                    (colorize (t/inc "github.com/pycket" size2)
                              (if (= westley 0) "black" "white")))))
)
;(slide (langs-pict #t))

(define (titlet s)
  (t/quat s size2))

(tr-slow)

(slide #:title "Pycket Status"
       (t "Adapting to changes in Racket")
       (t "Adding features to become realistic")
       (blank 100)
       (t "Led by Caner Derici"))

(pic "maxresdefault.jpg" .8)
