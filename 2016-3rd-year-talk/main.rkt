#lang slideshow

(require slideshow/step slideshow/code slideshow/face 
         unstable/gui/ppict unstable/gui/pslide
         (only-in slideshow/slide title-size)
          "config.ss"
         (except-in "beamer.ss" title) "lib.ss" racket/gui  "thanks.ss" 
         "tslide.ss" lang-slide/hudak-quote "contracts.rkt"
         "ts-intro.rkt" "stages.rkt"
         "helper.rkt"
         racket/runtime-path (except-in mzlib/etc identity) unstable/gui/slideshow)

(define (pic fname [r 1])
  (pslide #:go (coord .5 .5 'cc)
        (scale (bitmap fname) r)))

(do-start? #f)
(require ppict-slide-grid)
;(set-grid-base-pict!)

(define IU (freeze (scale (bitmap "IU.jpg") .25)))

(define back (cellophane IU .2))

(define (title n)
  (ppict-do
   ((pslide-base-pict))
   #:go (coord .5 .5 'cc) 
   (cellophane back n)
   #:go (coord 0.05 .85 'lc)
   (t/cant "Sam Tobin-Hochstadt" size2)
   #:go (coord 0.05 .95 'lc)
   (t/quat "Indiana University" size2)
   #:go (coord 0.01 .15 'lc)
   (t/cant "Software Contracts:" (+ 10 size1))
   #:go (coord 0.4 .43 'lc)
   (t/quat "Semantics" size1)
   ;#:go (coord 0.95 .30 'rc)
   (t/quat "  Verification" size1)
   ;#:go (coord 0.91 .30 'rc)
   (t/quat "    Optimization" size1)
   #:go (coord .5 .5 'cc) 
   (cellophane IU (- 1 n))))

(play-n title #:steps 150)

#;(define (pic fname [r 1])
  (pslide #:go (coord .5 .5 'cc)
        (scale (bitmap fname) r)))

(code-colorize-enabled #t)

(tslide* "Not in this talk")

(pslide #:go (coord .1 .5 'lc)
        (t/cant "Gradual Types" size1)
        (blank 30)
        (t/cant "Metaprogramming" size1)
        (blank 30)
        (t/cant "Refinement Types" size1)
        (blank 30)
        (t/cant "Concurrent Systems" size1)
        (blank 30)
        (t/cant "Performance Tools" size1))

(tslide* "Contracts")


(pslide #:go (coord .4 .5 'cc)
        (scale (bitmap "parnas.png") 1))

(pslide #:go (coord .5 .5 'cc)
        (scale (bitmap "eiffel_tower.jpg") .3)
        ;#:next
        #:go (coord 3/4 3/5 'cc)
        (shadow-frame (t/cant "Eiffel" 50)
                      #:shadow-descent 5))

(pslide #:go (coord .5 .5 'cc)
        (scale (bitmap "eiffel_dbc.png") 1.1)
        ;#:next
        #:go (coord 3/4 3/5 'cc)
        (shadow-frame (t/cant "Eiffel" 50)
                      #:shadow-descent 5))

(pslide/title "Now available in"
              #:go (coord .1 .3 'lt)
       (item "Python")
       (item "JavaScript")
       (item "Ruby")
       (item "C#")
       (item "Java")
       (item "..."))


(require "icfp-2014-contracts-talk/intro.rkt"
         (except-in "icfp-2014-contracts-talk/util.rkt" subtitle)
         slideshow/play)
(define (robby-intro)
  (define first-sequence (make-pip-sequence 0 0 #f #f))
  (play-n (wrap/first-argument-always-1 first-sequence))
  
  (define pip5050 (make-pip-sequence 50 50 #t #f))
  (slide (pip5050 1 1 0 0 0 0 0 0))
  (play-n
   (λ (a b c)
     (pip5050 1 1 1 1 1 a b c)))
  
  (bad-call)
  
  (define pip-false (make-pip-sequence #f #f 'error #f))
  (slide (pip-false 1 1 1 1 1 0 0 0))
  
  (define pip-false/contract (make-pip-sequence #f #f 'error 'type #:red-contract? #t))
  (play-n (λ (n1 n2 n3) (pip-false/contract n1 n2 n3 0 0 0 0 0)))
  (violation-with-type-based-contract))

(when #t
  (robby-intro))
  
;(time-vs-contracts)

(tslide* "Semantics")

(pic "contract.jpg" .5)

(pslide
 #:go (coord .5 .5 'cc)
 (cellophane (scale (bitmap "scale.jpg") .5) .3)
 #:go (coord .5 .5 'cc)
        (t/br "I agree to give you bricks weighing" size1)
        (t/br "  less than 500 lbs every week" size1)
        (t/br "on Wednesday." size1)
        #:next
        #:go (coord .5 .8 'cc)
        (colorize (t/cant "What if the scale breaks?" size1) "red")
        #:next
        #:go (coord .5 .5 'cc)
        (rotate (shadow-frame (t/cant "Contracts can go wrong" 50)
                              #:shadow-descent 5) (/ pi 6)))



(pslide
 #:go (coord .5 .5 'cc)
 (cellophane (scale (bitmap "scale.jpg") .5) .3)
 #:go (coord .5 .5 'cc)
        (t/br "I agree to give you bricks weighing" size1)
        (t/br "  less than 500 lbs every week" size1)
        (t/br "on Wednesday." size1)
        #:next
        (t/br "  And the scale to weight them with." size1)
        #:next
        #:go (coord .5 .8 'cc)
        (colorize (t/cant "Now what if the scale breaks?" size1) "red")
        )

(define con2 (code (provide
                    [construction
                     (->d [b bricks?]
                          [scale (-> <1000/c weight)]
                          #:pre (< (scale b) 500)
                          any)])))
(define con1 (code (provide
                    [construction
                     (->d [b bricks?]
                          [scale (-> any/c weight)]
                          #:pre (< (scale b) 500)
                          any)])))

(pslide/title "Dependent contracts"
              #:go (coord .01 .3 'lt)
       (lt-superimpose con1 (ghost con2))
       (blank 40)
       (code (construction heavy-bricks scale)))

(pslide/title "Dependent contracts"
              #:go (coord .01 .3 'lt)
              con2
              (blank 40)
              #:next
              (code (construction heavy-bricks fragile-scale))
              #:next
              #:go (coord .5 .5 'cc)
              (rotate (shadow-frame (t/cant "Who should be blamed?" 50)
                                    #:shadow-descent 5) (/ pi 6)))


(pslide/title "Solution: Complete Blame"
              #:go (coord .1 .3 'lt)
       (item "Says who should be blamed in tricky cases")
       (item "Ensures that contracts are complete")
       (blank 40)
       (item "Helps with gradual typing!"))


(tslide* "Semantics II")

(pslide/title "Many approaches to contracts"
              #:go (coord .1 .3 'lt)
       (item "Check everything now")
       (item "Check everything now, in parallel")
       (item "Check when needed")
       (item "..."))

(pslide #:go (coord .5 .5 'cc)
        (slide->pict (most-recent-slide))
        #:go (coord .5 .5 'cc)
        (rotate (shadow-frame (t/cant "Insight: communicating processes" 50)
                              #:shadow-descent 5) (/ pi 6)))

(pic "eager.png" .6)
(pic "async.png" .6)
(pic "semi.png" .8)

(require pdf-read "verif.rkt")

(tslide* "Verification")

(pslide #:go (coord .5 .5 'cc)
        (page->pict (pdf-page "vanhorn-icfp2014.pdf" 3))
        #:next
        #:go (coord .5 .5 'cc)
        (rotate (shadow-frame (t/cant "Check at compile time!" 50)
                              #:shadow-descent 5) (/ pi 6)))

(pslide #:go (coord .5 .5 'cc)
        (scale (bitmap "drr-error.png") .7)
        #:next
        #:go (coord .5 .5 'cc)
        (rotate (shadow-frame (t/cant "Check at compile time!" 50)
                              #:shadow-descent 5) (/ pi 6)))


(pslide/title "A simple language")
(go 17 23 #:title "A simple language")
(go 42 54 #:title "With abstract values")
;(go 59 62)

(go 64 71 #:title "The higher-order case")

(go 100 100 #f)



(tslide* "Optimization")


(pslide #:go (coord .5 .5 'cc)
        (page->pict (pdf-page "vanhorn-icfp2014.pdf" 3))
        #:next
        #:go (coord .5 .5 'cc)
        (rotate (shadow-frame (t/cant "Check at runtime, faster!" 50)
                              #:shadow-descent 5) (/ pi 6)))

(pslide/staged/title
 (zero one three)
 "A simpler program"
 #:go (coord .5 .4 'cc)
 (pict-case stage-name #:combine cc-superimpose
            [(zero one)(scale (code (define (dot u v)
                                      (for/sum ([x u]
                                                [y v])
                                        (* x y))))
                       1.2)]
            [(two) (scale (code (define (dot u v)
                                  (for/sum ([x (in-vector u)]
                                            [y (in-vector v)])
                                    (fl* x y))))
                          1.2)]
            [(three) (scale (code (define/contract (dot u v)
                                    ((vectorof flonum?)
                                     (vectorof flonum?)
                                     . -> . flonum?)
                                    (for/sum ([x u] [y v])
                                      (* x y))))
                            1.2)]
            [(four) 
             (scale (code (define (dot v1 v2)
                            (define len (flvector-length v1))
                            (unless (= len (flvector-length v2))
                              (error 'fail))
                            (let loop ([n 0] [sum 0.0])
                              (if (unsafe-fx= len n) sum
                                  (loop (unsafe-fx+ n 1)
                                        (unsafe-fl+
                                         sum (unsafe-fl*
                                              (unsafe-flvector-ref v1 n)
                                              (unsafe-flvector-ref v1 n))))))))
                    .8)])
 #:go (coord .5 .8 'cc)
 (pict-case stage-name #:combine rc-superimpose
            [(one) (t/cant "506 ms (size 10000000)" size2)]
            [(two) (t/cant "39 ms (size 10000000)" size2)]
            [(three) (t/cant "1159 ms (size 10000000)" size2)]
            [(four) (t/cant "29 ms (size 10000000)" size2)]))


(pslide/title "Why are contracts hard to optimize?"
              #:go (coord .5 .5)
              (code (contract (-> integer? integer?)
                              (lambda (x) x))))

(pslide/title "Why are contracts hard to optimize?"
              #:go (coord .5 .5)
              (code (chaperone-procedure
                     (lambda (x) x)
                     (lambda (v)
                       (unless (integer? v) (error 'blame))
                       v)
                     (lambda (v)
                       (unless (integer? v) (error 'blame))
                       v))))



(pslide #:go (coord .5 .6 'cc) (scale (bitmap "cake.jpg") 1.7))


(pslide/staged/title
 (three four)
 "With Pycket ..."
 #:go (coord .5 .4 'cc)
 (pict-case stage-name #:combine cc-superimpose
            [(three)(scale (code (define (dot u v)
                                   (for/sum ([x u]
                                             [y v])
                                     (* x y))))
                           1.2)]
            [(two) (scale (code (define (dot u v)
                                (for/sum ([x (in-vector u)]
                                          [y (in-vector v)])
                                  (fl* x y))))
                        1.2)]
            [(four) (scale (code (define/contract (dot u v)
                                   ((vectorof flonum?)
                                    (vectorof flonum?)
                                    . -> . flonum?)
                                   (for/sum ([x u] [y v])
                                     (* x y))))
                        1.2)]
            [(one) (scale (code (define (dot v1 v2)
                                (define len (flvector-length v1))
                                (unless (= len (flvector-length v2))
                                  (error 'fail))
                                (let loop ([n 0] [sum 0.0])
                                  (if (unsafe-fx= len n) sum
                                      (loop (unsafe-fx+ n 1)
                                            (unsafe-fl+
                                             sum (unsafe-fl*
                                                  (unsafe-flvector-ref v1 n)
                                                  (unsafe-flvector-ref v1 n))))))))
                        .8)])
 #:go (coord .5 .8 'cc)
 (pict-case stage-name #:combine rc-superimpose
            [(three) (t/cant "12 ms (size 10000000)" size2)]
            [(two) (t/cant "11 ms (size 10000000)" size2)]
            [(four) (t/cant "17 ms (size 10000000)" size2)]
            [(one) (t/cant "8 ms (size 10000000)" size2)]))

(define (pic2 t b [scl 1])
  (pslide/title t #:go (coord .5 .5 'cc) (scale (bitmap (~a b ".png")) scl)))

;(tslide* "How does it work?")

(pslide/title "Tracing JIT"
              #:go (coord .1 .45 'lc)
              20
              (t/quat "1. Interpret Program" size2)
              (t/quat "2. Find hot loop" size2)
              (t/quat "3. Record operations for one iteration"  size2)
              (t/quat "4. Optimize" size2)
              (t/quat "5. Switch to new code" size2)
)

(pslide/title "Tracing JIT"
              #:go (coord .5 .52 'cc) (bitmap "jit.png")
              #:go (coord .9 .95 'rc) (t/quat "(Diagram from Antonio Cuni)" 16))

(pslide/title "Dot product Inner Loop"
              #:go (coord .5 .5 'cc) (scale (bitmap (~a "inner-loop" ".png")) .9))


(pslide/title "Key Optimizations"
              #:go (coord .1 .45 'lc)
              20
              (t/quat "Inlining (happens for free)" size2)
              (t/quat "Constant propagation" size2)
              (t/quat "Allocation Removal" size2)
)

(define (t* l) (t/quat l size2))

;(tslide* "Meta-tracing: the magic part")

(pslide #:go (coord .5 .5 'cc #:compose vl-append)
        40
        (t/quat "We didn't write a JIT or an optimizer!" size2)
        #:next
        (t/quat "RPython creates a JIT from an interpreter" size2))

(pic2 "CEK Machine" "cek")

(pslide/title "CEK Advantages"
              #:go (coord .1 .45 'lc)
              20
              (t* "Fast continuations")
              (t* "Tail recursion")
              (t* "Arbitrary size stack")
              #:next
              (colorize (t* "Allocation everywhere") "red")
              )

(pslide/title "From CEK to JIT"
              #:go (coord .1 .45 'lc)
              20
              (t* "1. Whole-program type inference")
              (t* "2. Translation to C")
              (t* "3. Adding JIT based on hints"))

(pic2 "Main Interpreter Loop" "main-loop")





(pslide #:go (coord .5 .5 'cc) (scale (bitmap (~a "contract-bench" ".png")) .9))


(require unstable/gui/pict)

(define (wrap-up [thanks? #t])
  (slide  #:layout 'center
          (vc-append 20
           (shadow-frame
            (vl-append
             (t/cant "Contracts: a useful techology" size2)
             (t/cant "and a source of research ideas" size2))
            #:shadow-descent 10)

         (t "")
         (blank 40)
         (t/inc "samth@indiana.edu" size2)
         (t/inc "samth.github.io" size2))))


(parameterize ([current-background-pict (bitmap plt-background-path)])
  (wrap-up #t))
