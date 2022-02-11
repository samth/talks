#lang racket/base
(require slideshow "title.rkt" "tslide.rkt" (submod "../dls06/talk.ss" title)
         slideshow/slides-to-picts "lib.rkt"
         unstable/gui/ppict unstable/gui/pslide
         slideshow/code  unstable/gui/slideshow
         "ts-intro.rkt"
         "config.rkt" "helper.rkt")

(define old-margin margin)

(title-slide)
;; (slide #:title "Why?")

;; (slide
;;  (t "Static Checking of Racket programs")
;;  (t "Address maintinence issues")
;;  (t "Understand interaction of typed and untyped languages"))


;(tslide "10 years ago ...")


(dynamic-require "intro.rkt" #f)


(pslide
 #:go (coord .5 .5 'cc)
 (colorize (filled-rectangle 1200 1000) "black")
 #:go (coord .5 .5 'cc) 
 (scale (bitmap  "A_long_time_ago.jpg") .75))



(parameterize ([current-slide-assembler
                (lambda (t g p)
                  (cc-superimpose
                   (frame (scale p .8))
                   full-page))])
  (dls-title))

(define q (bitmap "q.png"))
(define q* (inset/clip  (scale q (/ (pict-height (t "?")) (pict-height q))) 0 -7 0 -8))
(define q** (t/kau "? " size2))

(ts-intro )


(slide #:layout 'top
       (blank 20)
       (t/kau "4 ideas" size1)
       (blank 5)
       (item #:bullet q** "Gradual Typing")
       (item #:bullet q** "Soundness")
       (item #:bullet q** "Migrating untyped to typed")
       (item #:bullet q** "Custom type systems")
       #;
       (item #:bullet q** "Languages as libraries"))

(tslide "Gradual typing")
;(set-margin! old-margin)
(define-values (ack-define ack-cond)
  (values (code define) (code cond)))

(define (ack-def #:typed [typed #t])
  (code
     #,(pict-if
        typed 
        (code (: ack : Integer Integer -> Integer))
        (code (code:comment " ack : Integer Integer -> Integer")))
     (#,ack-define (ack m n)
       (#,ack-cond [(<= m 0) (+ n 1)]
             [(<= n 0) (ack (- m 1) 1)]
             [else (ack (- m 1) (ack m (- n 1)))]))))

(define-syntax-rule (pslide/title e . rest)
  (pslide #:go (coord 0.05 0.05 'lc)
        (colorize (t/cant e size2) "blue")
        . rest))
(define-syntax-rule (pslide/staged/title [name ...] t arg ...)
  (staged [name ...] (pslide/title t arg ...)))

(ts-intro #:skip '(1 2 3) #:title? #f)

#;(staged 
 (untyped typed)
 (pslide/title "Functions"
               #:go (coord .5 .5 'cc)
        (pict-case 
         stage-name #:combine ltl-superimpose
         [(untyped) (smod #:name "ack"
                          (code
                             #,(ack-def #:typed #f)
                             ||
                             (ack 2 3)))]
         [(typed) (tmod #:name "ack"
                        (code
                         #,(ack-def)              
                         ||
                         (ack 2 3)))]
         #;
         [else (mini-slide (smod #:name "ack"
                                 (ack-def #:typed #f))
                           #;(blank 10)
                           (smod #:name "compute" #:sizeof (ack-def)
                                 (code (require ack)
                                       ||                                        
                                       (ack 2 3))))])))



(define (pic f [s 1])
  (pslide #:go (coord .5 .5 'cc) (scale (bitmap f) s)))

(pic "typescript.png" .9)

(pic "hack.png" .9)

(pic "icfp17.png" .9)

(pslide/title "Why did gradual typing succeed?"

              #:go (coord .15 .3 'lt) (t "Dynamic languages")
              #:go (coord .8 .3 'rt) (t "[Ousterhout, ...]")
              #:go (coord .15 .5 'lt) (t "Contracts")
              #:go (coord .8 .5 'rt) (t "[Findler & Felleisen, ...]")
              #:next
              #:go (coord .15 .7 'lt) (t "Solve the easy problems!"))

(slide #:layout 'center
       (scale (titlet "Gradual Typing: A+") 2))

(tslide "Soundness")

(pslide/title "What is soundness?"
              #:go (coord .5 .5 'cc)
              (hbl-append (t "If ")
                          (t/quat " ⊢ e : Boolean" size3)
                          (t " and ")
                          (t/quat "e ⇓ v" size3)
                          (t " then ")
                          (t/quat "v = " size3)
                          (code #t)
                          (t " or ")
                          (t/quat "v = " size3)
                          (code #f)
                          (t ".")))

(pslide/title "What is soundness for gradual typing?"
              #:go (coord .1 .4 'lt)
              (vl-append
               (t "Typed modules can’t go wrong, and")
               (t "all run-time errors originate in untyped modules."))
              #:go (coord .8 .62 'rt)
              (t "[DLS 2006]"))

(require pdf-read)

(pslide/title "Why care?"
              #:go (coord .1 .5 'lc)
              
              (t "Optimization")
              (t "Debugging")
              (t "Refactoring")
              (t "")
              (t "Obviously the right thing")
              #:next
              #:go (coord .5 .5 'cc)
              (cellophane (filled-rectangle #:draw-border? #f
                                            #:color "white"
                                            900 500) .7)
              #:go (coord .5 .5 'cc)
              (rotate (t/kau "Performance!" (+ 58 size1)) (/ pi 7)))

(pslide #:go (coord .5 .5 'cc)
        (scale (bitmap (page->bitmap (pdf-page "aggregate-cdf.pdf" 0))) 1.5))

(define mark (filled-rectangle 12 100))

(pslide/title "The Soundness Spectrum"
              #:go (coord .5 .5 'cc)
        (cc-superimpose
         (hc-append 0
                    (blank 25)
                    (filled-rectangle 12 100)
                    (filled-rectangle 800 2)
                    (filled-rectangle 12 100))
         mark)
        #:next
        (inset
         (ht-append
          20
          (rotate (t "Typed Racket") (/ pi 2.5))
          (blank 30)
          (rotate (t "Reticulated Python") (/ pi 2.5))
          (blank 50)
          (rotate (t "Most research") (/ pi 2.5))
          (blank 90)
          (rotate (t "Flow") (/ pi 2.5))
          
          (rotate (t "TypeScript") (/ pi 2.5)))
         -30 0 0 0))


(slide #:layout 'center
       (scale (titlet "Soundness: B") 2))



(tslide "Migration")

(ts-intro #:skip '(1 4) #:title? #f)

(pic "stats.png" .9)

(pslide/title "Results"
              #:go (coord .05 .3 'lt)
              (t/cant "Files using of Typed Racket in Racket:" size2)
              #:go (coord .85 .37 'rt)
              (t/cant "266" size2)
              #:next
              #:go (coord .05 .55 'lt)
              (t/cant "Ported Files:" size2)
              #:go (coord .85 .62 'rt)
              (t/cant "2" size2))

(pslide/title "Why not?"
              #:go (coord .05 .3 'lt)
              (t/cant "If it ain't broke, don't fix it" size2)
              (blank 25)
              (t/cant "Racket programmers like Racket" size2)
              (blank 25)
              (t/cant "Pent-up demand for Racket with types" size2)
              (blank 25)
              (t/cant "Ports become rewrites" size2))

(slide #:layout 'center
       (scale (titlet "Migration: D") 2))

(tslide "Custom types")

(define narrow1 (t/cant "Racket"))
(define wide1 (t/cant "JavaScript"))
(define narrow2 (t/cant "Haskell"))
(define wide2 (t/cant "Haskell"))
(define x-diff1 (- (pict-width wide1) (pict-width narrow1)))
(define x-diff2 (- (pict-width wide2) (pict-width narrow2)))

(slide/staged [one many after] ;#:title (titlet "How do Racket programmers think?")
       (hc-append
        (pict-case 
         stage-name #:combine cc-superimpose
         [(one) (t/cant "Racket")]
         [(many after) 
          (inset 
           (vc-append 
            (t/cant "Ruby")
            (t/cant "Python")
            (t/cant "Racket")
            (t/cant "JavaScript")
            (t/cant "Lua"))
           (/ x-diff1 -2) 0)])
        (t/cant " programs are not secretly ")
        (pict-case
         stage-name #:combine cc-superimpose
         [(one) (t/cant "Haskell")]
         [(many after) 
          (inset 
           (vc-append (t/cant "Java")
                      (t/cant "ML")
                      (t/cant "Haskell")
                      (t/cant "Scala")
                      (t/cant "C++"))
           (/ x-diff2 -2))])
         (t/cant " programs")
        )
       (blank 30)
       (pict-case
        stage-name
        [(after)
         (t "Consider the native idioms of a language")]
        [else (blank)]))

(start)

(define forall (lift-above-baseline (code ∀) -3))

(slide/staged 
 [occur union varar local]
 ;#:title (titlet "Types for Racket Idioms")
 (tmod #:name (symbol->string stage-name)
  (pict-case stage-name
   [(occur)
    (code (: f (Any -> Number))
          (define (f x)
            (if (number? x)
                (add1 x)
                0)))]
   [(varar) 
    (code
     (: wrap (#,forall (B A ...)
               ((A ... -> B) -> (A ... -> B))))
     (define (wrap f)
       (lambda args
         (printf "args are: ~a\n" args)
         (apply f args))))]
   [(refine)
    (code
     (: check (String -> (Refinement sql-safe?)))
     (define (check s)
       (if (sql-safe? s)
           s
           (error "unsafe string!"))))]
   [(local) 
    (code
     (define x 1)
     (define y 2)
     (map - (list x y)))]
   [(union)
    (code
     (define-type Peano (U 'Zero (List 'S Peano)))
     (: convert : Peano -> Number)
     (define (convert n)
       (cond [(symbol? n) 0]
             [else (add1 (convert (rest n)))])))])))

(define (esq-text)              
  (code
   (define-type Esq-Text%
     (Class #:implements Text%
            [new-prompt (-> Void)]
            [output (String -> Void)]
            [reset (-> Void)]))))
(define (mixin-ty)
  (code
   (: esq-mixin
      (All (r #:row)
           (-> (Class #:row-var r #:implements Text%)
               (Class #:row-var r #:implements Esq-Text%))))))

(define (mixin-impl)
  (code
   (code:comment "add REPL functions to `base-class`")
   (define (esq-mixin base-class)
     (class base-class
       (super-new)
       (inherit insert last-position get-text erase)
       ||
       (define/public (new-prompt) ...)
       (define/public (output s) ...)
       (define/public (reset) ...)))))

(slide ;#:title (titlet "Mixins in Racket")
       #:layout 'center
       (parameterize ([current-font-size 35])
         (smod #:name "racket-esq"
               (ghost (esq-text))
               (ghost (mixin-ty))
               (mixin-impl))))

(slide ;#:title (titlet "Mixins in Typed Racket")
       #:layout 'center
       (parameterize ([current-font-size 35])
         (tmod #:name "racket-esq"
               (esq-text)
               (mixin-ty)
               (mixin-impl))))


(require "peano.rkt" "combine.rkt")

(peano1) (combine1)
(peano2) (combine2)

(define t/dosis t/cant)


(pslide/title
 "Mixins in the DrRacket IDE"
 #:go (coord 0.0 0.55 'lc)
 (parameterize ([current-font-size 25])
   (code
    (define drracket-frame%
      (online-expand-frame-mixin
       (frame-mixin
        (drracket:frame:mixin
         (drracket:frame:basics-mixin 
          (frame:size-pref-mixin
           (frame:searchable-text-mixin 
            (frame:searchable-mixin
             (frame:text-info-mixin 
              (frame:delegate-mixin
               (frame:status-line-mixin
                (frame:info-mixin
                 (frame:text-mixin
                  (frame:editor-mixin
                   (frame:standard-menus-mixin
                    (frame:register-group-mixin
                     (frame:focus-table-mixin
                      (frame:basic-mixin
                       frame%))))))))))))))))))))
 #:go (coord 0.5 0.45 'lc)
 (shadow-frame
  (vl-append
   (t/dosis "Layered development" 35)
   (t/dosis "DrRacket frame: 17 mixins" 35)
   (t/dosis "≥ 49 mixins in codebase" 35))))


;(start)
;; Occurrence Typing + Classes

(slide ;#:title (titlet "Lessons")
       (titlet "Existing idioms are a source of type system ideas")
       (blank 50)
       (para "Repeated in TypeScript, Typed Clojure, Hack, ..."))


(pic "stairs.jpg")

(slide #:layout 'center
       (scale (titlet "Custom Types: A-") 2))


(slide #:layout 'top
       (blank 20)
       (t/kau "4 ideas" size1)
       (blank 5)
       (item #:bullet q** "Gradual Typing: A+")
       (item #:bullet q** "Soundness: B")
       (item #:bullet q** "Migrating untyped to typed: D")
       (item #:bullet q** "Custom type systems: A-")
       #;
       (item #:bullet q** "Languages as libraries"))

(slide #:layout 'top
       (blank 20)
       (t/kau "4 more ideas!" size1)
       (blank 5)
       (item #:bullet q** "Language as Libraries")
       (item #:bullet q** "Casts via Contracts")
       (item #:bullet q** "Types and Language Levels")
       (item #:bullet q** "How precise are types?")
       'next
       (blank 20)
       (t/cant "Thank You!" size1))
