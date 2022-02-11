#lang slideshow
(require slideshow/step slideshow/code slideshow/face 
         unstable/gui/ppict unstable/gui/pslide "title.rkt"
         (only-in slideshow/slide title-size)
          "config.ss"
         (except-in "beamer.ss" title title-slide) "lib.ss" racket/gui  "thanks.ss" 
         "tslide.ss" "contracts.rkt"
         "ts-intro.rkt" "stages.rkt" 
         "helper.rkt"
         racket/runtime-path (except-in mzlib/etc identity) unstable/gui/slideshow)

(require "overview.rkt" (prefix-in p: racket-poppler))
(require "tree-sum.rkt" "../../papers/control-proxy/paper/pictures.rkt")
(require "../../papers/practical-fcc/picts.rkt")


(define (page->pict pth #:rotate [r 0] #:scale [factor 1] [page 0])
  (bitmap (scale (rotate (p:page->pict (p:pdf-page (p:open-pdf pth) page)) r) factor)))


(define (scale-h p)
  (scale p (/ (+ margin margin client-h) (pict-height p))))

(define (scale-w p)
  (scale p (/ (+ margin margin client-w) (pict-width p))))

(define (pic fname [r 1])
  (pslide #:go (coord .5 .5 'cc) (scale-w (bitmap fname))))

(define (run-intro)
  (title-slide)
  
  (thanks)
  
  (quotes-slide)
  
  (go) ;; overview
  )
  
(define (run-js)

  (pic "JS.png")
  
  (pic "ecma262.png")
  
  
  (pslide #:go (coord .5 .5 'cc)
        #:alt ((scale-h (bitmap "02_module_scope_02.png"))
               #:go (coord 0 1 'lb)
               (t/cant "Graphics by Lin Clark" (- size3 10)))
        #:alt ((scale-h (bitmap "02_module_scope_03.png"))
               #:go (coord 0 1 'lb)
               (t/cant "Graphics by Lin Clark" (- size3 10)))
        (scale-h (bitmap "02_module_scope_04.png"))

        #:go (coord 0 1 'lb)
        (t/cant "Graphics by Lin Clark" (- size3 10)))

 (pic "typescript.png"))

(define (run-tr)
(pslide
 #:go (coord .5 .5 'cc) (cellophane (scale-h (bitmap "blocks.jpg")) .3)
 #:go (coord .5 .5 'cc) (t/kau "Typed Racket" (+ 16 size1)))


(ts-intro)

(tr-slide ""
          #:go (coord .5 .1 'cc)
          (t/quat "Design Goals" (- size2 0))
          
 ;; FIXME: use `cascade` here, but needs lt instead of cc in bounding box
 #:go (cascade 'auto 'auto 0 0) ;(coord 0.05 0.3 'lc)
 (shadow-frame #:shadow-descent 10 (t "Typed sister language to Racket"))
; #:go (coord 0.15 0.5 'lc)
 (shadow-frame #:shadow-descent 10 (t "Easy porting of existing programs and idioms"))
; #:go (coord 0.25 0.7 'lc)
 (shadow-frame #:shadow-descent 10 (t "Sound interoperability with untyped code"))
; #:go (coord 0.35 0.9 'lc)
 (shadow-frame #:shadow-descent 10 (t "Built entirely as a library -- no changes to Racket"))
 )

(define center (coord .5 .5 'cc))
(pslide #:go (coord .5 .5 'cc) (cellophane (scale-w (bitmap "three-ing.jpg")) .3)
        #:go (coord .5 .5 'cc)
        (t/kau "Three Key Ingredients" (+ 16 size1) ))



(tr-slide "Occurrence Typing"
          #:go center
          (hc-append (t/cant "Using " size2) (t/cant "⊢" (+ 20 size1)) (t/cant " to reason about" size2) (scale (bitmap "cfg.png") .6)))
          
(sum1) (sum2)

(refinements)

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
       (inherit insert last-position get-text erase)
       (define/public (new-prompt) ...)
       (define/public (output s) ...)
       (define/public (reset) ...)))))





(tr-slide "Types for Idioms"
          #:go (coord .5 .4 'cc)
          #:alt ((parameterize ([current-font-size 25])
                  (smod #:name "racket-esq"
                        (ghost (esq-text))
                        (ghost (mixin-ty))
                        (mixin-impl))))
          (parameterize ([current-font-size 25])
            (tmod #:name "racket-esq"
                  (esq-text)
                  (mixin-ty)
                  (mixin-impl))))

(tr-slide "Types for Idioms"
          #:go (coord .5 .1 'cc)
          (t/quat "Typed Clojure" (- size2 0))
          #:go (coord .5 .5 'cc)
          (code
           (ann pname [(U File String) -> (U nil String)])
           (code:comment "multimethod dispatching on class of argument")
           (defmulti pname class)
           (code:comment "String case")
           (defmethod pname String [s] (pname (new File s)))
           (code:comment "File case")
           (defmethod pname File [f] (.getName f)) 
           (pname "STAINS/JELLY") (code:comment "=> \"JELLY\" :- (U nil Str)")))


(tr-slide "Gradual Typing"
          #:go (coord .5 .1 'cc)
          (t/quat "Dynamically enforcing types" (- size2 0))
          #:go center
          (vc-append (t/cant "Type" (- size2 4))
                     (arrow 30 (- (/ pi 2)))
                     (t/cant "Contract" (- size2 4))
                     (arrow 30 (- (/ pi 2)))
                     (t/cant "Runtime wrapper" (- size2 4)))
          )

(server-typed #:arg (code 100 6 (λ (n) 'hi))
              #:title "Gradual Typing"
              #:msg (shadow-frame
                     (inset 
                            (vl-append (t/inc "Expected real? but got 'hi" size2 #:color "red")
                                       (t/inc "  blaming: compute" size2 #:color "red")
                                       (t/inc "  contract from: grad" size2 #:color "red"))
                            20)))


(tr-slide "Gradual Typing"
          #:go (coord .5 .1 'cc)
          (t/quat "Complex features, complex boundaries" (- size2 0))
          #:go (coord .5 .5 'cc)
          #:alt ((scale frame-pict-3 4))
          (scale (bitmap "class-contract.png") 0.8)
          )

(define tc-mod
  (code (define (typecheck form)
          (syntax-parse form
            [v:identifier 
             ...]
            [(λ args body)
             ...]
            [(define v body)
             ...]
            #,(blank 30)
            #,(t "... other syntactic forms ...")))))

(define tr-mod 
  (vl-append
   (red-block 
    (t "Module Semantics")
    (pict-if #t (code (define-syntax module-begin ...)) (t "Standard Functions")))
   (blank 20)
   (block (pict-if #t (t "Core Syntax") (code (define-syntax module-begin ...)))
          (code (define-syntax λ ...)))
   (blank 20)
   (block (pict-if #t (t "Standard Functions") (code (define-syntax module-begin ...)))
          (code (define + ...)))))

(define bigm (code (#,(red-code module-begin)
                    #,(grad-def)
                    ||
                    grad-call)))


(tr-slide "Extensible Language"
          #:go (coord .5 .5 'cc)
          (cc-superimpose (ghost 
                           (scale ((current-code-tt) "#lang typed/racket") 3))
                          (scale ((current-code-tt) "#lang racket") 3)))
(tr-slide "Extensible Language"
          #:go (coord .5 .5 'cc)
          (scale ((current-code-tt) "#lang typed/racket") 3))

(tr-slide "Extensible Language"
          #:go (coord .5 .1 'cc)
          (t/quat "Implementing a language" (- size2 0))
          #:go (coord .5 .5 'cc)
          (smod #:name "typed/racket" #:lang (code racket) #:sizeof (cc-superimpose (inset bigm 0 ) tc-mod)
                tr-mod))

(tr-slide "Extensible Language"
          #:go (coord .5 .1 'cc)
          (t/quat "Implementing a language" (- size2 0))
          #:go (coord .5 .5 'cc)
          
          (smod #:name "typed/racket" #:lang (code racket) #:sizeof (cc-superimpose (inset bigm 0 0) tc-mod)
                (code
                 (define-syntax module-begin
                   (syntax-parser
                     [(_ forms ...)
                      #,(red-code (for ([form #'(forms ...)])
                                    (typecheck form)))
                      ||
                      #'(forms ...)])))))

(tr-slide "Extensible Language"
          #:go (coord .5 .1 'cc)
          (t/quat "The Typechecker" (- size2 0))
          #:go (coord .5 .5 'cc)
       (smod #:name "typechecker" #:lang (code racket) #:sizeof (cc-superimpose (inset bigm 0 0) tc-mod)
             tc-mod))

#;(tr-slide "Extensible Language"
          #:go (coord .5 .5 'cc)
          (t/cant "Languages as libraries")))

(define (run-jit)


(pslide
 #:go (coord .5 .5 'cc) (cellophane (scale-w (bitmap "CHEETAH.jpg")) .3)
 #:go (coord .5 .5 'cc) (t/kau "JIT Compilers" (+ 16 size1)))

(define-syntax-rule (jit-slide s more ...)
  (bottom-bar-slide "JIT Compilers"
                    #:go (coord 1 1 'rb)
                    (if (string? s) (t/cant s size2 #:color "white") (colorize s "white"))
                    more ...))


(server-typed #:arg (code 100 6 (λ (x) (- (* 4 x x) (* 9 x))))
              #:title "The Problem"
              #:msg (shadow-frame
                     (inset 
                      (vl-append (t/cant "1.5x slower than the untyped code!" size2))
                      20)))

(define (quoted author ts)
  (vr-append (apply vl-append (for/list ([t ts])
                                (t/cant t size2)))
             (blank 30)
             (t/kau (string-append "" author) size2)))


(jit-slide "The Problem"
           #:go (coord .5 .5 'cc)
           (quoted "Vincent St. Amour, Dec. 2015"
               '("Interfacing Typed Racket and Racket code"
                 "may involve a lot of dynamic checks, which"
                 "can have significant overhead, and cause"
                 "that kind of stuttering.")))

(jit-slide "The Problem"
           #:go (coord .5 .5 'cc)

           (quoted "Neil Toronto, May 2015"
               '("It's very slow."
                 ""
                 "It looks like it has to do with a dc<%>"
                 "instance crossing from untyped to"
                 "typed code.")))


(jit-slide "The Problem"
                      #:go (coord .5 .5 'cc)

                      (quoted "John Clements, January 2016"
               '("... the resulting dynamic checks"
                 "will probably cause them to be"
                 "unacceptably slow.")))


(jit-slide "The Problem"
 #:go (coord .5 .0 'ct)
 (page->pict "popl16-tfgnvf.pdf" #:scale 1.8)
 #:go (coord .5 .5 'cc)
 #:next
 #:alt
 [(shadow-frame
   (vl-append
    (t/cant "The problem is that, according to our measurements,"
            size3)
    (t/cant "the cost of enforcing soundness is overwhelming."
            size3)))]
 #:do [(define synth-bmp (scale (bitmap "synth.png") .8))
       (define inset-synth (scale (inset (scale (bitmap "synth.png") .8)
                                         -600 0 0 0) 1.3))]
 #:alt
 [(shadow-frame (cc-superimpose (inset synth-bmp 0 0 90 0) (ghost inset-synth)))]
 
 #:alt
 [(shadow-frame
   (cc-superimpose ;(cellophane inset-synth .3)
                   (scale (inset/clip (scale (bitmap "synth.png") .8)
                                      -600 0 0 0) 1.3)
                   (ghost synth-bmp)))]
 #:alt
 [(shadow-frame
   (cc-superimpose
    (inset (apply vl-append
                  (for/list ([t '("We find that Typed Racket’s cost of"
                                  "soundness is not tolerable. If applying"
                                  "our method to other gradual type system"
                                  "implementations yields similar results,"
                                  "then sound gradual typing is dead.")])
                    (t/cant t size3)))
           75 30)))]

 )

(pslide #:go (coord .5 .5 'cc) (scale-h (bitmap "aggregate.png")))

(jit-slide "Synth, again"
            #:go (coord .5 0 'ct)
            (page->pict #:scale 1.4 "slowdown-synth-warmup-0.pdf"))

(jit-slide  "Key Idea"
             #:go (coord .5 .5 'cc)

             (item (t/cant "Tracing JIT Compilation" size2)))
;(code-colorize-enabled #t)

(define chaperoned
  (code
   (define f* (checked-fn #,(red-code (λ (x) (+ x 1)))
                          Integer Integer))
   (define (f x*)
     (cast ((checked-fn-op f)
            (cast x* (checked-fn-domain f)))
           (checked-fn-range f)))))


(jit-slide ""
           #:go (coord .5 .5 'cc)
           20
 (code (define f (cast #,(red-code (λ (x) (+ x 1)))
                       (-> Integer Integer))))
 #:next
 (arrow 30 (- (/ pi 2)))
 (ct-superimpose
  (ghost chaperoned)
  (code (define (f x*)
          (cast (#,(red-code (λ (x) (+ x 1))) (cast x* Integer))
                Integer)))))

(jit-slide ""
 #:go (coord .5 .5 'cc) 20
 (code (define f (cast #,(red-code (λ (x) (+ x 1)))
                       (-> Integer Integer))))
 (arrow 30 (- (/ pi 2)))

 chaperoned)



(jit-slide (code (f x))
 #:go (coord .5 .5 'cc)
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

(jit-slide (code (f x))
 #:go (coord .5 .5 'cc)
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

(jit-slide (code (f x))
 #:go (coord .5 .5 'cc)
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

(jit-slide "Results"
 #:go (coord .5 0 'ct)
 (apply vc-append 10
        (for/list ([s slowdowns])
          (apply
           hc-append 10
           (for/list ([f s])
             (bitmap
              (pict->bitmap
               (scale
                (page->pict
                 (string-append "../../papers/pycket-papers/oopsla-2017/figs/" f))
                .47))))))))

(pslide #:go (coord .5 .5 'cc) (scale-h (bitmap "miraclemax.jpg"))
        #:go (coord .5 .5 'cc)
        (t/kau #:color "white" "Only mostly dead!" (+ 10 size1)))


)
(define (run-finish)


  (bottom-bar-slide "In the future ..."
                    #:go (cascade 'auto 'auto)
                    (shadow-frame (t/cant "Using JIT compilation & DSLs for machine learning" size2))
                    (shadow-frame (t/cant "Giving users the powers of tracing in the language" size2))
                    (shadow-frame (t/cant "Automatically inferring type annotations from tests" size2))
                    (shadow-frame (t/cant "Integrating verification and gradual typechecking" size2)))

  (bottom-bar-slide "In the future ..."
                    #:go (coord 1 1 'rb)
                    (t/cant "DSLs & JITs" size2 #:color "white")
                    #:go (coord .5 .1 'ct)
                    #:alt ((scale (bitmap "radar.png") .8))
                    #:go (coord .5 .1 'ct)
                    (hc-append 30 
                               (page->pict "GmmGibbs50-10000.pdf" #:scale 1.9)
                               (page->pict "GmmGibbs25-5000.pdf" #:scale 1.9))
                    )




  (pslide
   #:go (coord .5 .5 'cc)
   (cellophane (scale-w (bitmap "canopy.jpg")) .3)
   #:go (coord .5 .5 'cc)
   (t/kau "Thank You!" (+ 10 size1)))
  )

;(run-intro)
;(run-js)
;(run-tr)
;(run-jit)
(run-finish)

;(tslide "Building Languages")
