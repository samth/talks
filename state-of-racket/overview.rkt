#lang slideshow

(require  fancy-app "tslide.rkt" ppict/slideshow2 ppict/2 "helper.rkt" "config.rkt"
          lang-slide (prefix-in p: racket-poppler))

(require pict/code)
;(tslide "Overview")

(define (page->pict pth #:rotate [r 0] #:scale [factor 1] [page 0])
  (bitmap (scale (rotate (p:page->pict (p:pdf-page (p:open-pdf pth) page)) r) factor)))

(define (modules c1 c2 c3)
  (let ([N 80])
    (define (r c)
      (filled-rounded-rectangle #:color c (* 1.5 N) N))
    (define one (r c1))
    (define two (r c2))
    (define three (r c3))
    (define p
      (vc-append
      (ht-append one (blank N) two)
      (blank N)
      three))
    ((compose (pin-arrow-line 15 _ one cb-find three ct-find)
              (pin-arrow-line 15 _ two cb-find three ct-find)
              (pin-arrow-line 15 _ one rc-find two lc-find))
     p)))

(provide bottom-bar-slide go)

(define-syntax-rule (bottom-bar-slide text more ...)
  (pslide #:go (coord .5 1 'cb)
          (inset (filled-rectangle #:border-color "maroon" #:color "maroon"
                                   (+ margin margin client-w) (* 0.15 client-h))
                 0 0 0 (- margin))

          #:go (coord 0 1 'lb)
          (t/kau text #:color "white" size2)
          more ...
          ))

(define-syntax-rule (overview-slide title papers topline more ...)
  (bottom-bar-slide title
                    #:go (coord .5 .1 'cc)
                    (t/quat topline (- size2 0))
                    
                    #:go (coord 1 1 'rb)
                    (t/cant #:color "white" papers 30)
                    
                    more ... 
                    ))
(define (go)
(overview-slide "Gradual Typing" "[ESOP 12, OOPSLA 12, ESOP 13, ESOP 15, ECOOP 15, SNAPL 17]"
                "Typed and untyped languages, together"
                #:go (coord .05 .5 'lc)
                (vl-append 
                 (t/cant "Automatic & sound interoperation" size3)
                 (blank 20)
                 (t/cant "Blame when something goes wrong" size3))
          
                #:alt (#:go (coord .75 .5 'cc) (modules "tomato" "tomato" "tomato"))
                #:alt (#:go (coord .75 .5 'cc) (modules "tomato" "tomato" "lightblue"))
                #:alt (#:go (coord .75 .5 'cc) (modules "tomato" "lightblue" "tomato"))
                #:go        (coord .75 .5 'cc) (modules "lightblue" "lightblue" "lightblue"))
(overview-slide "Type Systems"
                "[ICFP 10, PADL 12, OOPSLA 12, ESOP 13, ECOOP 15, PLDI 16, ESOP 16]"
                "Advanced type systems for existing languages"
                #:go (coord .15 .5 'cc)
                (page->pict "refinement-plot.pdf" #:scale .95)
                #:go (coord .32 .5 'lc)
                (code
                 (: build-vector
                    (∀ (A)
                       (-> ([n : ℕ]
                            [p : (-> (Refine [i : ℕ] (< i n)) A)])
                           (Refine [v : (Vectorof A)]
                                   (= n (vector-length v))))))))

(overview-slide "Extensible Languages"
                "[PLDI 11, SNAPL 15, CACM 18]"
                "Making languages that make languages"
                #:alt (#:go (coord .5 .5 'cc) (scale (langs-pict #f #:fit? #t #:columns 4) 1))
                #:go (coord .5 .5 'cc) (scale (langs-pict #t #:fit? #t #:columns 4) 1)
                )

(overview-slide "Compiler Optimization"
                "[PLDI 11, OOPSLA 12, ICFP 15, ECOOP 17, OOPSLA 17]"
                "Removing overhead from abstractions"
                #:go (coord .5 .5 'cc)
                (page->pict "ShootoutBenchmarks.pdf" #:scale 3)
                )

(overview-slide "Verification"
                "[OOPSLA 12, ICFP 14, JFP 17, POPL 18]"
                "Automated verification from rich specs"
                #:go (coord .5 .5 'cc)
                (scale (bitmap "verify.png") .85))

(overview-slide "Dabbling"
                "[POPL 12, ESOP 14, PLDI 14, ICFP 15, PPOPP 16, JFP 18]"
                "Languages for ..." 
                #:go (coord .1 .2 'lt)
                (vl-append 20
                           (t/cant "Safe parallelism with 80x speedup" size3)
                           (t/cant "Packed data layout with 2x speedup" size3)
                           (t/cant "Probablistic modeling with efficiency and expressiveness" size3)
                           (t/cant "Contract semantics with extensible monitoring" size3)
                           (t/cant "Networked systems designed with concurrency in mind"
                                   size3))
                #:next
                20
                (t/cant "PL Research that's executable and checked" size3))
)
