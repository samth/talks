#lang slideshow

(require slideshow/step slideshow/code slideshow/face (only-in slideshow/slide title-size)
         (except-in "beamer.ss" title) "lib.ss" scheme/gui "class-slide.ss" "thanks.ss" "ts-intro.ss"
         "contracts.ss" "intro.ss" "tslide.ss" "config.ss" "occur-seq.ss"
         "combine.rkt" "peano.rkt" "type-outline.rkt" "language.rkt"
         scheme/runtime-path mzlib/etc unstable/gui/slideshow)

(title '("Evolving Existing Languages")
       '("The Typed Racket Experience")
       '(("Sam Tobin-Hochstadt" "PLT @ Northeastern University"))
       "Nov 29, 2010    Indiana University")
(set-page-numbers-visible! #f)
(do-start? #f)

(slide #:layout 'center
       (scale (titlet "Programs Evolve") 2.5))

(slide #:title "Automated Testing"
       (code
        (for ([f (in-directory (find-collects-dir))]
              #:when (file-exists? f))
          (system* "racket" "-t" (path->string f))))
       (blank 50)
       (t "3 lines"))

(define-runtime-path drdr-png "drdr.png")

(slide/staged 
 [two bb]
 #:title (case stage-name [(bb) "BuildBot"] [else "DrDr"])
 #:layout 'center
 (cc-superimpose
  (case stage-name
    [(bb) (scale (bitmap "buildbot.png") .8)]
    [else (scale (bitmap drdr-png) .9)])
  (let ([p (case stage-name
             [(two) (titlet "4200+ lines")]
             [(bb) (titlet "50000+ lines")]
             [(three) (titlet "DrDr2: ???? lines")]
             [else (blank)])])
    (cc-superimpose (cellophane (colorize (filled-rounded-rectangle (+ 25 (pict-width p)) (pict-height p)) "white") .9) p))))

(slide #:layout 'center
       (scale (titlet "Languages Must Evolve") 2.5))

(slide #:title "What is Language Evolution?"
       (para "Smooth Interoperation")
       (para "Easy Migration")
       (para "Comprehensive Reuse"))

(slide #:title "Typed Racket"
       (para "Smooth Interoperation - Typed/Untyped Interaction")
       (para "Easy Migration - Types for Existing Programs")
       (para "Comprehensive Reuse - Building on Existing Languages"))

(ts-intro)

(tslide "Interoperating with Existing Languages")

(multi-sound)

(tslide "Checking Existing Languages"
        (list (subtitle-pict "Occurrence Typing") "Variable-Arity")
        (list (ghost (subtitle-pict "Occurrence Typing")) "Refinement Types")
        (list (ghost (subtitle-pict "Occurrence Typing")) "Ad-Hoc Data"))

(type-outline)

(peano1) (combine1)
(peano2) (combine2)

(tslide "Building on Existing Languages")

(language)

(tslide "Typed Racket as Research Agenda")

(slide #:title "Contracts"
       (para (code (Vectorof (Integer -> Integer))))
       (para (code (class/c ....)))
       (para (code (unit/c ....)))
       (para "Analysis using contracts")
       (blank 40)
       (t "With Stevie Strickland, Robby Findler, Matthew Flatt, David Van Horn"))

(start)

(slide #:title "Types"
       (para "First-class Classes")
       (para "Generic Operations")
       (para "Variable-Arity Functions")
       (para ".... and Inference")
       (blank 40)
       (t "With Stevie Strickland, Asumu Takikawa, Matthias Felleisen"))
(slide #:title "Macros"
       (para (code syntax-parse))
       (para "Macro Debugging")
       (para "Defensible Abstractions")
       (blank 40)
       (t "With Ryan Culpepper"))
(slide #:title "Education"
       #:layout 'center
       (para "Beginner-Level Error Messages")
       (tmod
        (code (first 3)))
       (vl-append
        (error-t "Type Checker: Polymorphic function first ")
        (error-t "    could not be applied to arguments:")
        (error-t "Domains: (Pairof a (Listof b))")
        (error-t "         (Listof a)")
        (error-t "Arguments: Positive-Fixnum"))
       
       (blank 40)
       (t "With Eli Barzilay, Matthias Felleisen"))
(slide #:title "Optimization"
       (para "Low-Level Operations for")
       (item "Structure Representation")
       (item "Memory Allocation")
       (item "...")
       
       (blank 40)
       (t "With Vincent St-Amour, Matthew Flatt"))


(thanks)


