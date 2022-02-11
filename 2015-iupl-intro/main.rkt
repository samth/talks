#lang slideshow



(require slideshow/step slideshow/code slideshow/face 
         unstable/gui/ppict unstable/gui/pslide
         (only-in slideshow/slide title-size)
          "config.ss"
         (except-in "beamer.ss" title) "lib.ss" racket/gui   
         "tslide.ss" "helper.rkt"
         racket/runtime-path (except-in mzlib/etc identity) unstable/gui/slideshow)

(define (pic fname [r 1])
  (pslide #:go (coord .5 .5 'cc)
        (scale (bitmap fname) r)))

(do-start? #f)
(require ppict-slide-grid)
;(set-grid-base-pict!)


(pslide
 #:go (coord 1.1 .7 'cc) 
 (cellophane (bitmap "seal.png") 0.1)
 #:go (coord 0.01 .85 'lc)
 (t/quat "Sam Tobin-Hochstadt" size2)
 #:go (coord 0.01 .1 'lc)
 (t/quat "Welcome to PL @ IU" (+ 20 size1)))

#;(define (pic fname [r 1])
  (pslide #:go (coord .5 .5 'cc)
        (scale (bitmap fname) r)))

(pic "IU.jpg" .25)


(define (hl p) (cc-superimpose
                (inset (cellophane (colorize (filled-rectangle (pict-width p) (pict-height p))
                                             "pink") .7) -20)
                                p))

(tslide* "The Faculty")


(define (prof name paper  course [paper2 ""])
  (pslide #:go (coord .01 .05 'lc)
          (t/quat name size1)
          #:go (coord .05 .3 'lc)
          (t/quat "A recent paper" size2)
          #:go (coord .15 .42 'lc)
          (t/cant paper size2)
          (t/cant paper2 size2)
          #:go (coord .05 .62 'lc)
          (t/quat "Teaching" size2)
          #:go (coord .15 .72 'lc)
          (t/cant course size2)))

(prof "Dan Friedman" "The Little Prover (MIT Press)" "521: Programming Languages")
(prof "Ryan Newton" "Freeze after writing" "H211: Intro to parallel programming")

(prof "Amr Sabry" "Reversible Communicating Processes" "343: Data Structures")

(prof "Chung-chieh Shan" "Combinators for impure yet" "629: Probabalistic Programming" " hygienic code generation")

(prof "Jeremy Siek" "Refined criteria for gradual typing" "523: Compilers")

(prof "Sam Tobin-Hochstadt" "Towards practical gradual typing"
              "522: PL Theory")

(tslide* "PL Wonks")

(pslide #:go (coord .01 .05 'lc)
        (t/quat "The Meeting" size1)
        #:go (coord .05 .3 'lc)
        (t/quat "Every Friday" size2)
        #:go (coord .05 .45 'lc)
        (t/quat "Lindley 101 (right here!)" size2)
        #:go (coord .05 .6 'lc)
        (t/quat "Includes delicious baked goods" size2))


(pslide #:go (coord .01 .05 'lc)
        (t/quat "The Organization" size1)
        #:go (coord .05 .3 'lc)
        (t/quat "Managed by Cameron Swords" size2)
        #:go (coord .05 .45 'lc)
        (t/quat "Organization meeting next week" size2)
        #:go (coord .05 .6 'lc)
        (t/quat "Volunteer to talk and bake" size2))


(pslide #:go (coord .01 .05 'lc)
        (t/quat "The Internet" size1)
        #:go (coord .05 .3 'lc)
        (t/inc "wonks.github.io" size2)
        #:go (coord .05 .45 'lc)
        (t/inc "pl-wonks-l@list.indiana.edu" size2)
        #:go (coord .25 .8 'lc)
        (t/quat "Sign up for the list!" size1))

(tslide "Other things to know")

(pslide #:go (coord .01 .05 'lc)
        (t/quat "Offices" size1)
        #:go (coord .15 .3 'lt)
        (t/quat "330A" size2)
        (t/quat "330I" size2)
        (t/quat "328" size2)
        (t/quat "315" size2)
        (t/quat "125" size2)
        (blank 30)
        (t/quat "CREST" size2))

(pslide #:go (coord .01 .05 'lc)
        (t/quat "Other Meetings" size1)
        #:go (coord .1 .3 'lt)
        (t/quat "Logic Seminar" size2)
        (t/quat "Research Group Meetings" size2)
        (t/quat "Department Colloquium" size2))

(tslide* "What to do when you're new")

(pslide #:go (coord .01 .05 'lc)
        (t/quat "Your First Year" size1)
        #:go (coord .1 .25 'lt)
        (t/quat "Sign up for classes" size2)
        (t/quat "Meet with your mentor/advisor" size2)
        (t/quat "Start working on research" size2)
        (t/quat "Do well in your classes" size2)
        (t/quat "Talk with your fellow students" size2)
        (blank 40)
        #:next
        (t/quat "Have fun!" size1))
