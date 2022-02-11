#lang at-exp slideshow

(require slideshow/step slideshow/code slideshow/face 
         (only-in slideshow/slide title-size)
         (except-in "beamer.ss" title) "lib.ss" racket/gui  "thanks.ss" 
         "tslide.ss" "config.ss" "langs/main.rkt"
         "ts-intro.rkt" "stages.rkt" unstable/gui/slideshow
	 unstable/gui/pict
         "vulns.rkt")

(provide gnosys-intro)
(define (gnosys-intro)

(slide/staged [one two] #:title "GnoSys Overview" #:layout 'center
              (blank 20)
       (shadow-frame (t "Raising the level of discourse in systems building")
       (blank 30)
       	(vc-append                  
	 (t "Reliable systems require programmers to capture")
	 (hbl-append (t  " more ") (colorize (t "design knowledge") "red") (t " about their systems."))))
       ;(blank 30)
       ;'next
       (columns
        (column 500
        (vl-append 10
                   (subitem "Operating Systems")
                   ((if (= stage two) (λ (e) (colorize e "red")) values)
                    (subitem "Domain-Specific Languages"))
                   (subitem "Program Analysis")))
        (column 500
        (vl-append 10
                   (subitem "Formal Methods")
                   (subitem "Contracts")
                   (subitem "Type Systems"))))
       (blank 10)
       (hc-append 20 (t "Building on Racket") (bitmap "plt.png")))
  (define utah-logo (bitmap "utah.gif"))
  (define nu-logo (cc-superimpose (ghost utah-logo) (inset (bitmap "nu.jpg") 0 -100)))

(slide/staged
 [one two]
 #:title "GnoSys People" #:layout 'center
 (let ([item* (λ (e) (if (= stage one) (item e) (colorize (item e) "red")))])
 (ht-append 50
  (column 350
          (vc-append
           nu-logo
           (blank 30)
           (vl-append 
            (item* "Olin Shivers")
            (item "Matthias Felleisen")
            (item* "Mitchell Wand")
            (item "Panagiotis Manolios")
            (item* "Sam Tobin-Hochstadt")
            (item* "David Van Horn")
            (item* "Tony Garnock-Jones"))))
  (column 350
          (vc-append
           utah-logo
           (blank 30)
           (vl-append 
            (item "Matthew Flatt")
            (item* "Matthew Might")
            (item* "Ryan Culpepper"))))))
 (blank 50)
 (pict-if (= stage two)
          (colorize (t "Here today") "red")
          (blank))))