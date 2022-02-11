#lang at-exp slideshow/widescreen
(require slideshow/step slideshow/code slideshow/face 
         ppict/2 ppict/slideshow2 "title.rkt"
         (only-in slideshow/slide title-size)
          "config.rkt" "helper.rkt"
         (except-in "beamer.rkt" title title-slide) "lib.rkt" racket/gui/base
         "tslide.rkt" 
          
         racket/runtime-path (except-in mzlib/etc identity) unstable/gui/slideshow)

(require  (prefix-in p: racket-poppler))

(define (bullet-slide #:title t . args)
  (apply slide #:title t
         (map b args)))

(define (b t) (if (symbol? t) t
                  (lc-superimpose (blank (* .8 client-w) 1)
                                  (cond [(string? t)
                                         (t/cant t size2)]
                                        [(list? t)
                                         (apply vl-append (map (lambda (t) (t/cant t size2)) t))]
                                        [else t]))))


(define (page->pict pth #:rotate [r 0] #:scale [factor 1] [page 0])
  (bitmap (scale (rotate (p:page->pict (p:pdf-page (p:open-pdf pth) page)) r) factor)))


(define (scale-h p)
  (scale p (/ (+ margin margin client-h) (pict-height p))))

(define (scale-w p)
  (scale p (/ (+ margin margin client-w) (pict-width p))))

(define (pic fname [r 1])
  (pslide #:go (coord .5 .5 'cc) (scale-h (bitmap fname))))


(define bar-colors (make-parameter "red"))

(define (rect n txt #:div [k 500] #:size [sz 100])
  (hc-append 20 (lc-superimpose (colorize (filled-rectangle (/ n k) sz)
                                          (match txt
                                            ["BC" "blue"]
                                            ["CS" "red"]
                                            [_ 
                                             (bar-colors)]))
                                (inset (colorize (t/inc (number->string n)) "white") 5))
             (t/cant txt (floor (* (/ sz 100) size2)))))

(define (chart #:title t #:size [sz 50] ns)
  (pslide/title
   t
   #:go (coord .05 .5 'lc #:sep 20)
   (chart-pic ns #:size sz)))


(define (chart-pic #:size [sz 50] ns)
  (define len (length ns))
  (define mx (apply max (map car ns)))
  (apply vl-append 20
         (for/list ([n ns])
           (rect (car n) (cadr n) #:div (/ mx 1000) #:size sz))))

(define size0 (+ 30 size1))

(define (scaleto i h)
  (scale i (/ h (pict-height i))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; talk starts here

(title-slide 2021 "eleventh")

;(slide (scale (bitmap "cake.jpg") .7))

#|

Things to mention:

- 8.0
- Syntax changes (arming goes away)
- raco cross?
- Discord
- Rhombus (and rhombus tooling improvements)
- Discourse?
- expeditor
- fun new packages?
  - gui-easy

  

|#

(slide (scale (t/cant "8.0! 🎉") 12))


(slide (scale (t/cant "8.1!") 12))
(slide (scale (t/cant "8.2!") 12))
(slide (scale (t/cant "8.3!") 12))

(slide (vl-append 80
        (t/cant "Code" size0)
        (t/cant "Community" size0)
        (t/cant "Coming Soon!" size0)))


(slide (t/cant "Code" (+ 20 size0)))

(tslide "Compatibility and consolidation")

(pic "cs-build.png")

(bullet-slide  #:title "Simplification"
               "Easier build process"
               "More platforms"
               "Single repository")

(slide #:title "Syntax Simplification"
       (t/inc "❌ syntax-arm")
       (t/inc "❌ syntax-disarm"))

(slide #:title "raco cross"
       (code |$ raco cross --target x86_64-linux dist example-dist example|))


(current-titlet (lambda (e) (lc-superimpose (ghost (rectangle (- client-w 100) 10)) (t/quat e (+ 5 size2)))))

(pslide/title
 "C Code"
 #:go (coord .05 .2 'lt #:sep 20)
 (rect 425978 "BC 5.0 (6/2010)")
 ;#:next
 (rect 249617 "BC 6.0 (2/2014)")
 ;#:next
 (rect 244543 "BC 7.0 (7/2018)")
 ;#:next
 (rect 50773 "CS 7.9.0.3 (10/2020)")
 #:next
 (rect 50667 "CS 8.3 (11/2021)"))

;; code size:
;; - 7.9.0.3 chez scheme : 50773
;; - 7.0 bc : 244543
;; - 6.0 bc : 249617
;; - 5.0 bc : 425978




(define riscv (scale (bitmap "riscv.png") .8))


(slide (t/cant "Community" (+ 20 size0)))

(pic "discord.png")


(slide #:title "Competitions"
       (scale-w (inset (hc-append 50
                           (scaleto (bitmap "dense.png")  400)
                           (scaleto (bitmap "quickscript.png") 400)
                           (scaleto (bitmap "logo-200.png") 400))
                       100)))


;; stats:

(pslide/title
 "PRs Merged"
 #:go (coord .05 .2 'lt #:sep 20)
 (rect 532 "2021" #:div .6 #:size 60)
 (rect 525 "2020" #:div .6 #:size 60)
 (rect 402 "2019" #:div .6  #:size 60)
 (rect 342 "2018" #:div .6 #:size 60)
 (rect 357 "2017" #:div .6 #:size 60)
 (rect 222 "2016" #:div .6 #:size 60)
 (rect 118 "2015" #:div .6 #:size 60)
 ;(rect 27 "2014" #:div .6 #:size 60)
 )

(bar-colors "blue")

; https://github.com/search?q=org%3Aracket+is%3Aissue+is%3Aclosed+updated%3A%3E%3D2020-11-06++sort%3Aupdated-desc&type=Issues&ref=advsearch&l=&l=

(pslide/title
 "Issues Closed"
 #:go (coord .05 .2 'lt #:sep 20)
 (rect 577 "2021" #:div .6 #:size 60)
 (rect 440 "2020" #:div .6 #:size 60)
 (rect 311 "2019" #:div .6  #:size 60)
 (rect 312 "2018" #:div .6 #:size 60); 1063
 (rect 294 "2017" #:div .6 #:size 60); 1357
 (rect 328 "2016" #:div .6 #:size 60); 1685
 (rect 126"2015" #:div .6 #:size 60) ;1811
 ;(rect 27 "2014" #:div .6 #:size 60) ;1836
 )

(bar-colors "darkgreen")


; git shortlog --before=2020-11-1 -sn |  cut -c 8- | sort -u | wc -l

(pslide/title
 "Contributors to racket/racket"
 #:go (coord .05 .2 'lt #:sep 20)
 (rect 69 "2021" #:div .1 #:size 60)
 (rect 80 "2020" #:div .1 #:size 60)
 (rect 78 "2019" #:div .1  #:size 60)
 (rect 71 "2018" #:div .1 #:size 60); 1063
 (rect 60 "2017" #:div .1 #:size 60); 1357
 (rect 68 "2016" #:div .1 #:size 60); 1685
 (rect 61 "2015" #:div .1 #:size 60) ;1811
 ;(rect 58 "2014" #:div .1 #:size 60) ;1836
 )


(pslide/title
 "New Contributors to racket/racket"
 #:go (coord .05 .2 'lt #:sep 20)
 (rect 27 "2021" #:div .1 #:size 60)
 (rect 65 "2020" #:div .1 #:size 60)
 (rect 38 "2019" #:div .1  #:size 60)
 (rect 30 "2018" #:div .1 #:size 60); 1063
 (rect 27 "2017" #:div .1 #:size 60); 1357
 (rect 40 "2016" #:div .1 #:size 60); 1685
 (rect 26 "2015" #:div .1 #:size 60) ;1811
 ;(rect 58 "2014" #:div .1 #:size 60) ;1836
 )


;; (slide (t/cant "Racket Survey" (+ 20 size0)))

;; (slide (t/cant "422 Responses!" size0)
;;        'next
;;        (t/cant "Huge Thanks to Stephen De Gabrielle"))

;; (chart #:size 75 #:title "Do You Use Racket Currently?"
;;        '((339 "Yes")
;;          (61 "I've stopped")
;;          (22 "Never used")))
;; (chart #:size 75 #:title "Platforms"
;;        '((293 "Linux")
;;          (179 "Mac")
;;          (122 "Windows")
;;          (13 "*BSD")))
;; (chart #:size 75 #:title "How Long Have You Used Racket?"
;;        '((177 ">3 Years")
;;          (54 "2-3 Years")
;;          (70 "1-2 Years")
;;          (57 "3-12 Months")
;;          (61 "<3 Months")))
;; (chart #:size 60 #:title "How do you describe yourself as a developer?"
;;        `((205 "Professional")
;;          (,(+ 58 49) "Researcher")
;;          (88 "Autodidact")
;;          (68 "Student")
;;          (56 "Teacher + Lecturer")
;;          (54 "Professional (Other)")
;;          (10 "Parent")))
;; (chart #:size 60 #:title "Editors!"
;;        `((285 "DrRacket")
;;          (204 "Emacs")
;;          (63 "vim")
;;          (51 "VS Code")
;;          (5 "Sublime")
;;          (4 "Atom")
;;          (4 "Notepad++")))

;; (current-main-font "Cantarell")

;; (define quotes
;;   `(("It would be helpful if I knew where to look to find some easy starter tasks"
;;      "easier finding sources on GitHub")
;;     ("I need to get a sense where contributions are most needed, first."
;;     "I think having a guide/mentor/helper would help")
;;     ("how to find and run the test suite (especially for legacy code)"
;;     "I wish I knew how to make a \"clean\" copy of Racket and work there ")))

;; (slide #:title "How can we make contributing easier?"
;;        'alts
;;        (for/list ([l quotes])
;;          (add-between (map shadow-frame (map para l)) (blank 30))))

;(chart #:title "")


;; prs merged in 2020: 525
;; in 2019: 402
;; in 2018: 342
;; in 2017: 357



(slide (t/cant "Coming Soon!" (+ 20 size0)))

(pic "discourse.png" .8)

(pslide #:go (coord .5 .5 'cc) (inset (scale (bitmap "expeditor.png") .6) 20))

(slide #:title "Transient Typed Racket (again)"
       (scale (bitmap "transient.png") .6))

(slide #:title "Typed Racket with Kinds"
       (scale (bitmap "kinds.png") .6))

;(start-at-recent-slide)
;(slide #:title "Racket Week 2021")

(slide (t/cant "Rhombus" size0))

(pic "rhombus.png" .4)
(pic "rhombus-examples.png" .4)

;; (bullet-slide #:title "Rhombus goals"
       

;;               "More generics"
;;               "More consistency"
;;               "Lower barriers of entry"
;;               `("Extend smooth, fine-grained language""  extensibility to a broader set of syntaxes")
;;               `("Make backwards-incompatible changes""  that reflect newer thinking (e.g., structs)")
;;               )


;; (define id "define identity(x): x")

;; (define fib @string-append{
;; define fib(n):
;;   cond
;;    | n == 0: 0
;;    | n == 1: 1
;;    | else: fib(n-1) + fib(n-2)})

;; (define print_sexp @string-append{
;; define print_sexp(v):
;;   match v
;;    | empty: display("()")
;;    | cons(a, d):
;;        if is_list(d)
;;         | display("(")
;;           print_sexp(a)
;;           for (v = in_list(d)):
;;             display(" ")
;;             print_sexp(v)
;;           display(")")
;;         | ...
;;    | v: print_atom(v)
;;    })

;; (write print_sexp)

;; (require fancy-app)
;; (slide #:title "Shrubbery notation"
;;        'alts
;;        (for/list ([i (list id fib print_sexp)])
;;          (list (apply vl-append 0 (map (t/inc _ size3) (string-split i "\n"))))))

;; (slide #:title "Shrubbery notation"
;;        (scale
;;        (code 
;; (group define
;;        fib
;;        (parens (group n))
;;        (block
;;         (group match
;;                n
;;                (alts
;;                 (block (group 0 (block (group 0))))
;;                 (block (group 1 (block (group 1))))
;;                 (block
;;                  (group n
;;                         (block
;;                          (group fib
;;                                 (parens (group n - 1))
;;                                 +
;;                                 fib
;;                                 (parens (group n - 2)))))))))))
;; .8))


;; (pslide #:go (coord .5 0 'ct) (scale (bitmap "iwant.jpeg") .5))

(require images/icons/stickman)

(when #t
(pslide #:go (coord .5 .5 'cc #:sep 100)
        (apply hc-append
               (for/list ([t  (in-range 0 1 1/12)])
                 (bitmap (running-stickman-icon t #:height 64))))
        ;(blank 20)
        (t/cant "Thank You" size0)))
;(start-at-recent-slide)
(module+ no)
