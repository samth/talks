#lang racket/gui

;; Taken from Asumu Takikawa

(require pict-utils/npict
         slideshow
         slideshow/flash
         slideshow/play
         unstable/gui/pict
         (prefix-in pp: unstable/gui/ppict)
         (only-in unstable/gui/ppict ppict-do)
         unstable/sequence
         unstable/gui/pslide
         unstable/gui/slideshow)

(provide interface-pict
         contract-pict-1
         contract-pict-2
         contract-pict-4
         contract-pict-5
         contract-pict-6
         darker lighter
         chaperone-pict
         prompt-chap-pict
         shade-pict
         ho-wrapper
         blame-color-1
         blame-color-2
         t/quat t/kau
         t/cant
         t/inc
         t/section
         curly
         pslide/staged
         pslide/play
         pslide/play/staged
         make-stack
         axes-pict
         axes-with-paper
         web-components-pict-1
         web-components-pict-2
         gt-pict-1
         gt-pict-2
         ops-pict
         gt-intro-pict
         component-flow-pict
         component-flow-pict-2
         prompt-doc-1-pict
         prompt-doc-2-pict
         web-src-pict
         web-ref-pict
         typed-color untyped-color
         size1 size2 size3 size4)

;; make font-specific t functions
(define ((make-font-t font) content
                            [size (current-font-size)]
                            [angle 0]
                            #:color [color #f]
                            #:extra-style [extra-style '()])
  (define text-pict
    (text content (append extra-style font) size angle))
  (if color
      (colorize text-pict color)
      text-pict))

;;
(define blame-color-1 "DarkRed")
(define blame-color-2 "Midnight Blue")

;; sizing
(define size1 70)
(define size2 50)
(define size3 40)
(define size4 30)

;; quattrocento font
(define t/quat (make-font-t "Quattrocento"))

;; cantarell
(define t/cant (make-font-t "Cantarell"))

;; inconsolata
(define t/inc (make-font-t "Inconsolata"))

;; kaushan
(define t/kau (make-font-t "Kaushan Script"))

;; section separator
(define (t/section str)
  (colorize ((make-font-t "Quattrocento") str size1)
            "Midnight Blue"))

;; darker colors
(define (darker color [mag 10])
    (define c (make-object color% color))
    (define-values (r g b)
      (values (send c red) (send c green) (send c blue)))
    (make-color (max 0 (- r mag)) 
                (max 0 (- g mag)) 
                (max 0 (- b mag))))

(define (lighter color [mag 10])
    (define c (make-object color% color))
    (define-values (r g b)
      (values (send c red) (send c green) (send c blue)))
    (make-color (min 255 (+ r mag)) 
                (min 255 (+ g mag)) 
                (min 255 (+ b mag))))

;;; picts from here

(define prompt-doc-1-pict (bitmap "prompt-docs.png"))
(define prompt-doc-2-pict (bitmap "prompt-docs-2.png"))
(define web-src-pict (bitmap "web-src.png"))
(define web-ref-pict (bitmap "webapp-ref.png"))

;; pslide helper
(define-syntax-rule (pslide/staged [name ...] arg ...)
  (staged [name ...] (pslide arg ...)))

;; ho wrapper pict
(define (ho-wrapper p lst #:inc [inc 30])
  (define-values (result _)
    (for/fold ([p p]
               [diam (+ inc (max (pict-width p)
                                (pict-height p)))])
      ([(color border) (in-pairs lst)])
      (values (cc-superimpose
               (circle/border diam 
                              #:color color
                              #:border-color border)
               p)
              (+ diam inc))))
  (npict (node #:pict result)
         (line #:from (coord (- (+ 25 (/ (pict-width result) 2))) 0)
               #:to (coord -15 0)
               #:line-width 2
               #:arrow? 10)
         (line #:from (coord 15 0)
               #:to (coord (+ 25 (/ (pict-width result) 2)) 0)
               #:line-width 2
               #:arrow? 10)))

;; nice curly brace
(define (curly w h)
  (define p (t/quat "{" 100))
  (define p-w (pict-width p))
  (define p-h (pict-height p))
  (scale p (/ w p-w) (/ h p-h)))

;; more colors
(define untyped-color "mistyrose")
(define typed-color "aliceblue")
(define untyped-border (darker untyped-color 20))
(define typed-border (darker typed-color 20))

;; shade out screen
(define shade-pict
  (cellophane 
   (colorize (filled-rectangle client-w client-h) "whitesmoke")
   0.8))

(define server-pict
  (cc-superimpose (rectangle/border 200 100 #:border-color "gray")
                  (t "server")))

(define client-pict
  (cc-superimpose (rectangle/border 200 100 #:border-color "gray")
                  (t "client")))

(define interface-pict
  (npict (node #:name "server"
               #:at (coord 0 200)
               #:pict server-pict)
         (node #:name "client"
               #:at (coord 0 -200)
               ;; FIXME
               #:pict client-pict)
         (line #:from "server"
               #:to "client"
               #:start-align 'cb
               #:end-align 'ct
               #:arrow? #t
               #:color "black")
         (node #:at (coord 0 0)
               #:pict (linestyle 'long-dash (hline 300 1)))))

;; illustration of contracts and component boundaries
#|
(define party-color-1 (make-color 240 250 240))
(define party-border-1 (make-color 200 220 200))
(define party-color-2 (make-color 240 240 250))
(define party-border-2 (make-color 200 200 220))
|#
(define party-color-1 (make-color 220 250 220))
(define party-border-1 (make-color 180 220 180))
(define party-color-2 (make-color 220 220 250))
(define party-border-2 (make-color 180 180 220))

;; a pict used in the intro section
;;
;; this pict should really use styles to reduce copy+paste
(define (axes-pict stage)
  (npict
   (node #:at (coord 0 400)) ;; dummy pict
   ;; tick marks (manually, though doing it via a list
   ;; could be better)
   (line #:from (coord 0 0) #:to (coord 0 -15)
         #:color "gray" #:line-width 3)
   (line #:from (coord 140 0) #:to (coord 140 -15)
         #:color "gray" #:line-width 3)
   (line #:from (coord 280 0) #:to (coord 280 -15)
         #:color "gray" #:line-width 3)
   (line #:from (coord 420 0) #:to (coord 420 -15)
         #:color "gray" #:line-width 3)

   ;; labels on bottom
   (node #:at (coord 0 -40) 
         #:pict (t/cant "∅" 20))
   (node #:at (coord 140 -40) 
         #:pict (t/cant "Types" 20))
   (node #:at (coord 280 -40)
         #:pict (t/cant "Contracts" 20))
   (node #:at (coord 420 -40)
         #:pict (t/cant "Gradual typing" 20))
   
   ;; labels on side
   (node #:at (coord -40 250 'rc)
         #:pict (t/inc "wcm" 20))
   (node #:at (coord -40 200 'rc)
         #:pict (t/inc "ccm" 20))
   (node #:at (coord -40 150 'rc)
         #:pict (t/inc "call/comp" 20))
   (node #:at (coord -40 100 'rc)
         #:pict (t/inc "abort" 20))
   (node #:at (coord -40 50 'rc)
         #:pict (t/inc "prompt" 20))
   
   ;; more ticks
   (line #:from (coord 0 250) #:to (coord -15 250)
         #:line-width 3 #:color "gray")
   (line #:from (coord 0 200) #:to (coord -15 200)
         #:line-width 3 #:color "gray")
   (line #:from (coord 0 150) #:to (coord -15 150)
         #:line-width 3 #:color "gray")
   (line #:from (coord 0 100) #:to (coord -15 100)
         #:line-width 3 #:color "gray")
   (line #:from (coord 0 50) #:to (coord -15 50)
         #:line-width 3 #:color "gray")
   
   ;; grid lines
   (line #:from (coord 0 250) #:to (coord 500 250)
         #:line-width 1 #:color "lightgray")
   (line #:from (coord 0 200) #:to (coord 500 200)
         #:line-width 1 #:color "lightgray")
   (line #:from (coord 0 150) #:to (coord 500 150)
         #:line-width 1 #:color "lightgray")
   (line #:from (coord 0 100) #:to (coord 210 100)
         #:line-width 1 #:color "lightgray")
   (line #:from (coord 350 100) #:to (coord 500 100)
         #:line-width 1 #:color "lightgray")
   (line #:from (coord 0 50) #:to (coord 210 50)
         #:line-width 1 #:color "lightgray")
   (line #:from (coord 350 50) #:to (coord 500 50)
         #:line-width 1 #:color "lightgray")
   (if (not (eq? stage 'this-talk))
       (line #:from (coord 0 50) #:to (coord 500 50)
             #:color "lightgray" #:line-width 1)
       (node))
   (if (not (eq? stage 'this-talk))
       (line #:from (coord 0 100) #:to (coord 500 100)
             #:color "lightgray" #:line-width 1)
       (node))
   (line #:from (coord 0 0) #:to (coord 0 300)
         #:color "lightgray" #:line-width 1)
   (line #:from (coord 140 0) #:to (coord 140 300)
         #:color "lightgray" #:line-width 1)
   (line #:from (coord 280 0) #:to (coord 280 25)
         #:color "lightgray" #:line-width 1)
   (line #:from (coord 280 125) #:to (coord 280 300)
         #:color "lightgray" #:line-width 1)
   (if (eq? stage 'init)
       (line #:from (coord 280 0) #:to (coord 280 300)
             #:color "lightgray" #:line-width 1)
       (node))
   (line #:from (coord 420 0) #:to (coord 420 300)
         #:color "lightgray" #:line-width 1)

   ;; axes
   (line #:from (coord 0 0) #:to (coord 500 0)
         #:color "gray" #:line-width 3 #:arrow? 10)
   (line #:from (coord 0 0) #:to (coord 0 300)
         #:color "gray" #:line-width 3 #:arrow? 10)
   
   ;; box
   (node #:at (coord 280 75)
         #:pict 
         (show (cc-superimpose
                (rectangle/border
                 140 100
                 #:color typed-color
                 #:border-color typed-border)
                (t/cant "talk" size4))
               (eq? stage 'this-talk)))))

(define (axes-with-paper stage-name)
  (npict (node #:pict (axes-pict 'init))
         (node #:at (coord 95 35)
               #:pict (frame (scale (bitmap "paper.png") 0.5)))
         (if (eq? stage-name 'this-talk-1)
             (node #:at (coord 60 -45)
                   #:pict 
                   (cc-superimpose
                          (rectangle/border
                           140 100
                           #:color typed-color
                           #:border-color typed-border)
                          (t/cant "talk" size4)))
             (node))))
  
;; make various stacks
;; Listof<(U Pict String)> [Int] [Int] -> Pict
(define (make-stack lst
                    [width 55]
                    [height 15]
                    #:color [color "white"]
                    #:border-color [border-color "gray"]
                    ;; multiple values, yes/no
                    #:mv [mv #f])
  (define sub-picts
    (for/list ([x lst])
      (cc-superimpose
       (rectangle/border
        width height
        #:color color
        #:border-color border-color)
       (if (pict? x)
           x
           (t/inc x 12)))))
  (define pict (apply vc-append sub-picts))
  (if mv (values pict sub-picts) pict))
      
;; continuation marks
(define right-disk (colorize (disk 5) "darkblue"))
(define (cmark-box #:store-color [store-color "whitesmoke"]
                   #:store-border [store-border "gray"]
                   #:color [color "darkblue"])
  (cc-superimpose 
   (circle/border 25
                  #:color store-color
                  #:border-color store-border
                  #:border-width 1)
   (hc-append 
    3
    (colorize (disk 5) "darkblue")
    right-disk)))

(define (small-stack #:color [color "white"]
                     #:border-color [border-color "gray"])
  (apply vc-append
         (make-list 
          5 
          (rectangle/border 50 10 
                            #:color color
                            #:border-color border-color))))

(define (big-stack #:color [color "white"]
                   #:border-color [border-color "gray"])
  (apply vc-append
         (make-list 
          10
          (rectangle/border 50 10
                            #:color color
                            #:border-color border-color))))

;; blame
(define (blame-pict str #:big? [big? #f])
  (cc-superimpose
   (colorize (filled-flash (if big? 150 100) 50) "red")
   (colorize (t/cant (string-append "Blame " str) 15
                     #:extra-style '(bold))
             "white")))

;; some intro picts, one for showing regular old component import/export
;; and the other one emphasizing that it's about *stacks*
;;
(define (web-components-pict-1)
  (npict
   (node #:name "above"
         #:at (coord 0 25 'cb)
         #:pict (component-box (t/cant "web server" 15) 100 50
                               #:color party-color-1
                               #:border-color party-border-1))
   (node #:name "boundary"
         #:at (coord 0 0)
         #:pict (linestyle 'short-dash (hline 400 1)))
   (node #:name "point1" #:at (coord 30 0) #:pict (disk 10))
   (node #:name "point2" #:at (coord -30 0) #:pict (disk 10))
   (node #:name "contract1" #:at (coord -90 -30) #:pict (t/inc "request?" 12))
   (node #:name "contract2" #:at (coord 130 30) #:pict (t/inc "(-> request? response?)" 12))
   (line #:from "contract1" #:to "point2" #:start-align 'ct)
   (line #:from "contract2" #:to "point1" #:start-align 'cb)
   (line #:from (coord 30 25) #:to (coord 30 -25) #:arrow? #t
         #:start-align 'cb #:end-align 'ct)
   (line #:from (coord -30 -25) #:to (coord -30 25) #:arrow? #t
         #:start-align 'ct #:end-align 'cb)
   (node #:name "below"
         #:at (coord 0 -25 'ct)
         #:pict (component-box (t/cant "servlet" 15) 100 50
                               #:color party-color-2
                               #:border-color party-border-2))))

(define (web-components-pict-2 stage)
  (define-values (p2 frames2)
    (make-stack #:mv #t
                '("" "" "" "" "" "" "")
                #:color party-color-2
                #:border-color party-border-2))
  (define-values (p1 frames1)
    (make-stack #:mv #t
                '("" "" "" "")
                #:color party-color-1
                #:border-color party-border-1))
  (npict
   ;; labels at the top
   (node #:at (coord 190 100 'rt)
         #:pict (backdrop (t/cant "web server" 15)
                          #:color party-color-1))
   (node #:at (coord 190 -120 'rb)
         #:pict (backdrop (t/cant "servlet" 15)
                          #:color party-color-2))
   ;; communication lines
   (line #:from (third frames1) #:to (fourth frames2)
         #:start-align 'rc #:end-align 'rc
         #:start-angle (- (/ pi 4)) #:end-angle (+ pi (/ pi 4))
         #:arrow? #t)
   (line #:to (second frames1) #:from (third frames2)
         #:start-align 'lc #:end-align 'lc
         #:start-angle (- pi (/ pi 4)) #:end-angle (/ pi 4)
         #:arrow? #t)
   ;;
   (node #:name "above"
         #:at (coord 0 25 'cb)
         #:pict p1)
   (node #:name "boundary"
         #:at (coord 0 0)
         #:pict (linestyle 'short-dash (hline 400 1)))
   (node #:name "point" #:at (coord 0 0) #:pict (disk 10))
   (node #:at (coord 45 0) 
         #:pict (show (linestyle 'long-dash (rectangle 30 30))
                      (or (eq? stage 'missing)
                          (eq? stage 'last))))
   (node #:at (coord -45 0) 
         #:pict (show (linestyle 'long-dash (rectangle 30 30))
                      (or (eq? stage 'missing)
                          (eq? stage 'last))))
   (line #:from "above" #:to "below" #:arrow? #t
         #:start-align 'cb #:end-align 'ct)
   (node #:name "below"
         #:at (coord 0 -25 'ct)
         #:pict p2)))

;; contract disk in picts
(define ctc-circle (disk 10))

(define contract-pict-1
  (npict
   ;; labels at the top
   (node #:at (coord 190 100 'rt)
         #:pict (backdrop (t/cant "component A" 15)
                          #:color party-color-1))
   (node #:at (coord 190 -120 'rb)
         #:pict (backdrop (t/cant "component B" 15)
                          #:color party-color-2))
   ;;
   (node #:name "above"
         #:at (coord 0 25 'cb)
         #:pict (make-stack '("" "" "" "" "") 65
                            #:color party-color-1
                            #:border-color party-border-1))
   (node #:name "boundary"
         #:at (coord 0 0)
         #:pict (linestyle 'short-dash (hline 400 1)))
   (node #:name "point"
         #:at (coord 0 0)
         #:pict (disk 10))
   (node #:name "contract"
         #:at (coord -80 30)
         #:pict (t/cant "prime?" 15))
   (line #:from "contract" #:to "point"
         #:start-align 'cb)
   (line #:from "above" #:to "below" #:arrow? #t
         #:start-align 'cb #:end-align 'ct
         ;#:start-angle (- (/ pi 6))
         ;#:end-angle (+ pi (/ pi 6))
         )
   (node #:name "comment"
         #:at (coord 10 0 'lb)
         #:pict (t/cant "protected channel" 15))
   (node #:name "below"
         #:at (coord 0 -25 'ct)
         #:pict (make-stack '("" "" "" "" "") 65
                            #:color party-color-2
                            #:border-color party-border-2))))

;; This one depicts prompt tags and aborting
(define contract-pict-2
  (npict
   ;; labels at the top
   (node #:at (coord 190 100 'rt)
         #:pict (backdrop (t/cant "component A" 15)
                          #:color party-color-1))
   (node #:at (coord 190 -120 'rb)
         #:pict (backdrop (t/cant "component B" 15)
                          #:color party-color-2))
   ;;
   (node #:name "above"
         #:at (coord 0 25 'cb)
         #:pict (make-stack
                 '("" "% ☆ h" "" "" "")
                 65
                 #:color party-color-1
                 #:border-color party-border-1))
   ;; prompt note
   (node #:name "prompt" #:at (coord 28 75 'lc))
   (node #:name "abort"  #:at (coord 28 -93 'lc))
   ;; abort path
   (line #:from "abort" #:to "prompt"
         #:start-align 'rc #:end-align 'rc
         #:start-angle (/ pi 5) #:end-angle (+ (/ pi 2) (/ pi 3))
         #:arrow? #t)
   ;; comment
   (node #:at (coord 60 0 'lb)
         #:pict (t/cant "unprotected channel" 15))
   (node #:name "boundary"
         #:at (coord 0 0)
         #:pict (linestyle 'short-dash (hline 400 1)))
   (node #:name "point"
         #:at (coord 0 0)
         #:pict (disk 10))
   (node #:name "contract"
         #:at (coord -80 30)
         #:pict (t/inc "prime?" 15))
   (line #:from "contract" #:to "point"
         #:start-align 'cb)
   (line #:from "above" #:to "below" #:arrow? #t
         #:start-align 'cb #:end-align 'ct)
   (node #:name "below"
         #:at (coord 0 -25 'ct)
         #:pict (make-stack
                 '("" "" "" "" "abort ☆ 4")
                 65
                 #:color party-color-2
                 #:border-color party-border-2))))

;; these next few picts show the subtleties of blame with
;; prompt tag contracts
(define (contract-pict-4 #:induce? [induce? #f] #:blame? [blame? #f])
  (define-values (s1 s1-picts)
    (make-stack
     #:mv #t
     '("" "% ★ h" "" "" "")
     65
     #:color party-color-1
     #:border-color party-border-1))
  (npict
   (node #:at (coord 120 30)
         #:pict (if blame? (blame-pict "B") (blank 1 1)))
   ;; labels at the top
   (node #:at (coord 190 100 'rt)
         #:pict (backdrop (t/cant "component A" 15)
                          #:color party-color-1))
   (node #:at (coord 190 -120 'rb)
         #:pict (backdrop (t/cant "component B" 15)
                          #:color party-color-2))
   ;;
   (node #:name "above"
         #:at (coord 0 25 'cb)
         #:pict s1)
   ;; prompt note
   (node #:name "prompt" #:at (coord 28 75 'lc))
   (node #:name "abort"  #:at (coord 28 -93 'lc))
   ;; prompt tag migration
   (node #:at (coord -100 0) #:pict ctc-circle)
   (node #:name "tag-A" #:at (coord -100 50)
         #:pict (t/inc "★" 25))
   (node #:name "tag-B" #:at (coord -100 -50)
         #:pict (t/inc "☆" 25))
   (line #:from "tag-B" #:to "tag-A"
         #:start-align 'ct #:end-align 'cb
         #:arrow? #t)
   ;; abort path
   (line #:from "abort" #:to "prompt"
         #:start-align 'rc #:end-align 'rc
         #:start-angle (/ pi 5) #:end-angle (+ (/ pi 2) (/ pi 3))
         #:arrow? #t)
   (node #:name "boundary"
         #:at (coord 0 0)
         #:pict (linestyle 'short-dash (hline 400 1)))
   (node #:name "point"
         #:at (coord 0 0)
         #:pict (disk 10))
   (line #:from "above" #:to "below" #:arrow? #t
         #:start-align 'cb #:end-align 'ct)
   (node #:name "below"
         #:at (coord 0 -25 'ct)
         #:pict (make-stack
                 '("" "" "" "" "abort ☆ 4")
                 65
                 #:color party-color-2
                 #:border-color party-border-2))
   ;; induced contract
   (if induce?
       (node #:name "induced" #:at (coord 57 0) #:pict ctc-circle)
       (node))
   ;; line showing induced contract
   (if induce?
       (line #:from (second s1-picts) #:to "induced"
             #:start-align 'cb #:end-align 'lt
             #:arrow? #t #:line-width 2 #:color "red")
       (node))))

;;
(define (contract-pict-5 #:blame? [blame? #f]
                         #:induce? [induce? #f]
                         #:ho? [ho? #f])
  (define-values (s2 s2-picts)
    (make-stack
     #:mv #t
     `("" "" "" "" ,(if ho? "abort ★ f" "abort ★ 4"))
     65
     #:color party-color-2
     #:border-color party-border-2))
  (npict
   ;; blame
   (node #:at (coord 120 -30)
         #:pict (if (and blame? (not ho?))
                    (blame-pict "B")
                    (blank 1 1)))
   (node #:at (coord 120 30)
         #:pict (if (and blame? ho?)
                    (blame-pict "A") 
                    (blank 1 1)))
   ;; labels at the top
   (node #:at (coord 190 100 'rt)
         #:pict (backdrop (t/cant "component A" 15)
                          #:color party-color-1))
   (node #:at (coord 190 -120 'rb)
         #:pict (backdrop (t/cant "component B" 15)
                          #:color party-color-2))
   ;;
   (node #:name "above"
         #:at (coord 0 25 'cb)
         #:pict (make-stack
                 '("" "% ☆ h" "" "" "")
                 65
                 #:color party-color-1
                 #:border-color party-border-1))
   ;; prompt note
   (node #:name "prompt" #:at (coord 28 75 'lc))
   (node #:name "abort"  #:at (coord 28 -93 'lc))
   ;; prompt tag migration
   (node #:at (coord -100 0) #:pict ctc-circle)
   (node #:name "tag-A" #:at (coord -100 50)
         #:pict (t/inc "☆" 25))
   (node #:name "tag-B" #:at (coord -100 -50)
         #:pict (t/inc "★" 25))
   (line #:from "tag-A" #:to "tag-B"
         #:start-align 'cb #:end-align 'ct
         #:arrow? #t)
   ;; abort path
   (line #:from "abort" #:to "prompt"
         #:start-align 'rc #:end-align 'rc
         #:start-angle (/ pi 5) #:end-angle (+ (/ pi 2) (/ pi 3))
         #:arrow? #t)
   (node #:name "boundary"
         #:at (coord 0 0)
         #:pict (linestyle 'short-dash (hline 400 1)))
   (node #:name "point"
         #:at (coord 0 0)
         #:pict (disk 10))
   (line #:from "above" #:to "below" #:arrow? #t
         #:start-align 'cb #:end-align 'ct)
   (node #:name "below"
         #:at (coord 0 -25 'ct)
         #:pict s2)
   ;; induced contract
   (if induce?
       (node #:name "induced" #:at (coord 57 0) #:pict ctc-circle)
       (node))
   ;; line showing induced contract
   (if induce?
       (line #:from (fifth s2-picts) #:to "induced"
             #:start-align 'ct #:end-align 'lb
             #:arrow? #t #:line-width 2 #:color "red")
       (node))))

;; the final case, both sides have a contract
(define (contract-pict-6 #:induce? [induce? #f] #:blame? [blame? #f])
  (define-values (s1 s1-picts)
    (make-stack
     #:mv #t
     '("" "% ★ h" "" "" "")
     65
     #:color party-color-1
     #:border-color party-border-1))
  (define-values (s2 s2-picts)
    (make-stack
     #:mv #t
     '("" "" "" "" "abort ★ f")
     65
     #:color party-color-2
     #:border-color party-border-2))
  (npict
   ;; blame
   (node #:at (coord 120 -30)
         #:pict (if blame?
                    (blame-pict "B")
                    (blank 1 1)))
   ;; labels at the top
   (node #:at (coord 190 100 'rt)
         #:pict (backdrop (t/cant "component A" 15)
                          #:color party-color-1))
   (node #:at (coord 190 -120 'rb)
         #:pict (backdrop (t/cant "component B" 15)
                          #:color party-color-2))
   ;;
   (node #:name "above"
         #:at (coord 0 25 'cb)
         #:pict s1)
   ;; prompt note
   (node #:name "prompt" #:at (coord 28 75 'lc))
   (node #:name "abort"  #:at (coord 28 -93 'lc))
   ;; prompt tag migration
   (node #:at (coord -100 0) #:pict ctc-circle)
   (node #:name "tag-A" #:at (coord -100 50)
         #:pict (t/inc "★" 25))
   (node #:name "tag-B" #:at (coord -100 -50)
         #:pict (t/inc "★" 25))
   (node #:name "C1" #:at (coord -150 50)
         #:pict (t/cant "C" 15))
   (node #:name "C2" #:at (coord -150 -50)
         #:pict (t/cant "C" 15))
   (line #:from "C1" #:to "tag-A" 
         #:start-align 'rc #:end-align 'lc #:arrow? #t)
   (line #:from "C2" #:to "tag-B"
         #:start-align 'rc #:end-align 'lc #:arrow? #t)
   ;; abort path
   (line #:from "abort" #:to "prompt"
         #:start-align 'rc #:end-align 'rc
         #:start-angle (/ pi 5) #:end-angle (+ (/ pi 2) (/ pi 3))
         #:arrow? #t)
   (node #:name "boundary"
         #:at (coord 0 0)
         #:pict (linestyle 'short-dash (hline 400 1)))
   (node #:name "point"
         #:at (coord 0 0)
         #:pict (disk 10))
   (line #:from "above" #:to "below" #:arrow? #t
         #:start-align 'cb #:end-align 'ct)
   (node #:name "below"
         #:at (coord 0 -25 'ct)
         #:pict s2)
   ;; induced contract
   (if induce?
       (node #:name "induced" #:at (coord 57 0) #:pict ctc-circle)
       (node))
   ;; line showing induced contract
   (if induce?
       (line #:from (second s1-picts) #:to "induced"
             #:start-align 'cb #:end-align 'lt
             #:arrow? #t #:line-width 2 #:color "red")
       (node))
   (if induce?
       (line #:from (fifth s2-picts) #:to "induced"
             #:start-align 'ct #:end-align 'lb
             #:arrow? #t #:line-width 2 #:color "red")
       (node))))

;; labeled rectangle

(define (label-box w h label)
  (define l-pict (text label))
  (rt-superimpose
   (rectangle w h)
   (cc-superimpose (rectangle (+ 15 (pict-width l-pict))
                              (+ 5 (pict-height l-pict)))
                   l-pict)))

;; chaperone pict
;; STILL VERY ROUGH, MAKE IT BETTER

(define (chaperone-pict [pict (blank 1 1)])
  (npict
   (node #:at (coord 0 0)
         #:pict (label-box 200 170 "chaperone"))
   (node #:at (coord 0 0)
         #:pict 
         (cc-superimpose
          (label-box 140 110 "original")
          pict))
   (node #:at (coord -50 -55) #:name "anchor1")
   (node #:at (coord -50 -85) #:name "anchor2")
   (node #:at (coord 50 -55) #:name "anchor3")
   (node #:at (coord 50 -85) #:name "anchor4")
   (node #:at (coord 80 -85) #:name "anchor5")
   (node #:at (coord 80 -115) #:name "anchor6")
   (node #:at (coord -80 -85) #:name "anchor7")
   (node #:at (coord -80 -115) #:name "anchor8")
   (line #:from "anchor2" #:to "anchor1" #:arrow? #t)
   (line #:from "anchor3" #:to "anchor4" #:arrow? #t)
   (line #:from "anchor5" #:to "anchor6" #:arrow? #t)
   (line #:from "anchor8" #:to "anchor7" #:arrow? #t)
   (node #:at (coord -80 -120 'ct)
         #:pict (text "x"))
   (node #:at (coord 80 -120 'ct)
         #:pict (text "(chaperone-of? x)  "))
   ))

;; chaperone, prompt-tag/c pict
(define (chart-box text)
  (cc-superimpose
   (rounded-rectangle/border 
    145 50
    #:color "whitesmoke"
    #:border-color "darkgray")
   (t/inc text 15)))

(define prompt-chap-pict
  (npict
   (node #:name "ctc" #:at (coord 0 0)
         #:pict (chart-box "prompt-tag/c"))
   (node #:at (coord 10 -50 'lc)
         #:pict (hc-append (t/inc "generates" 15) (blank 10 1)))
   (node #:name "chap" #:at (coord 0 -100)
         #:pict (chart-box "prompt chaperone"))
   (node #:at (coord 10 -150 'lc)
         #:pict (hc-append (t/inc "proxies" 15) (blank 10 1)))
   (node #:name "tag" #:at (coord 0 -200)
         #:pict (chart-box "prompt tag"))
   (line #:arrow? #t #:from "ctc" #:to "chap"
         #:start-align 'cb #:end-align 'ct)
   (line #:arrow? #t #:from "chap" #:to "tag"
         #:start-align 'cb #:end-align 'ct)
   ))

;; operations pict function
(define (ops-pict stage)
  (define s-width 85)
  (cond [(eq? stage 'init) (blank 1 1)]
        [(eq? stage 'prompt)
         (scale 
          (make-stack '("% ☆ h") s-width)
          4)]
        [(eq? stage 'p2)
         (scale (make-stack '("% ☆ h" "...") s-width) 4)]
        [(eq? stage 'p3)
         (scale (make-stack '("% ☆ h" "..." "...") s-width) 4)]
        [(eq? stage 'p4)
         (scale (make-stack '("% ☆ h" "..." "..." "...") s-width) 4)]
        [(eq? stage 'abort)
         (define-values (pict subs)
           (make-stack #:mv #t '("% ☆ h" "..." "..." "..." "abort ☆ 5") s-width))
         (scale
          (npict 
           (node #:pict pict)
           (line #:from (last subs) #:to (first subs)
                 #:start-align 'lc #:end-align 'lc
                 #:start-angle pi #:end-angle 0
                 #:arrow? #t))
          4)]
        [(eq? stage 'abort2)
         (scale (make-stack '("(h 5)") s-width) 4)]
        [(eq? stage 'call/comp)
         (scale (make-stack '("% ■ h" "..." "..." "..." "call/comp ■ f") s-width) 4)]
        [(eq? stage 'call/comp2)
         (define-values (s picts)
           (make-stack #:mv #t '("% ■ h" "..." "..." "..." "call/comp ■ f") s-width))
         (scale (ht-append 
                 (vc-append (blank 1 (pict-height (car picts)))
                            (hc-append
                             (vc-append (t/inc "k" 10) (blank 1 3))
                             (curly 15 (* (pict-height (car picts)) 3))))
                 s) 
                4)]
        [(eq? stage 'call/comp3)
         (define-values (s picts)
           (make-stack #:mv #t '("% ■ h" "..." "..." "..." "(f k)") s-width))
         (scale (ht-append 
                 (vc-append (blank 1 (pict-height (car picts)))
                            (hc-append
                             (vc-append (t/inc "k" 10) (blank 1 3))
                             (curly 15 (* (pict-height (car picts)) 3))))
                 s)
                4)]
        [(eq? stage 'invoke)
         (define-values (s picts)
           (make-stack #:mv #t '("% ■ h" "..." "..." "..." "(k 0)") s-width))
         (scale (ht-append 
                 (vc-append (blank 1 (pict-height (car picts)))
                            (hc-append
                             (vc-append (t/inc "k" 10) (blank 1 3))
                             (curly 15 (* (pict-height (car picts)) 3))))
                 s)
                4)]
        [(eq? stage 'invoke2)
         (define-values (s picts)
           (make-stack #:mv #t '("% ■ h" "..." "..." "..." "..." "..." "..." "0") 
                       s-width))
         (define kb (hc-append
                     (vc-append (t/inc "k" 10) (blank 1 3))
                     (curly 15 (* (pict-height (car picts)) 3))))
         (scale (ht-append 
                 (vc-append (blank 1 (pict-height (car picts)))
                            kb kb)
                 s)
                4)]
        [(eq? stage 'wcm)
         (define-values (s picts)
           (make-stack #:mv #t
                       '("wcm ■ v" "..." "..." "..." "ccm ■")
                       s-width))
         (define b (cmark-box))
         (scale (npict
                 (node #:at (coord -2.5 (- (/ (pict-height (car picts)) 2)))
                       #:pict (colorize (filled-rectangle 5 3)
                                        "darkgray"))
                 (node #:at (coord 0 0 'lt) #:pict s)
                 (node #:at (coord -5 (- (/ (pict-height (car picts)) 2))
                                   'rc)
                       #:pict b)
                 (line #:from right-disk #:to (last picts)
                       #:start-align 'cb #:end-align 'lc
                       #:start-angle (* 3 (/ pi 2)) #:end-angle 0
                       #:arrow? #t))
                4)]
        [else (blank 1 1)]))

(define (component-box [pict (blank 1 1)]
                       [width 100]
                       [height 100]
                       #:color [color "whitesmoke"]
                       #:border-color [border-color "darkgray"])
  (cc-superimpose
   (rectangle/border width height
                     #:color color
                     #:border-color border-color)
   pict))

;; flow through component C
(define (component-flow-pict stage n)
  (define ho-pict (ho-wrapper (t/inc "f" size4) `((,party-color-1 . ,party-border-1))))
  (define ho-pict-2 (ho-wrapper (t/inc "f" size4)
                                `((,party-color-1 . ,party-border-1)
                                  ("whitesmoke" . "darkgray"))))
  (npict
   ;; dummy notes
   (node #:at (coord -350 0)) (node #:at (coord 350 0))
   ;; boundaries
   (node #:at (coord -100 0) #:pict (linestyle 'short-dash (vline 1 80)))
   (node #:at (coord 100 0) #:pict (linestyle 'short-dash (vline 1 80)))
   ;; contract
   (node #:at (coord -100 -55) #:pict (t/inc "ctc" 25))
   (node #:at (coord 100 -55) #:pict (t/inc "ctc" 25))
   ;; boxes
   (node #:name "B" #:at (coord -200 0)
         #:pict (component-box (t/inc "B" size3)
                               #:color party-color-1
                               #:border-color party-border-1))
   (node #:name "C" #:at (coord 0 0)
         #:pict (component-box (t/inc "C" size3)))
   (node #:name "A" #:at (coord 200 0)
         #:pict (component-box (t/inc "A" size3)
                               #:color party-color-2
                               #:border-color party-border-2))
   (line #:from "B" #:to "C"
         #:start-align 'rc #:end-align 'lc
         #:arrow? #t)
   (line #:from "C" #:to "A"
         #:start-align 'rc #:end-align 'lc
         #:arrow? #t)
   (node #:name "f" 
         #:at (cond [(eq? stage 'init) (coord -200 65)]
                    [(eq? stage '1st) (coord (+ -200 (* n 100)) 65)]
                    [(eq? stage 'wrap) (coord -100 65)]
                    [(eq? stage '2nd) (coord (+ -100 (* n 200)) 65)]
                    [(eq? stage 'wrap2) (coord 100 65)]
                    [(eq? stage '3rd) (coord (+ 100 (* n 100)) 65)]
                    [(eq? stage 'done) (coord 200 65)])
         #:pict (cond [(eq? stage 'done) 
                       (cc-superimpose (ghost ho-pict-2) (blame-pict "B"))]
                      [(or (eq? stage 'wrap) (eq? stage '2nd))
                       (cc-superimpose
                        (ghost ho-pict-2)
                        ho-pict)]
                      [(or (eq? stage 'wrap2) (eq? stage '3rd)) ho-pict-2]                       
                      [else (cc-superimpose
                             (ghost ho-pict-2)
                             (t/inc "f" size4))]
                      ))))

;; second component flow pict (earlier in talk though)
(define (component-flow-pict-2 stage n)
  (define ho-pict (ho-wrapper (t/inc "f" size4) `((,party-color-1 . ,party-border-1))))
  (npict
   ;; dummy notes
   (node #:at (coord -250 0)) (node #:at (coord 250 0))
   ;; boundaries
   (node #:at (coord 0 0) #:pict (linestyle 'short-dash (vline 1 80)))
   ;; contract
   (node #:at (coord 0 -55) #:pict (t/inc "ctc" 25))
   ;; boxes
   (node #:name "B" #:at (coord -100 0)
         #:pict (component-box (t/inc "B" size3)
                               #:color party-color-1
                               #:border-color party-border-1))
   (node #:name "A" #:at (coord 100 0)
         #:pict (component-box (t/inc "A" size3)
                               #:color party-color-2
                               #:border-color party-border-2))
   (line #:from "B" #:to "A"
         #:start-align 'rc #:end-align 'lc
         #:arrow? #t)
   (node #:name "f"
         #:at (cond [(eq? stage 'init) (coord -100 65)]
                    [(eq? stage '1st) (coord (+ -100 (* n 100)) 65)]
                    [(eq? stage 'wrap) (coord 0 65)]
                    [(eq? stage '2nd) (coord (+ 0 (* n 100)) 65)]
                    [(eq? stage 'done) (coord 100 65)])
         #:pict (cond [(eq? stage 'done)
                       (cc-superimpose (ghost ho-pict) (blame-pict "A"))]
                      [(or (eq? stage 'wrap) (eq? stage '2nd)) ho-pict]
                      [else (cc-superimpose
                             (ghost ho-pict)
                             (t/inc "f" size4))]))))

;; gradual typing picts

(define (gt-intro-pict)
  (npict
   (node #:name "typed" #:at (coord 0 150)
         #:pict (component-box (t/cant "typed") 150 100
                               #:color typed-color
                               #:border-color (darker typed-color 20)))
   (node #:at (coord 0 0)
         #:pict (linestyle 'short-dash (hline 300 1)))
   (node #:name "untyped" #:at (coord 0 -150)
         #:pict (component-box (t/cant "untyped") 150 100
                               #:color untyped-color
                               #:border-color (darker untyped-color 20)))
   (node #:name "t1" #:at (coord 30 100))
   (node #:name "t2" #:at (coord -30 100))
   (node #:name "u1" #:at (coord 30 -100))
   (node #:name "u2" #:at (coord -30 -100))
   ;; contract bubbles
   (node #:name "c2" #:at (coord -65 0) #:pict ctc-circle)
   (node #:name "c1" #:at (coord 65 0) #:pict ctc-circle)
   ;; label
   (node #:name "label"
         #:at (coord 80 30 'lc) #:pict (t/cant "contract check" 20))
   (line #:from "label" #:to "c1"
         #:start-align 'lb)
   ;; lines
   (line #:from "t1" #:to "u1"
         #:start-align 'rb #:end-align 'rt
         #:start-angle (- (/ pi 6)) #:end-angle (+ pi (/ pi 6))
         #:arrow? #t)
   (line #:from "u2" #:to "t2"
         #:start-align 'lt #:end-align 'lb
         #:start-angle (- pi (/ pi 6)) #:end-angle (/ pi 6)
         #:arrow? #t)))

;; more gradual typing
(define (gt-pict-1 #:blame? [blame? #f])
  (npict
   (node #:at (coord 120 -30)
         #:pict (if blame?
                    (blame-pict #:big? #t "untyped")
                    (blank 1 1)))
   ;; labels at the top
   (node #:at (coord 190 100 'rt)
         #:pict (backdrop (t/cant "typed" 15)
                          #:color typed-color))
   (node #:at (coord 190 -120 'rb)
         #:pict (backdrop (t/cant "untyped" 15)
                          #:color untyped-color))
   ;;
   (node #:name "above"
         #:at (coord 0 25 'cb)
         #:pict (make-stack
                 '("" "% ☆ h" "" "" "")
                 70
                 #:color typed-color
                 #:border-color typed-border))
   ;; prompt note
   (node #:name "prompt" #:at (coord 28 75 'lc))
   (node #:name "abort"  #:at (coord 32 -93 'lc))
   ;; contract migration
   (node #:at (coord -100 0) #:pict ctc-circle)
   (node #:name "tag-A" #:at (coord -100 50)
         #:pict (t/inc "☆" 25))
   (node #:name "tag-B" #:at (coord -100 -50)
         #:pict (t/inc "★" 25))
   (line #:from "tag-A" #:to "tag-B"
         #:start-align 'cb #:end-align 'ct
         #:arrow? #t)
   ;; prompt tag migration
   ;; abort path
   (line #:from "abort" #:to "prompt"
         #:start-align 'rc #:end-align 'rc
         #:start-angle (/ pi 5) #:end-angle (+ (/ pi 2) (/ pi 3))
         #:arrow? #t)
   (node #:name "boundary"
         #:at (coord 0 0)
         #:pict (linestyle 'short-dash (hline 400 1)))
   (node #:name "point"
         #:at (coord 0 0)
         #:pict (disk 10))
   (line #:from "above" #:to "below" #:arrow? #t
         #:start-align 'cb #:end-align 'ct)
   (node #:name "below"
         #:at (coord 0 -25 'ct)
         #:pict (make-stack
                 '("" "" "" "" "abort ★ \"a\"")
                 70
                 #:color untyped-color
                 #:border-color untyped-border))))

;; more gradual typing
(define (gt-pict-2 #:blame? [blame? #f])
  (npict
   (node #:at (coord 120 -30)
         #:pict (if blame?
                    (blame-pict #:big? #t "untyped")
                    (blank 1 1)))
   ;; labels at the top
   (node #:at (coord 190 100 'rt)
         #:pict (backdrop (t/cant "typed" 15)
                          #:color typed-color))
   (node #:at (coord 190 -120 'rb)
         #:pict (backdrop (t/cant "untyped" 15)
                          #:color untyped-color))
   ;;
   (node #:name "above"
         #:at (coord 0 25 'cb)
         #:pict (make-stack
                 '("" "wcm ☆ f" "" "" "")
                 70
                 #:color typed-color
                 #:border-color typed-border))
   ;; prompt note
   (node #:name "prompt" #:at (coord 28 75 'lc))
   (node #:name "abort"  #:at (coord 35 -93 'lc))
   ;; contract migration
   (node #:at (coord -100 0) #:pict ctc-circle)
   (node #:name "tag-A" #:at (coord -100 50)
         #:pict (t/inc "☆" 25))
   (node #:name "tag-B" #:at (coord -100 -50)
         #:pict (t/inc "★" 25))
   (line #:from "tag-A" #:to "tag-B"
         #:start-align 'cb #:end-align 'ct
         #:arrow? #t)
   ;; prompt tag migration
   ;; abort path
   (line #:from right-disk #:to "abort"
         #:start-align 'cb #:end-align 'rc
         #:start-angle (- (/ pi 2)) #:end-angle (+ pi (/ pi 6))
         #:arrow? #t)
   (node #:name "boundary"
         #:at (coord 0 0)
         #:pict (linestyle 'short-dash (hline 400 1)))
   (node #:name "point"
         #:at (coord 0 0)
         #:pict (disk 10))
   (line #:from "above" #:to "below" #:arrow? #t
         #:start-align 'cb #:end-align 'ct)
   ;; marks
   (node #:at (coord 37 77 'cc)
         #:pict (colorize (filled-rectangle 7 3) typed-border))
   (node #:name "mark" #:at (coord 37 78 'lc)
         #:pict (cmark-box #:store-color typed-color
                           #:store-border typed-border))
   (node #:name "below"
         #:at (coord 0 -25 'ct)
         #:pict (make-stack
                 '("" "" "" "" "(first \"5\")")
                 70
                 #:color untyped-color
                 #:border-color untyped-border))))

;; work flow pict
(require slideshow/face)
(provide work-flow-pict)

(define (work-flow-pict #:mean? [mean? #f])
  (npict
   (node #:name "docs"
         #:at (coord -180 0)
         #:pict (component-box
                 (cc-superimpose
                  (file-icon 70 80 "wheat" #t)
                  (t/quat "Docs" 18))
                 120 120))
   (node #:name "src"
         #:at (coord 0 0)
         #:pict (component-box
                 (desktop-machine 1 '(plt binary))
                 120 120))
   (node #:name "person"
         #:at (coord 180 0)
         #:pict (component-box
                 (scale (if mean?
                            (face 'unhappy)
                            (face 'sortof-happy)) 0.30)
                 120 120))
   (line #:from "docs" #:to "src"
         #:start-align 'rc #:end-align 'lc
         #:arrow? #t)
   (line #:from "src" #:to "person"
         #:start-align 'rc #:end-align 'lc
         #:arrow? #t)))

;; animations with pslide
(require syntax/parse/define syntax/parse)
(define-simple-macro (pslide/play kws ... [n:id] body ...)
  (void (play kws ... (λ (n) (ppict-do ((pslide-base-pict)) body ...)))))

(define-syntax-rule (pslide/play/staged [stage ...] [n] body ...)
  (staged [stage ...] (pslide/play [n] body ...)))
