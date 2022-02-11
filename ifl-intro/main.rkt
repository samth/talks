#lang racket
(require slideshow unstable/gui/pslide unstable/gui/ppict unstable/gui/slideshow)

(define logo (bitmap "1000px-Northeastern-seal.svg.png"))

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

(pslide
 #:go (coord 0.7 0.5)
 (cellophane (scale logo .9) .15)
 #:go (coord .5 .5)
 (t/kau "Welcome to IFL 2014!"  100))

(pslide 
 #:go (coord 0.1 0.1 'lc)
 (t/quat "WiFi:" size1)
 #:go (coord 0.9 0.1 'rc)
 (t/inc "NUWave-guest" size2)
 #:go (coord 0.9 0.2 'rc)
 (t/cant "Password in your nametag" size2)
 
 #:go (coord 0.1 0.4 'lb)
 (t/quat "Lunch:" size1)
 #:go (coord 0.9 0.4 'rb)
 (t/cant "In this room" size2)
 
 #:go (coord 0.1 0.6 'lb)
 (t/quat "Tomorrow:" size1)
 #:go (coord 0.9 0.67 'rb)
 (t/cant "IFL is in Egan Research Center" size2)
 
 #:go (coord 0.1 0.85 'lb)
 (t/quat "Hashtag:" size1)
 #:go (coord 0.9 0.85 'rb)
 (t/inc "#ifl2014" size2))

(slide #:layout 'center (t/quat "31 papers" 60))

(slide #:layout 'center (t/quat "48 participants" 60))

(slide #:layout 'center (t/quat "8 countries" 60))

(slide #:layout 'center (t/quat "1 invited talk" 60))

(slide #:layout 'center (t/kau "Thanks to ..." 80))

(pslide
 #:go (coord .5 .5)
 (scale (bitmap "profile-vest.jpg") 2)
 (t/quat "Asumu Takikawa" 80)
 )

(pslide
 #:go (coord .1 .02 'lt)
 (t/quat "Doreen Hodgkin," size2)
 (t/quat "Nicole Bekerian & Andy Fong" size2)
 (t/quat "... and Northeastern CCIS" size2)
 (blank 40) #:next
 (t/quat "Matthias Felleisen" size2)
 (blank 40) #:next
 (t/quat "Rinus Plasmeijer" size2)
 (t/quat "Juriaan Hage" size2)
 (t/quat "... and the IFL PC" size2)
 (blank 40) #:next
 (t/quat "The student volunteers" size2)
 (blank 40) #:next
 (t/quat "SIGPLAN & ACM" size2))

