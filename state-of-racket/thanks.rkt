#lang slideshow/widescreen

(require slideshow/play (except-in "beamer.ss" title) unstable/gui/slideshow "lib.ss" "config.ss"
         "config.ss" pict/shadow
         (except-in "beamer.ss" title) "lib.ss" racket/gui
         "tslide.ss" lang-slide/hudak-quote "contracts.rkt"
         "ts-intro.rkt" "stages.rkt"
         "helper.rkt")


(define names (list "Matthias Felleisen" "Ryan Culpepper" "Stevie Strickland"
                    "Ivan Gazeau" "Carl Eastlund" "Matthew Flatt"
                    "David Van Horn" "Phil Nguyen" "Tom Gilray"
                    "Asumu Takikawa" "Christos Dimoulas" "Tony Garnock-Jones"
                    "Vincent St-Amour" "Robby Findler" "Eli Barzilay"
                    "Shriram Krishnamurthi"
                    "Jay McCarthy" "Hari Prashanth"
                    "Carl Friedrich Bolz-Tiereck"
                    "Tobias Pape"
                    "Robert Hirschfeld"
                    ))

(define (red-t s) (colorize (t* s) "red"))
(define (t* s) (t/quat s size2))
(define iu-names (list
                  "Ambrose Bonnaire-Sargeant"
                  "Andrew Kent"
                  "Sarah Spall"
                  "Caner Derici"
                  "Rajan Walia"
                  "Chung-chieh Shan"
                  "Jeremy Siek"
                  "Ryan Newton"
                  "Michael Vollmer"
                  "Matteo Cimini"
                  "Peter Fogg"
                  "Omer Agacan"
                  "Amr Sabry"
                  "Cameron Swords"
                  "Earl Dean"
                  "Michael Vitousek"
                  "Paulette Koronkevich"
                  "Joshua Larkin"
                  "Spenser Bauman"))

(define l (sort (append (map t* names) (map red-t iu-names)) < #:key (lambda _ (random)) #:cache-keys? #t))
(define pic (apply vl-append (append (map launder l) l (map launder l))))
(define (name-pict n)
  (define count (max 0 (sub1 (ceiling (inexact->exact (* n (+ (length iu-names) (length names))))))))
  (define name (list-ref l count))
  (define rest (drop l count))
  (define pic* (refocus (cellophane pic .15) name))
  (inset (lc-superimpose name pic* (ghost (t* "Ambrose Bonnaire-Sargeant"))) -300 0 0 0))

(define (thanks)
  (play-n #:steps 1000 name-pict #:delay 0.01))
(provide thanks)

(module+ main (thanks))
