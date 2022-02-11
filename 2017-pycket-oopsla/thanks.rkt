#lang slideshow

(require slideshow/play (except-in "beamer.ss" title) unstable/gui/slideshow "lib.ss" "config.ss")

(define names (list "Matthias Felleisen" "Ryan Culpepper" "Stevie Strickland" "Ivan Gazeau" "Carl Eastlund" "Matthew Flatt"
                    "Felix Klock" "Jesse Tov" "Aaron Turon" "David Van Horn"
                    "Vincent St-Amour" "Robby Findler" "Eli Barzilay" "Shriram Krishnamurthi"
                    "Jay McCarthy" "Neil Toronto" "Hari Prashanth"
                    "Students at Northeastern and Brown"
                    "And everyone who has tried Typed Racket!"))

(define (name-pict n)
  (lt-superimpose 
   (ghost 
    (apply vl-append
           (sort (map t names) < #:key pict-width)))
   (apply vl-append
           (take (sort (map t names) < #:key pict-width) 
                 (inexact->exact (* n (length names)))))))

(define (thanks)
  (parameterize ([current-background-pict (bitmap plt-title-background-path)])
    (slide
     (text "Thanks!" (current-title-font) title-text-size)
     (blank 60)
     (text "Available from" (current-title-font) (current-font-size))
     (text "racket-lang.org" `(bold . ," Inconsolata") (current-font-size))
     (blank 20)
     (text "Supported by the Mozilla Foundation" (current-title-font) (current-font-size))) ))

(provide thanks)
