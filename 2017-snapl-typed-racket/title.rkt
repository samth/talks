#lang racket/gui
(require slideshow "title-lib.rkt" "util.rkt")

(provide title-slide)
(define (title-slide)
  
  (define (lighten c)
    (let ([f (λ (x) (- 255 (quotient (- 255 x) 2)))])
      (make-object color%
        (f (send c red))
        (f (send c green))
        (f (send c blue)))))
  
  (define title-text-color (send the-color-database find-color "black"))
  (define my-red-color (make-object color% 242 183 183))
  (define my-blue-color (make-object color% 183 202 242))
  (define my-background-color (make-object color% 209 220 248))
  (define my-lambda-color (send the-color-database find-color "white"))
  (define plt-pen-color "black")
  (define plt-pen-style 'transparent)

    (define lighter-lambda-color (lighten my-lambda-color))
  (define lighter-red-color (lighten my-red-color))
  (define lighter-blue-color (lighten my-blue-color))
  (define lighter-background-color (lighten my-background-color))

;  (define old-margin margin)
  (set-margin! 0)
  (define plt-title-background/colors 
    (make-plt-title-background my-red-color
                               my-blue-color
                               my-background-color
                               my-lambda-color
                               plt-pen-color
                               plt-pen-style
                               #:clip? #t))
  (define title
    (inset (scale (let ([l1 (inset (t "Typed Racket") 10 0 10 0)]
                        [l2 (scale (bt " years of") 2)])
                    (hc-append
                     -4
                      (scale (bt "10") 3) 
                     (vc-append 
                      -10
                      
                      (scale l2 (/ (pict-width l1) (pict-width l2)))
                      l1)))
                  1.8)
           50 0))
  
  (define (above/top-big p1 p2)
    (vc-append (scale p1 (/ (pict-width p2) (pict-width p1)))
               p2))
  
  (define all-title-words 
    (vc-append
     title
     (blank 0 100)
     (colorize
      (scale (vc-append
              (scale (vc-append
                      (t "Sam Tobin-Hochstadt"))
                     1)
              (t "Indiana University"))
             1.2)
      "black")))
  
  (define placed
    (cc-superimpose (ghost plt-title-background/colors)
                    all-title-words))
  
  (define lighter-version 
    (let-values ([(l t) (lt-find placed title)]
                 [(r b) (rb-find placed title)])
      (inset/clip (make-plt-title-background
                   lighter-red-color
                   lighter-blue-color
                   lighter-background-color
                   lighter-lambda-color
                   plt-pen-color 
                   plt-pen-style)
                  (- l)
                  (- t)
                  (- r (pict-width plt-title-background/colors))
                  (- b (pict-height plt-title-background/colors)))))

      (slide
       (cc-superimpose
        plt-title-background/colors
        (pin-under
     placed 
     title
     lt-find
     lighter-version))))

(module+ main (title-slide))
