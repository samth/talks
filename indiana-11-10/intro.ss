#lang typed/racket/no-check

(require unstable/gui/slideshow slideshow)

(provide growth-of-scripting)

(: growth-of-scripting (-> Void))
(define (growth-of-scripting)
  (staged [a b c]
          (slide 
           #:title "The PL Renaissance"
           (case stage-name
             [(a) (scale (bitmap "Sanzio_01.png") .85)]
             [else
              (vc-append 
               20
               (hc-append 20
                          (scale (bitmap "rhino-alpha.png") .6)
                          (bitmap "perl-logo.jpg")
                          (scale (bitmap "python-logo.png") .5)
                          (bitmap "Clojure-logo.png")
                          )
               (hc-append 35
                            (bitmap "ruby-400.png")                          
                            (bitmap "tcl.png")
                            (bitmap "php.gif")
                            (bitmap "lua.png")
                            ((case
                              stage-name
                               [(a b) ghost]
                               [else (inst values Pict)])
                             (bitmap "plt-logo-red-diffuse.png"))))])))
  
  (slide #:title "What's good"
         (para "These languages are")
         
         (subitem "interactive")
         (subitem "designed for rapid development")
         (subitem "supported by an active community")
         (subitem "modular")
         (subitem "higher-order")
         (blank 10)
         ;'next
         (para "And they're exciting!")))
