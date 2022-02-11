#lang slideshow

(require slideshow/repl unstable/gui/ppict unstable/gui/pslide
         "ts-intro.rkt" "stages.rkt"
         "helper.rkt" "tslide.rkt"
         slideshow/code "config.rkt" "lib.rkt")

(slide #:title "one")

(define pc-ns (make-base-namespace))

(parameterize ([current-namespace pc-ns])
  (eval '(require polyconf)))

(pslide/title "three"
              #:go (coord .5 .5 'cc)
        (repl-area
         #:make-namespace (lambda () pc-ns)
         #:height (* client-h .75)))
(slide #:title "two")
