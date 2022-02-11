#lang at-exp slideshow

(require slideshow/step slideshow/code slideshow/face 
         (only-in slideshow/slide title-size)
         (except-in "beamer.ss" title) "lib.ss" racket/gui 
         "tslide.ss" "config.ss" "langs/main.rkt"
         "ts-intro.rkt" "stages.rkt" unstable/gui/slideshow)

(tslide "Bit Syntax")

(slide #:title "Construct + Parse"
       (vl-append 30
        (code (bit-string-case expr
                ([pattern ...] (when expr) expr ...)
                ([pattern ...] expr ...)
                (else expr ...)))
       (blank 100)
       
       (code (bit-string piece-expr ...))))

(slide #:title "SRV Bug"
       (t "???"))

(slide #:title "Advantages"
       (item "No buffer overflows")       
       (item "Enforce length/body correlation")       
       (item "Dispatch on type tags")            
       (item "Same notation for marshalling and unmarshalling")
       (blank 30)
       (item "Used for DNS, SSH, DHCP")
       (blank 50)
       (tt "https://github.com/tonyg/bitsyntax"))