#lang at-exp slideshow

(require slideshow/step slideshow/code slideshow/face 
         (only-in slideshow/slide title-size)
         (except-in "beamer.ss" title) "lib.ss" racket/gui  "thanks.ss" 
         "ts-intro.rkt"
         "tslide.ss" "config.ss"  unstable/gui/slideshow)

(title '("Research meets Application")
       '("Life on the EcmaScript Committee")
       '(("Sam Tobin-Hochstadt" ""))
       "PhD Seminar")
(set-page-numbers-visible! #f)
(do-start? #t)



(slide (scale (bitmap "fep.png") .8))

(tslide "How did I get here?")

(slide (bitmap "whiteboard.jpg"))

(slide (bitmap "blog.png"))

(slide (scale (bitmap "blog2.png") 1.5))

(tslide "Meanwhile ...")

(ts-intro)

(tslide "Meanwhile ...")

(slide (scale (bitmap "harmony.png") .8))

(slide (bitmap "ff.png"))

(tslide "TC39")

(slide (scale (bitmap "ecma.jpg") 2))
(slide (bitmap "google.jpg"))
(slide (bitmap "yahoo.jpg"))
(slide (bitmap "moz.jpg"))
(slide (bitmap "apple-logo.jpg"))
(slide (bitmap "microsoft_logo.jpg"))
(slide (scale (bitmap "js.png") .5))

(tslide "Why web standards?")

(slide (scale (bitmap "html-source.png") .8))
(slide (scale (bitmap "js-src.png") 0.9))

(slide (bitmap "w3.jpg"))

(slide (bitmap "HTML5-LOGO.jpg"))




(tslide "What we work on")

(slide (scale (bitmap "proposals.png") .8))

(slide (scale (bitmap "spec.png") .8))

(slide #:title "ES.next" #:layout 'center
       (colorize (item "Proxies") "black")
       'next
       (item "Generators")
       'next(item "Comprehensions")
       'next(colorize (item "Proper Tail Calls") "black")
       'next(colorize (item "Binary Data") "black")
       'next(colorize (item (code let)) "black")
       'next(item "Collections")
       'next(colorize (item "Private names") "black")
       'next(item "String interpolation")
       'next(item "Optional & default arguments"))



(tslide "What I work on")

(slide #:title "ES.next" #:layout 'center
       (colorize (item "Proxies") "red")
       (item "Generators")
       (item "Comprehensions")
       (colorize (item "Proper Tail Calls") "red")
       (colorize (item "Binary Data") "red")
       (colorize (item (red-code let)) "red")
       (item "Collections")
       (colorize (item "Private names") "red")
       (item "String interpolation")
       (item "Optional & default arguments"))

(define (s f [n 1] #:title [t #f]) (slide #:layout 'center #:title t (scale (bitmap f) n)))

(s "require-js-web.jpg" #:title "Modules")
(s "commonjs.png" #:title "Modules")
(s "nodejs.png" #:title "Modules")

(s "script-tag.png" #:title "Modules")



(tslide "So Dave and I created one")



(s "mod-syntax.png" 0.9 #:title "Modules")

(slide 
 (scale 
  (code |module m {|
        |    import Math.*;|
        ||
        |    let x = sin(17);|
        ||
        |    export x;|
        |}|)
  1.5))

(slide (scale (code |module $ = "http://jquery.com/jq.js"|) 1.4))

(slide (scale (code |module M {|
                    |    module N { ... };|
                    ||
                    |    let x = N.y;|
                    ||
                    |    export x;|
                    |}|)
              1.5))

(slide (scale (code |let csLoader = |
                    |    new Loader(csTranslate)|) 1.5))

(tslide "Minor details")

(s "goldbar.jpg")

(s "secline.jpg")

(s "fep.png" .8)

(s "cloud.jpg" 1.6)

(tslide "On having an impact")

(tslide "Thanks!")












