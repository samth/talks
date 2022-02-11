#lang slideshow

(require slideshow/step slideshow/code slideshow/face (only-in slideshow/slide title-size)
         racket/gui racket/require
         (except-in
          (path-up "icfp2010/beamer.ss") title)
         (path-up "icfp2010/lib.ss") 
         (path-up "icfp2010/tslide.ss")
         (path-up "config.ss")
         (prefix-in 2: 2htdp/image)
         racket/runtime-path mzlib/etc  unstable/gui/slideshow )

(define ff (bitmap "firefox.png"))
(define ff-blank (colorize (filled-rectangle (* .5 (pict-width ff)) (pict-height ff)) "white"))

(define (js t) (text t " Inconsolata" (+ 4 (current-font-size))))

#;(current-title-background-pict (blank 1024 768))

(title '("Unique Names")
       '()
       '(("Sam & Dave" ""))
       "TC39 September 2010")
(set-page-numbers-visible! #t)
(do-start? #t)

(slide 
 #:title "Unique Names"
 (para "A new atomic type of data which can be used directly as a property key")
 (blank 50)
 (para "Applications:")
 (subitem "Private methods & fields")
 (subitem "Collision-free extension points"))

(slide/staged 
 [one two]
 #:title "Private methods"
 (vl-append
  (js "function F(){")
  (js "  let n = new Name();")
  (js "  this[n] = function (x) { .... } ;")
  (js "  return this;")
  (js "}")
    (js "")
    (js "let f = new F();")
  (show 
   (vl-append
    (hbl-append 10 (js "f.n;") (colorize (js "// unbound") "red")))
   (at two))))

(slide/staged
 [one two]
 #:title "Extension without Collision"
 (vl-append (js "module jQuery {")
            (js "  export var serialize = new Name();")
            (js "  export function sendAsXML(x) {")
            (js "    ... x[serialize]() ...")
            (js "  }")
            (js "}")
            (js "")
 (show
 (vl-append (js "module YUI {")
            (js "  export var serialize = new Name();")
            (js "  export function storeInCookie(x) {")
            (js "    ... x[serialize]() ...")
            (js "  }")
            (js "}"))
  (at two))))

(slide 
 #:title "Semantics"
 (para "Names are not converted to strings on property lookup")
 (para "Names are never enumerated")
 ;; FIXME - do we need to do this?
 (para "Names are not produced by " (parameterize ([current-font-size (- (current-font-size) 4)])(js "Object.getOwnPropertyNames")  ) "," (parameterize ([current-font-size (- (current-font-size) 4)])(js "Object.keys")  ) "etc") 
 (para "Name objects are frozen on creation"))

(slide 
 #:title (js "private")
 (para(vl-append
       (js "function F() {")
       (js "  private n;")
       (js "  this.n = ...;")
       (js "}")))
  (para "Convenient use of Names API")
  (para "Natural integration of OO styles")
  (para "Integrates well with extended object literal proposal"))

