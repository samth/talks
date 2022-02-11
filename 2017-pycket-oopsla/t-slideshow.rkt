#lang typed/racket

(provide (all-defined-out))

(require unstable/gui/slideshow (only-in slideshow/code define-code) scheme/class)


(require/typed scheme/gui
               [color% Color%])

(require/typed slideshow
               [opaque Pict pict?]
               [text (String Style Integer -> Pict)]
               [colorize (Pict (U (Instance Color%) String) -> Pict)]
               [cc-superimpose (Pict Pict * -> Pict)]
               [ghost (Pict -> Pict)]
               [scale (Pict Number -> Pict)]
               [bitmap ((U Path String) -> Pict)]
               [para (String * -> Pict)]
               [subitem (String * -> Pict)]
               [blank (case-lambda (Integer -> Pict)
                                   (Integer Integer -> Pict))]
               [hc-append (Integer Pict * -> Pict)]
               [hbl-append (Integer Pict * -> Pict)]
               [vc-append (Integer Pict * -> Pict)]
               [slide (Pict * [#:title String] [#:layout (U 'center 'auto 'top)] -> Void)])

(require/typed slideshow/code
               [typeset-code ((Syntaxof Any) -> Pict)])

(require/typed "beamer.ss"
               [current-background-pict (Parameter Pict)]
               [plt-title-background-path Path]
               [current-title-font (Parameter Style)])

(require/typed "lib.ss"
               [title-text-size Integer]
               [large-text-size Integer])

(define-code code typeset-code)

(provide text current-title-font)
