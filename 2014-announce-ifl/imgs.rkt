#lang slideshow

(require unstable/gui/slideshow unstable/gui/ppict unstable/gui/pslide
         racket/draw images/compile-time (for-syntax racket/draw racket/class))

(define-syntax-rule (bmp e) (bitmap (compiled-bitmap (make-object bitmap% e))))

(define b (bmp "boston1.jpg"))
(define b2 (bmp "boston2.jpg"))
(define b3 (bmp "boston3.jpg"))
(define b4 (bmp "boston4.jpg"))
(define nu-logo (bmp "nu-logo.png"))
(define iu-logo (bmp "IU.jpg"))
(define wvh (bmp "wvh.jpg"))
(define neu (bmp "northeastern1.jpg"))

(provide (all-defined-out))