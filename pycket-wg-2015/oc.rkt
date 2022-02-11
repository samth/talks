#lang slideshow


(require slideshow/step slideshow/code slideshow/face 
         slideshow/balloon
         unstable/gui/ppict unstable/gui/pslide
         (only-in slideshow/slide title-size)
          "config.ss"
         (except-in "beamer.ss" title) "lib.ss" racket/gui  "thanks.ss" 
         "tslide.ss" lang-slide "contracts.rkt"
         "ts-intro.rkt" "stages.rkt"
         "helper.rkt"
         racket/runtime-path (except-in mzlib/etc identity) unstable/gui/slideshow)


;; TODO abstract with above
(define (oc-highlight-in orig target #:color [color "red"] #:show [show? #t])
  (define-values (x y) (lt-find orig target))
  (pin-under orig x y
             (show (colorize (filled-rectangle (pict-width target)
                                               (pict-height target))
                             color)
                   show?)))
(define (multi-oc-highlight orig targets #:color [color "red"] #:show [show? #t])
  (for/fold ([res orig])
      ([t targets])
    (oc-highlight-in res t #:color color #:show show?)))

(define (strikeout pict src #:line-width [line-width 5] #:x? [x? #f])
  ((if x?
       (lambda (p) (pin-line p src rt-find src lb-find
                             #:line-width line-width #:color "red"))
       values)
   (pin-line pict src lt-find src rb-find
             #:line-width line-width #:color "red")))


(define prng-recommendation.png (bitmap "prng-recommendation.png"))
(define prng-popup2.png (bitmap "prng-popup2.png"))

(define prng-success-max (code max))
(define prng-success-min (code min))
(define prng-success-code
  (code (- #,prng-success-max #,prng-success-min)))
(define prng-last-code (code last))
(define prng-inner-near-miss-code
  (code (* #,prng-success-code #,prng-last-code)))
(define prng-IM-code (code IM))
(define prng-outer-near-miss-code
  (code (/ #,prng-inner-near-miss-code #,prng-IM-code)))
(define prng-near-miss-code
  (code (+ #,prng-outer-near-miss-code min)))
(define prng-failure-last-code (code last))
(define prng-failure-IA-code   (code IA))
(define prng-failure-code
  (code (* #,prng-failure-last-code #,prng-failure-IA-code)))

(define prng-code
  (vl-append ;(code |#lang typed/racket/base|) (code ||)
  (code
(define IM   139968)
(define IA     3877)
(define IC    29573)
code:blank
(define last 42)
(define min  35.3)
(define max  156.8)
(define (gen-random)
  (set! last (modulo (+ #,prng-failure-code IC) IM))
  #,prng-near-miss-code)
)))


(define prng-fixed-code
  (vl-append ;(code |#lang typed/racket/base|) (code ||)
  (code
(define IM   139968)
(define IA     3877)
(define IC    29573)
code:blank
(define last 42)
(define min  35.3)
(define max  156.8)
(define (gen-random)
  (set! last (modulo (+ #,prng-failure-code IC) IM))
  (+ (/ (* (- max min) (fx->fl last)) (fx->fl IM)) min))
)))

(define-syntax-rule (code/types whole color
                                [left  left-type]
                                [right right-type])
  (let ([code-ex (big (oc-highlight-in whole whole #:color color))])
    (define-values (left-x  left-y)  (ct-find whole left))
    (define-values (right-x right-y) (ct-find whole right))
    (define left-balloon
      (wrap-balloon (small (code left-type))  'se 10 10 "white" 12))
    (define right-balloon
      (wrap-balloon (small (code right-type)) 'sw 10 10 "white" 12))
    (pin-balloon left-balloon
                 (pin-balloon right-balloon code-ex right-x right-y)
                 left-x left-y)))
(define prng-success-code/types
  (code/types prng-success-code (light "green")
              [prng-success-max Float] [prng-success-min Float]))
(define (prng-inner-near-miss-code/types [color (light "red")])
  (code/types prng-inner-near-miss-code color
              [prng-success-code Float]
              [prng-last-code    Integer]))
(define (prng-outer-near-miss-code/types [color (light "red")])
  (code/types prng-outer-near-miss-code color
              ;; TODO not float, so opt. prox would discard it...
              ;;  figure out exactly what to do with this
              [prng-inner-near-miss-code Real]
              [prng-IM-code              Integer]))
(define (prng-merged-near-misses-code/types [color (light "red")])
  (code/types prng-outer-near-miss-code color
              [prng-last-code Integer]
              [prng-IM-code   Integer]))
(define prng-failure-code/types
  (code/types prng-failure-code "white" ; no highlight
              [prng-failure-last-code Integer]
              [prng-failure-IA-code   Integer]))

(define rewrite-fl-
  (frame
   (let ()
     (define minus (code -))
     (define old (code (#,minus <Float> <Float>)))
     (inset (vl-append 20 (code fl-) (strikeout old minus)) 25))))

(define (do-prng)


;; TALK:
;;     Now, we'll go through a complete example,
;;     following the entire pipeline with an actual near miss
;;     - prng, written in typed/racket
;;     - typed racket performs various type-driven optimizations
;;     - these type-driven opts are one of the optimimzations handled
;;       by our prototype, the other being inlining, as in the intro
;;  NEXT
;;     - for instance, it can specialize generic arith to type-specialized arith
;;  NEXT
;;     - BUT, this optimization is only valid when both arguments are floats
;;     - so in cases where mixed-mode (int and float) arith is used, the opt
;;       doesn't apply
;;     - our prototype optimization coach reports this
;;     - so now, let's see how we go from *this* program to *this* report
(slide/staged
 [code-only popup after]
 (ppict-do ((pslide-base-pict))
            #:go (coord 0.05 0.05 'lc)
           (title-t "Developer Tools")
           #:go (coord 0.0 1/5 'lt)
           (cond [(= stage code-only)
                  prng-code]
                 [(= stage after)
                  prng-fixed-code]
                 [(= stage popup)
                  (oc-highlight-in prng-code prng-near-miss-code
                                   #:color (light "red"))])
           #:go (coord 2/3 0.48 'cc)
           (show (scale prng-popup2.png 1.2)
                 (= stage popup))
           #:go (coord 0.9 0.4 'rt)
           (show (shadow-frame (t "3x speedup")
                               #:shadow-descent 12)
                 (= stage after))))
)

(module+ main (do-prng))

(provide do-prng)
