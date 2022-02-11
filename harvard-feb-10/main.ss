#lang slideshow

(require slideshow/step slideshow/code slideshow/face (only-in slideshow/slide title-size)
         (except-in "beamer.ss" title) "lib.ss" scheme/gui "class-slide.ss" "thanks.ss" "ts-intro.ss"
         "contracts.ss" "intro.ss" "tslide.ss" "config.ss" "occur-seq.ss"
         scheme/runtime-path mzlib/etc (planet cce/scheme:6/slideshow))

(title '("Typed Scheme")
       '("From Scripts to Programs")
       '(("Sam Tobin-Hochstadt" "PLT @ Northeastern University"))
       "Feb 3, 2010    Harvard University")
(set-page-numbers-visible! #t)
(do-start? #f)



;OUTLINE:

;Motivation
; - Growth of Scripting Languages

(growth-of-scripting)

; - Problems of Scripting Languages
; - Types as maintenence tool

(class-slide)


;Thesis
; Module-by-module porting of code from an untyped language to a
; typed sister language allows for an easy transition from untyped
; scripts to typed programs.

; - What does this mean: 
;  * Module-by-module
;  * Typed sister language
;  * Easy transition

;; why Scheme?


(let* ([p1 (subtitle-pict "Modules")]
       [p2 (subtitle-pict "Contracts")]
       [p3 (subtitle-pict "Abstractions")]
       [gp3 (ghost p3)])
  (slide #:title "Why PLT Scheme?"
         (hc-append 50 (rc-superimpose p1 gp3) gp3 gp3)
         (hc-append 50 gp3 (cc-superimpose p2 gp3) gp3)
         (hc-append 50 gp3 gp3 p3)))


;; Intro to TS

(ts-intro)

; - Technique for interlanguage interop
;  * With proof of soundness/blame thm


; - Type system for Scheme
;  * Occurrence Typing
;  * Variable-arity Polymorphism
;  * Refinement Types
;  * Local type inference


 


;Details of Support #1: Interlanguage Interop

(multi-sound)

; - The problem
;  * Untyped code can violate type invariants
;  * Two directions
;  * Higher-order
; - The solution
;  * Contracts
;  * Blame
;  * Boundaries/Interfaces
; - The theorem
;  * Intuition
;  * Formal Statement
;  * Why should you care?

;Details of Support #2: Occurrence Typing

(tslide "Types for Scheme"
        (list (subtitle-pict "Occurrence Typing") "Variable-Arity")
        (list (ghost (subtitle-pict "Occurrence Typing")) "Refinement Types")
        (list (ghost (subtitle-pict "Occurrence Typing")) "Ad-Hoc Data"))

(start)

(define forall (lift-above-baseline (code ∀) -3))

(slide/stage 
 [occur  refine union varar #;local]
 #:title "Types for Scheme"
 (tmod #:name (symbol->string stage-name)
  (pict-case stage-name
   [(occur)
    (code (: f (Any -> Number))
          (define (f x)
            (if (number? x)
                (add1 x)
                0)))]
   [(varar) 
    (code
     (: wrap (#,forall (B A ...)
               ((A ... -> B) -> (A ... -> B))))
     (define (wrap f)
       (lambda args
         (printf "args are: ~a\n" args)
         (apply f args))))]
   [(refine)
    (code
     (: check (String -> (Refinement sql-safe?)))
     (define (check s)
       (if (sql-safe? s)
           s
           (error "unsafe string!"))))]
   [(local) 
    (code
     (define x 1)
     (define y 2)
     (map - (list x y)))]
   [(union)
    (code
     (define-type-alias BT 
       (U Number (Pair BT BT)))
     (: sizeof (BT -> Number))
     (define (sizeof b)
       (if (number? b)
           1
           (+ 1 (sizeof (car b)) (sizeof (cdr b))))))])))

(parameterize ([current-keyword-list (cons "+" (current-keyword-list))])
  (occur-seq))

(slide #:title "All Together Now"
       (tmod
        (code
         (: f ((U Number String) (Pair Any Any) -> Number))
         (define (f input extra)
           (cond
             [(and (number? input) (number? (car extra)))
              (+ input (car extra))]
             [(number? (car extra))
              (+ (string-length input) (car extra))]
             [else 0])))))

; - Importance of type system for Scheme
; - Example Scheme idioms (number?, member, else, sexpr)
; - Supporting these idioms:
;  * True Unions
;  * Explicit recursive types
;  * Occurrence typing
; - number? or string? example
;  * Show both true and false filters, and objects

;Details of Support #3: Typed Scheme Software + Validation





;Support of Thesis
; - Full-scale implementation
;  * Integrated into PLT Scheme
;  * Implemented using PLT macros
;  * Numerous special-purpose features

(tslide "Implementation"
        (list "Macros" "Optimization"))

(define (big-mod opt)
  (code
   (provide (rename-out
             [module-begin #%module-begin]))
   (define-syntax (module-begin stx)
     (set! typed-context? #t)
     (let ([expanded
            (local-expand stx 'module-begin null)])
       (typecheck expanded)
       #,(if (not opt) (code expanded) (code (#,(red-code optimize) expanded)))))))

(slide/stage [mac opt]
             #:title (case stage-name
                       [(mac) "Macros"] [(opt) "Optimization"])
             (big-mod (eq? stage-name 'opt)))

(slide/stage 
 [one two]
 #:title "Optimization"
 (tmod
  (pict-case 
   stage-name
   [(one) (code (: norm (Float Float -> Float))
                (define (norm x y)
                  (flsqrt (+ (* x x) (* y y)))))]
   [(two) (code (: norm (Float Float -> Float))
                (define (norm x y)
                  (unsafe-flsqrt 
                   (unsafe-fl+ (unsafe-fl* x x) (unsafe-fl* y y)))))])))




(tslide "Results"
        (list "Integration" "Validation"))

(slide/stage 
 [integrate impl]
 #:title "Integrated with PLT" #:layout 'center
 (case stage-name
   [(integrate)
    (bitmap "large-letters.png")]
   [(impl)
    (tmod #:name "tslide"
     (code (: subtitle-pict : (String -> Pict))
           (define (subtitle-pict s)
             (text s (current-title-font) large-text-size))))]))


; - Experimental Validation
;  * Ported existing scripts
;  * Numeric Results
;  * Used in classes


(define (table-slide)
  
  (define left-col
    (tabular #:gap 12
             (list "Lines")
             (list "Increase")
             (list "Fixes (Good)")
             (list "Problems (Bad)")))
  
  (define (mytt txt header)
    (rb-superimpose
     (ghost (t header))
     (if (pict? txt) 
         txt
         ((current-code-tt) txt))))
  (define (mytts . txts)
    (map mytt txts headers))
  
  (define headers (list "Squad" "Metrics" "Acct" "Spam" "System" "Rand" "     Total"))
  
  (define top-row
    (tabular #:gap 12 headers))
  
  (define 6-1-8 (rb-superimpose (ghost (t "Rand")) ((current-code-tt) "618")))
  (define one ((current-code-tt) "1"))
  
  (slide 
   #:layout 'center
   #:title "Validation"
   (hbl-append
    left-col
    (blank 20);(vline 20 (pict-height left-col))
    (vl-append
     top-row
     (blank 20);(hline (pict-width top-row) 20)
     (frame 
      (hc-append 
       12
       (pin-line
        (tabular #:gap 12
        (mytts "2369"  "511"     "407"  "315" "1290" 6-1-8 "5510")
        (mytts "7%"    "25%"     "7%"   "6%"      "1%" "3%" "7%")
        (mytts "5"     "3"       "4"     "5"      "8" "0"  "25") 
        (mytts "7"     "4"       "3"     "1"      "0" one  "16"))
        6-1-8
        (move-find-left rt-find -35)
        one
        (move-find-left rb-find -35))
       (blank 10)))))))

(table-slide)
(start)

(staged
 [assert assert* div div*]
 (slide 
  #:title "Sample Fixes"
  (let ([body (pict-case 
               stage-name
               [(assert) (code (+ 10 (string->number str)))]
               [(assert*) (code (+ 10 (#,(red-code assert) (string->number str))))]
               [(div) (code (define (divs . args) 
                              (* -1 (apply / args))))]
               [(div*) (code (define (divs #,(red-code arg) . args)
                               (* -1 (apply / #,(red-code arg) args))))])])
    (case stage-name
      [(assert div) (smod #:name (symbol->string stage-name)
                          body)]
      [(assert*) (tmod #:name "assert"
                       body)]
      [(div*) (tmod #:name "div"
                    body)]))))

(slide/stage 
 [cond cond* mutate mutate*]
 #:title "Sample Problems"
 (let ([body (pict-case 
              stage-name
              ;; NB: `x' must be exact for this to work
              [(cond) (code (cond [(< x 0) 'negative]
                                  [(= x 0) 'zero]
                                  [(> x 0) 'positive]))]
              [(cond*) (code (cond [(< x 0) 'negative]
                                   [(= x 0) 'zero]
                                   [#,(red-code else)    'positive]))]
              [(mutate) (code (define pr (make-pair x y))
                              (when (string? (pair-left pr))
                                (set-pair-left! pr (string->symbol (pair-left pr)))))]
              [(mutate*) (code (define pr (make-pair 
                                           (if (string? x) (string->number x) x)
                                           y)))])])
   (case stage-name
     [(cond mutate) (smod #:name (symbol->string stage-name)
                          body)]
     [(cond*) (tmod #:name "cond"
                    body)]
     [(mutate*) (tmod #:name "mutate"
                      body)])))

;(tslide "Empirical Validation")

; - Implemented and Available
; - Validation
;  * Ported existing PLT software
;  * Measured required changes
;  * Show table
;  * Discuss major changes

;Related Work

(tslide "Related Work")

(slide 
 #:title "Interlanguage Integration" 
 (para "ProfessorJ")
 (subpara "Gray et al. (2005)")
 (para "Multilanguage Systems")
 (subpara "Matthews and Findler (2007)"))
(slide/stage
 [reynolds soft scheme strongtalk]
 #:title "Types for Untyped Languages"
 (para "John Reynolds (1968)")
 (case stage-name
   [(reynolds) (mini-slide (subpara "\"Some account should be taken of the premises")
                           (subpara "in conditional expressions.\""))]
   [else 'nothing])
 (para "Soft Typing")
 (case stage-name
   [(soft) (mini-slide (subpara "Fagan (1991), Aiken (1994)")
                       (subpara "Wright (1997), Flanagan (1999)"))]
   [else 'nothing])
 (para "Types for Scheme")
 (case stage-name
   [(scheme) (mini-slide (subpara "SPS (Wand 1984), Leavens (2005)")
                         (subpara "Infer (Haynes 1995)"))]
   [else 'nothing])
 (para "Strongtalk")
 (case stage-name
   [(strongtalk) (mini-slide (subpara "Bracha and Griswold (1993)")
                             (subpara ""))]
   [else 'nothing]))
(slide #:title "Contracts & Modules"
       (para "Contracts")       
       (subpara "Findler & Fellesien (2002)")
       (para "Modules with Macros")
       (subpara "Flatt (2002)"))
(slide #:title "Recent Work"
       (para "Gradual Typing")
       (subpara "Siek et al (2006-2009), Wadler & Findler (2007), Herman et al (2007)")
       (para "DRuby")
       (subpara "Furr et al (2009)"))

; - Interlanguage integration
; - Types for untyped languages
; - Prior work on contract/modules
; - Gradual Typing

;Conclusion/Thesis

(tslide "Conclusion")

(slide/stage 
 [one two three] 
 #:title "Conclusion"
 (colorize 
  (mini-slide
   (para "Methodology")
   (subitem "Modules + Contracts")
   (subitem "Custom Type System"))
  (if (eq? stage-name 'two) "gray" "black"))
 
 (colorize 
  (mini-slide
   (para "Typed Scheme")
   (subitem "Blame Theorem + Occurrence Typing")
   (subitem "Macros = Compilers")
   (subitem "Implemented and Available"))
  (if (eq? stage-name 'one) "gray" "black")))

(thanks)


