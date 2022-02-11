#lang slideshow

(require unstable/gui/ppict "config.rkt" "helper.rkt" "beamer.rkt"
         unstable/gui/pslide unstable/gui/pict/plt-logo pict/convert
         images/logos unstable/gui/pict "peano.rkt" "combine.rkt"
         "tslide.rkt" (except-in "lib.rkt" title)
         pict/code unstable/gui/slideshow)

(require ppict-slide-grid)
;(set-grid-base-pict!)

(pslide
 #:go (coord .5 .5 'cc) 
 (bitmap plt-background-path)
 #:go (coord 0.0 .98 'lb)
 (t/cant "Sam Tobin-Hochstadt" size2)
 (t/cant "Indiana University" size3)
 #:go (coord 1 .98 'rb)
 (t/quat "Dagstuhl, March 2016" size2)

 #:go (coord 0.0 .25 'lc)
 (t/kau "Occurrence Typing modulo Theories" size1))


(pslide
 #:go (coord .5 .5 'cc) 
 (bitmap plt-background-path)
 #:go (coord 0.0 .98 'lb)
 (t/cant "Sam Tobin-Hochstadt" size2)
 (t/cant "Indiana University" size3)
 #:go (coord 1 .98 'rb)
 (t/quat "Dagstuhl, March 2016" size2)

 #:go (coord 0.0 .25 'lc)
 (t/kau "Typed Racket + Refinement Types" size1))

(tslide "Reasoning about type tests")

(peano1)


(peano2)

(slide
 (t/cant "Type assignment is a logical proposition" size2))


(define-syntax-rule (pslide/staged/proc [ids ...] . rest)
  (lambda (name)
    (case name
      [(ids) (pslide/staged (ids) . rest)] ...)))
                                      

(define f
(pslide/staged/proc 
 [j  prop prop2 prop3 prop4 ]
 (pict-case 
  stage-name #:combine cc-superimpose
  [(j) (code Γ ⊢ e : T |;| φ_1 \| φ_2)]
  [(expr) (code Γ ⊢ #,(code e) : T |;| φ_1 \| φ_2 |;| o)]
  [(type) (code Γ ⊢ e : #,(code T) |;| φ_1 \| φ_2 |;| o)]
  [(prop) (code Γ ⊢ e : T |;| #,(code φ_1) \| #,(code φ_2))]
  [(prop2) (code Γ ⊢ e : T |;| #,(code φ_1) \| #,(code φ_2))]
  [(prop3 prop4) (code φ ⊢ e : T |;| #,(code φ_1) \| #,(code φ_2))]
  [(env env2 env0) (code #,(code Γ) ⊢ e : T |;| φ_1 \| φ_2 |;| o)]
  [(obj) (code Γ ⊢ e : T |;| φ_1 \| φ_2 |;| #,(code o))]
  [(j2 j2*) (code Γ ⊢ φ)]
  [else (blank)])
 (blank 20)
 (pict-case 
  stage-name
   [(expr) (code e ::= n \| c \| (λ x : T . e) \| (e e) \| (if e e e))]
   [(type) (code T ::= Number \| (U T ...) \| #t \| #f \| (x:T -> T : φ\|φ))]
   [(prop)  (code φ ::= |T@π(x)| \| #,(overbar (code |T@π(x)|)))]
   [(prop2 prop3) (code φ ::= |T@π(x)| \| #,(overbar (code |T@π(x)|)) \| φ_1 ∨ φ_2 \| φ_1 ∧ φ_2 \| φ_1 ⊃ φ_2)]
   [(prop4) (vl-append (code  φ ::= |T@π(x)| \| #,(overbar (code |T@π(x)|)) \| φ_1 ∨ φ_2 \| φ_1 ∧ φ_2 \| φ_1 ⊃ φ_2)
                       (code |   | \| Th_i))]
   [(j2*) (code #,(overbar (code Number @ x)) ∨ #,(overbar (code String @ y))|,| Number @ x ⊢ #,(overbar (code String @ y)))]
   [else (blank)])))

(f 'j)
(f 'prop)
#;
(slide/staged 
 [j2 j2*]
 ;#:title "Judgments"
 (code Γ ⊢ φ)
 (blank 20)
 (pict-case stage-name
            [(j2*) (code #,(overbar (code Number @ x)) ∨ #,(overbar (code String @ y))|,| Number @ x ⊢ #,(overbar (code String @ y)))]
            [else (blank)]))


(combine2)

(f 'prop2)
(f 'prop3)
(f 'prop4)

(slide
 (t/cant "And that's it ... " size2))

