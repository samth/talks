#lang slideshow

(require slideshow/step slideshow/code slideshow/face (only-in slideshow/slide title-size)
         (except-in "beamer.ss" title) "lib.ss" scheme/gui "class-slide.ss" "thanks.ss" 
         "tslide.ss" "config.ss" 
         scheme/runtime-path mzlib/etc (planet cce/scheme:6:3/slideshow))

(title '("Logical Types for Scheme")
       '()
       '(("Sam Tobin-Hochstadt" "PLT @ Northeastern University"))
       "NEPLS, April 29, 2010")
(set-page-numbers-visible! #t)
(do-start? #f)

#|

Outline:

- Why types for untyped languages
- What's interesting/hard about occurrence typing?
- How does it work

|#

(slide/stage
 [one two three] #:title "What do these languages have in common?"
 (item "COBOL" (show (t " [Komondoor 05]") (at two)))
 (item "Scheme" (show (t " [Tobin-Hochstadt 06]") (at two)))
 (item "Ruby" (show (t " [Furr 09]") (at two)))
 (item "Haskell" (show (t " [Vytiniotis 10]") (at two)))
 (pict-case stage-name 
             [(one) (blank)]
             [(two) (para "New static checks")]
             [(three) (para "Millions of lines of code")]))

(tslide "Types for Existing Languages")

(define (text-cloud str)
  (define txt (t str))
  (cc-superimpose
   (cloud (* 1.6 (pict-width txt)) (* 1.6 (pict-height txt)))
   txt))

(define-syntax-rule (at stg) (>= stage stg))
(define-syntax-rule (btw stg1 stg2) (<= stg1 stage stg2))

(define sz (lt-superimpose
            (code (: car-num? : (Pair Any Any) -> Boolean : Number @ car))
            (code ||
                  ||
                  ||
                  ||
                  ||
                  ||
                  ||
                  ||)))

(slide/stage 
 [zero one two three four five six]
 #:title "What's Hard?"
 (transparent-block 
  (vc-append 10
   (hc-append 20 (show (text-cloud "Simple Types") (at one)) (show (text-cloud "Classes and Objects") (at six)))
   (hc-append 20 (show (text-cloud "Dependent Types") (at four)) (show (text-cloud "Parametricity") (at three)))
   (hc-append 20 (show (text-cloud "Reflection") (at two)) (show (text-cloud "Generic Functions") (at five)))))
 (blank 40)
 (para "How programmers reason"))

(slide #:title "Checking Existing Code"
       (item "New static checking is valuable for existing code")
       (subpara "Maintenance, Optimization, Trust")
       (item "Work with existing idioms")
       (subpara "Survey, Analyze, Design, Validate"))

(slide #:title "What Can We Learn?"
       (item "New points in the design space")
       (subpara "Ruby, Scheme, ...")
       ;'next
       (item "New type system ideas")
       (subpara "Occurrence Typing"))

(tslide "Occurrence Typing")

(slide/stage 
 [one one* two two*]
 #:title "Simple Examples"
 (tmod #:sizeof sz
  (pict-case stage-name
    [(one one*)
     (code
      (: twice : Any -> Number)
      (define (twice x)
        (if #,(case stage-name [(one) (code (number? x))] [(one*) (red-code (number? x))])
            (* 2 x)
            0)))]
    [(two two*)
     (code
      (: twice : (U Number String) -> Number)
      (define (twice x)
        (if #,(case stage-name [(two) (code (number? x))] [(two*) (blue-code (number? x))])
            (* 2 x)
            (* 2 (string-length x)))))]))
 (pict-case stage-name
   [(one*) (code ⊢ Number_x)]
   [(two*) (code ⊢ #,(overbar (code Number_x)))]
   [else (blank)]))

(start)

(slide/stage 
 [one one* one** two two* three three* three** three***]
 #:title "Logical Combination"             
 (tmod #:sizeof sz
  (code 
   (: twice : (U Number String) -> Number)
   ||
   (: g : Any (U Number String) -> Number)
   (define (g x y)
     #,(pict-case stage-name
         [(one one* one**)
          (code (cond [#,(red-code (or (number? x) (string? x)))
                       (twice x)]
                      ||
                      [else 0]))]
         [(two two*)
          (code (cond [#,(red-code (and (number? x) (string? y)))
                       (+ (twice x) (twice y))]
                      ||
                      [else 0]))]
         [(three three*)
          (code (cond [#,(blue-code (and (number? x) (string? y)))
                       (+ (twice x) (twice y))]
                      [(number? x) (* 2 y)]
                      [else 0]))]
         [(three** three***)
          (code (cond [#,(blue-code (and (number? x) (string? y)))
                       (+ (twice x) (twice y))]
                      [#,(red-code (number? x)) (* 2 y)]
                      [else 0]))]))))
 (pict-case stage-name
            [(one*) (code ⊢ Number_x ∨ String_x)]
            [(one**) (code Number_x ∨ String_x ⊢ |(U Number String)_x|)]
            [(two*) (code ⊢ Number_x ∧ String_y)]
            [(three*) (code ⊢ #,(overbar (code Number_x)) ∨ #,(overbar (code String_y)))]
            [(three**) (code #,(overbar (code Number_x)) ∨ #,(overbar (code String_y))|,| Number_x ⊢ #,(overbar (code String_y)))]
            [(three***) (code |(U Number String)_y| |,| #,(overbar (code String_y)) ⊢ Number_y)]
            [else (blank)]))

(slide #:title "Data Structures"
       (tmod #:sizeof sz
        (code
         (: twice-car : (Pair Any Any)  -> Number)
         (define (twice-car x)
           (if #,(red-code (number? (car x)))
               (* 2 (car x))
               0))))
       'next
       (code ⊢ Number_|car(x)|))

(slide/stage [one two]
             #:title "Abstraction"
             (tmod  #:sizeof sz
                    (code
                     (: car-num? : (Pair Any Any) -> Boolean : #,(pict-case stage-name [(one) (code Number @ car)] [(two) (red-code Number @ car)]))
                     (define (car-num? x)
                       #,(red-code (number? (car x))))))
             (ghost (code ||)))

(tslide "Propositional Logic")

(slide/stage 
 [j expr type prop prop2 env env2 #;obj]
 #:title "Judgments"
 (pict-case 
  stage-name #:combine cc-superimpose
  [(j) (code Γ ⊢ e : T |;| φ_1 \| φ_2)]
  [(expr) (code Γ ⊢ #,(red-code e) : T |;| φ_1 \| φ_2)]
  [(type) (code Γ ⊢ e : #,(red-code T) |;| φ_1 \| φ_2)]
  [(prop prop2) (code Γ ⊢ e : T |;| #,(red-code φ_1) \| #,(blue-code φ_2))]
  [(env env2) (code #,(red-code Γ) ⊢ e : T |;| φ_1 \| φ_2)]
  [(obj) (code Γ ⊢ e : T |;| φ_1 \| φ_2 |;| #,(red-code o))]
  [(j2 j2*) (code Γ ⊢ φ)]
  [else (blank)])
 (blank 20)
 (pict-case 
  stage-name
  [(expr) (code e ::= n \| c \| (λ x : T . e) \| (e e) \| (if e e e))]
  [(type) (code T ::= Number \| (U T ...) \| #t \| #f \| (x:T -> T : φ\|φ))]
  [(prop) (code φ ::= |T_π(x)| \| #,(overbar (code |T_π(x)|)) \| φ_1 ∨ φ_2 \| φ_1 ∧ φ_2 \| φ_1 ⊃ φ_2)]
  [(prop2) (code φ ::= #,(red-code |T_π(x)|) \| #,(overbar (code |T_π(x)|)) \| φ_1 ∨ φ_2 \| φ_1 ∧ φ_2 \| φ_1 ⊃ φ_2)]
  [(env) (code Γ ::= |T_π(x)| ...)]
  [(env2) (code Γ ::= φ ...)]
  [(obj) (code o := ∅ \| |π(x)|)]
  [(j2*) (code #,(overbar (code Number_x)) ∨ #,(overbar (code String_y))|,| Number_x ⊢ #,(overbar (code String_y)))]
  [else (blank)]))

(slide/stage 
 [j2 j2*]
 #:title "Judgments"
 (code Γ ⊢ φ)
 (blank 20)
 (pict-case stage-name
            [(j2*) (code #,(overbar (code Number_x)) ∨ #,(overbar (code String_y))|,| Number_x ⊢ #,(overbar (code String_y)))]
            [else (blank)]))

(start)

(define-syntax-rule (def-p p+ p- p+- + - code1 code2)
  (begin
    (define p+ (code1 +))
    (define p- (code2 -))
    (define p+- (hbl-append p+ (code \|) p-))))

(def-p p+ p- p+- φ_+ φ_- code code)
(def-p r1p+ r1p- r1p+- φ_+ φ_- red-code code)
(def-p r2p+ r2p- r2p+- φ_+ φ_- code blue-code)

(def-p p1+ p1- p1+- φ1_+ φ1_- code code)
(def-p p2+ p2- p2+- φ2_+ φ2_- code code)

(def-p r1p1+ r1p1- r1p1+- φ1_+ φ1_- red-code code)
(def-p r1p2+ r1p2- r1p2+- φ2_+ φ2_- red-code code)

(def-p r2p1+ r2p1- r2p1+- φ1_+ φ1_- code blue-code)
(def-p r2p2+ r2p2- r2p2+- φ2_+ φ2_- code blue-code)

(define result (hbl-append p1+ (code ∨) p2+ (blank 10) (code \|) (blank 10) p1- (code ∨) p2-))
(define resultr1 (hbl-append r1p1+ (code ∨) r1p2+ (blank 10) (code \|) (blank 10) p1- (code ∨) p2-))
(define resultr2 (hbl-append p1+ (code ∨) p2+ (blank 10) (code \|) (blank 10) r2p1- (code ∨) r2p2-))



(slide/stage
 [zero one two three four four* four**]
 #:title "Typing"
 (pict-case stage-name
            [(zero) (code |   |(if e_1 e_2 e_3))]
            [(one one* one**) (code |   |(if #,(red-code e_1) e_2 e_3))]
            [(two) (code |   |(if e_1 #,(red-code e_2) e_3))]
            [(three) (code |   |(if e_1 e_2 #,(red-code e_3)))]
            [(four) (code Γ ⊢ (if e_1 e_2 e_3) : T |;| #,result)]
            [(four*) (code Γ ⊢ (if e_1 e_2 e_3) : T |;| #,resultr1)]
            [(four**) (code Γ ⊢ (if e_1 e_2 e_3) : T |;| #,resultr2)])
 (blank 20)
 (pict-case 
  stage-name
  [(one four four* four**) (code Γ ⊢ e_1 : T_1 |;| #,p+-)]
  [(two) (code Γ ⊢ e_1 : T_1 |;| #,r1p+-)]
  [(three) (code Γ ⊢ e_1 : T_1 |;| #,r2p+-)]
  [else (blank)])
 (pict-case 
  stage-name
  [(one) (blank)]
  [(two three four) (code #,(hbl-append (code |Γ,|) (case stage-name [(two) r1p+] [else p+])) ⊢ e_2 : T |;| #,p1+-)]
  [(four*) (code #,(hbl-append (code |Γ,|) (case stage-name [(two) r1p+] [else p+])) ⊢ e_2 : T |;| #,r1p1+-)]
  [(four**) (code #,(hbl-append (code |Γ,|) (case stage-name [(two) r1p+] [else p+])) ⊢ e_2 : T |;| #,r2p1+-)]
  [else (blank)])
 
 (pict-case 
  stage-name
  [(two three four) (code #,(hbl-append (code |Γ,|) (case stage-name [(three) r2p-] [else p-])) ⊢ e_3 : T |;| #,p2+-)]
  [(four*) (code #,(hbl-append (code |Γ,|) (case stage-name [(three) r2p-] [else p-])) ⊢ e_3 : T |;| #,r1p2+-)]
  [(four**) (code #,(hbl-append (code |Γ,|) (case stage-name [(three) r2p-] [else p-])) ⊢ e_3 : T |;| #,r2p2+-)]
  [else (blank)]))

(slide/stage
 [one] #:title "Typing"
 (code Γ ⊢ x : T)
 (blank 20) 'next
 (code Γ ⊢ T_x))

(thanks)


