#lang slideshow

(require slideshow/step slideshow/code slideshow/face (only-in slideshow/slide title-size)
         (except-in "beamer.ss" title) "lib.ss" racket/gui "class-slide.ss" "thanks.ss" 
         "tslide.ss" "config.ss"  "peano.rkt" "combine.rkt"
         racket/runtime-path mzlib/etc (except-in unstable/gui/slideshow at))

(title '("Logical Types for Untyped Languages")
       '()
       '(("Sam Tobin-Hochstadt & Matthias Felleisen" "PLT @ Northeastern University"))
       "ICFP, September 27, 2010")
(set-page-numbers-visible! #f)
(do-start? #f)

#|

Outline:

- Why types for untyped languages
- What's interesting/hard about occurrence typing?
- How does it work

|#

(slide 
 #:layout 'top 
 (blank 100)
 (para "Types for Untyped Languages:")
 (item "Ruby [Furr et al 09]")
 (item "Perl [Tang 06]")
 (item "Thorn [Wrigstad et al 09]")
 (item "ActionScript [Adobe 06]")
 (blank 40)
 (colorize (item "Typed Racket") "red"))

(slide 
 #:layout 'top 
 (blank 100)
 (para "Types for Untyped Languages:")
 (item "Reynolds 68")
 (item "Cartwright 75")
 (item "Wright & Cartwright 94")
 (item "Henglein & Rehof 95"))

(slide/staged
 [rey heng] 
 (para
  (pict-case stage-name
    [(rey) (t "\"Some account should be taken of the premises in conditional expressions.\"")]
    [(heng) (t "\"Type testing predicates aggravate the loss of static type information.\"")]))
 (blank 50)
 (para #:width 550 #:align 'right
       (case stage-name
         [(rey) "Reynolds, 1968"]
         [(heng) "Henglein & Rehof, 1995"])))

(tslide "Types and Predicates")

;; Example 1, informal

(peano1)

;; example 2, informal
(combine1)

(tslide "Types and Propositions")

(peano2)
(combine2)

(define-syntax-rule (bigger . arg)
  (parameterize ([current-font-size (+ 5 (current-font-size))]) . arg))

(define (connect/when p? big . args)
  (if p? (apply connect big args) big))

(staged
 [one two two* three four five]
 (define str-ty (code (x:Any -> Bool : String@x \| #,(overbar (code String@x)))))
 (define s-obj-small (code s))
 (define s-obj (pict-case stage-name #:combine cc-superimpose [(five) (code |car(s)|)] [else s-obj-small]))
 (define s0 (bigger (code s)))
 (define car-s0 (bigger (code (car #,s0))))
 (define s (bigger (case stage-name [(five) car-s0] [else (pin-over (ghost car-s0) s0 lt-find s0)])))
 (define cars (bigger (code (car #,s0))))
 (define str? (bigger (code string?)))
 (define test (bigger (code (#,str? #,s))))
 (slide 
    (refocus
     (connect/when 
   (at three)
   (connect/when 
    (at three)
 (mini-slide
     (show (code #,(vc-append 10 (show (t "Latent Propositions") (at four)) str-ty) |   | #,(vc-append 10 (show (t "Objects") (at four)) s-obj)) (at three))
     (blank 50)
     test
     (blank 50)
     (pict-case stage-name
       [(two) (bigger (code #,(rt-superimpose (ghost (code String @ |car(s)|))  (code String @ s)) | | #,(ghost (lt-superimpose (ghost (overbar (code String @ |car(s)|))) (overbar (code String @ s))))))]
       [(two* three) (bigger (code #,(rt-superimpose (ghost (code String @ |car(s)|))  (code String @ s)) \| #,(lt-superimpose (ghost (overbar (code String @ |car(s)|))) (overbar (code String @ s)))))]
       [(four) (let ([p (bigger (code #,(rt-superimpose (ghost (code String @ |car(s)|))  (code String @ s)) \| #,(lt-superimpose (ghost (overbar (code String @ |car(s)|))) (overbar (code String @ s)))))])
                 (refocus (vc-append 10
                            p
                            (t "Propositions")) p))]
       [(five) (let ([p (bigger (code String @ |car(s)| \| #,(overbar (code String @ |car(s)|))))])
                 (refocus (vc-append 10
                                     p
                                     (t "Propositions")) p))]))
    str-ty str? 5)   
   (if (at five) car-s0 s0) (if (at five) s-obj s-obj-small) 5) test)
    )
)

(staged 
 [#;one two three]
 (define test (bigger (code (string? (car s)))))
 (define lam (bigger (code (λ ([s : (Pair Any Any)])
                             #,test))))
 (define filters (bigger (code String @ |car(s)| \| #,(overbar (code String @ |car(s)|)))))
 (define type 
   (bigger (code (|s:(Pair Any Any)| -> Bool : 
                  #,filters))))
 (slide (case stage-name
          [(one)
           (refocus lam test)]
          [(two) (refocus (mini-slide lam
                                      (blank 15)
                                      (pin-over (ghost type) filters lt-find filters))
                          test)]
          [(three) (refocus (mini-slide lam
                                        (blank 15)
                                        type)
                            test)])))

(start)

(define-syntax-rule (at stg) (>= stage stg))
(define-syntax-rule (btw stg1 stg2) (<= stg1 stage stg2))

(tslide "Propositional Logic")

(slide/staged 
 [j expr type prop #;prop2 env0 env env2 #;obj]
 #:title "Judgments"
 (pict-case 
  stage-name #:combine cc-superimpose
  [(j) (code Γ ⊢ e : T |;| φ_1 \| φ_2 |;| o)]
  [(expr) (code Γ ⊢ #,(red-code e) : T |;| φ_1 \| φ_2 |;| o)]
  [(type) (code Γ ⊢ e : #,(red-code T) |;| φ_1 \| φ_2 |;| o)]
  [(prop prop2) (code Γ ⊢ e : T |;| #,(red-code φ_1) \| #,(blue-code φ_2) |;| o)]
  [(env env2 env0) (code #,(red-code Γ) ⊢ e : T |;| φ_1 \| φ_2 |;| o)]
  [(obj) (code Γ ⊢ e : T |;| φ_1 \| φ_2 |;| #,(red-code o))]
  [(j2 j2*) (code Γ ⊢ φ)]
  [else (blank)])
 (blank 20)
 (pict-case 
  stage-name
   [(expr) (code e ::= n \| c \| (λ x : T . e) \| (e e) \| (if e e e))]
   [(type) (code T ::= Number \| (U T ...) \| #t \| #f \| (x:T -> T : φ\|φ))]
   [(prop) (code φ ::= |T@π(x)| \| #,(overbar (code |T@π(x)|)) \| φ_1 ∨ φ_2 \| φ_1 ∧ φ_2 \| φ_1 ⊃ φ_2)]
   [(prop2) (code φ ::= #,(red-code |T@π(x)|) \| #,(overbar (code |T@π(x)|)) \| φ_1 ∨ φ_2 \| φ_1 ∧ φ_2 \| φ_1 ⊃ φ_2)]
   [(env0) (code Γ ::= x:T ...)]
   [(env) (code Γ ::= #,(linewidth 3 (strike (code x:T))) |T@π(x)| ...)]
   [(env2) (code Γ ::= #,(linewidth 3 (strike (code x:T))) #,(linewidth 3 (strike (code |T@π(x)|))) φ ...)]
   [(obj) (code o := ∅ \| |π(x)|)]
   [(j2*) (code #,(overbar (code Number @ x)) ∨ #,(overbar (code String @ y))|,| Number @ x ⊢ #,(overbar (code String @ y)))]
   [else (blank)]))

(slide/staged 
 [j2 j2*]
 #:title "Judgments"
 (code Γ ⊢ φ)
 (blank 20)
 (pict-case stage-name
            [(j2*) (code #,(overbar (code Number @ x)) ∨ #,(overbar (code String @ y))|,| Number @ x ⊢ #,(overbar (code String @ y)))]
            [else (blank)]))



(define-syntax-rule (def-p p+ p- p+- + - code1 code2)
  (begin
    (define p+ (code1 +))
    (define p- (code2 -))
    (define p+- (hbl-append p+ (code \|) p-))))

(def-p p+ p- p+- φ_+ φ_- code code)
(def-p r1p+ r1p- r1p+- φ_+ φ_- red-code code)
(def-p r2p+ r2p- r2p+- φ_+ φ_- code blue-code)

(def-p p1+ p1- p1+- φ_1+ φ_1- code code)
(def-p p2+ p2- p2+- φ_2+ φ_2- code code)

(def-p r1p1+ r1p1- r1p1+- φ_1+ φ_1- red-code blue-code)
(def-p r1p2+ r1p2- r1p2+- φ_2+ φ_2- red-code blue-code)

(def-p r2p1+ r2p1- r2p1+- φ_1+ φ_1- red-code blue-code)
(def-p r2p2+ r2p2- r2p2+- φ_2+ φ_2- red-code blue-code)

(define (result stg)
  (hbl-append (if (= stg 1) r1p1+ p1+)
              (code ∨)
              (if (= stg 1) r1p2+ p2+)
              (blank 10)
              (code \|)
              (blank 10)
              (if (= stg 1) r2p1- p1-)
              (code ∨)
              (if (= stg 1) r2p2- p2-)))
(define resultr0 (result 0))
(define resultr1 (result 1))
(define resultr2 (result 2))



(staged
 [zero one  four**]
 (define expr (pict-case stage-name
                         [(zero four four* four**) (code (if e_1 e_2 e_3))]
                         [(one one* one**) (code (if #,(red-code e_1) e_2 e_3))]
                         [(two) (code (if e_1 #,(red-code e_2) e_3))]
                         [(three) (code (if e_1 e_2 #,(red-code e_3)))]))
 (define full (code Γ ⊢ #,expr : T |;| #,(result (- stage four**)) |;| o))
 (define filter1 (pict-case   
                  stage-name
                  [(one four four* four**) p+-]
                  [(two) r1p+-]
                  [(three) r2p+-]))
 (define filter2 (case stage-name
                   [(four*)  r1p1+-]
                   [(four**) r2p1+-]
                   [else     p1+-]))
 (define filter3 (case stage-name
                   [(four*)  r1p2+-]
                   [(four**) r2p2+-]
                   [else     p2+-]))
 (define concl (pict-case stage-name
                          [(four four* four**) full]
                          [else (pin-over (ghost full) expr lt-find expr)]))
 (define prem1 (code Γ ⊢ e_1 : |T'| #,(hide (code |;|) (= stage zero)) #,filter1 |;| |o'|))
 (define prem2 (code #,(hbl-append (code |Γ,|) r1p+) ⊢ e_2 : T |;| #,filter2 |;| o))
 (define prem3 (code #,(hbl-append (code |Γ,|) r2p-) ⊢ e_3 : T |;| #,filter3 |;| o))
 (slide #:title "Typing" 
        (show prem2 (at one))
        (show prem3 (at one))
        (show prem1 (at one))
        (show (hline (pict-width concl) 5) (at one))
        concl))

(define concl2 (code Γ ⊢ x : T))
(slide/staged
 [one two] #:title "Typing" 
 (show (code Γ ⊢ T_x) (at two))
 (show (hline (pict-width concl2) 5) (at two))
 concl2)

(define check-bmp (scale (bitmap "check.png") .2))
(define check (refocus (vl-append (blank 100) check-bmp) check-bmp))

(tslide "Evaluation")

(slide/staged 
 [#;one two]
 #:layout 'top
 (blank 100)
 (two-columns
  (mini-slide (colorize (text "Scaling" (current-title-font) title-size) "blue")
              (para "Multiple Arguments")
              (para "Multiple Values")
              (para "User-defined Datatypes")
              ((if (= stage two) (λ (p) (colorize p "red")) values)
               (para "Local Binding")))
  (mini-slide (colorize (text "Adapting" (current-title-font) title-size) "blue")
              (para "Mutable Structures")
              (para "Mutable Variables")
              (para "")
              (para ""))))
(staged
 [one]
 (define imp (code #,(overbar (code #f @ x)) ⊃ φ*_+))
 (slide 
  #:title "Local Binding"
  (vc-append
   10
   (code Γ ⊢ e_0 : S |;| φ*_+ \| φ*_- |;| o)
   (code |Γ, S@x, | #,(refocus (highlight imp) imp) ⊢ e_1 : T |;| φ_+ \| φ_- |;| o*)
   (hline (pict-width (code Γ ⊢ (let [x e_0] e_1) : |T[o/x]| |;| φ_+ \| φ_- |[o/x]| |;| |o*[o/x]|)) 5)
   (code Γ ⊢ (let [x e_0] e_1) : |T[o/x]| |;| φ_+ \| φ_- |[o/x]| |;| |o*[o/x]|))))
(start)
(slide #:title "Empirical Evaluation"
       (para "Estimated usage of two idioms in Racket code base" (t "(600k lines)"))
       (blank 25)
       (item "Local binding with" (code or) ":  ~470 uses")
       (item "Predicates with Selectors:  ~440 uses"))

(slide #:title "Prior Work"
       (para "None of the Examples")
       (subitem "Shivers 91, Henglein & Rehof 95, Crary et al 98, ...")
       (para "Just" (code convert))
       (subitem "Aiken et al 94, Wright 94, Flanagan 97," (t "Komondoor et al 2005"))
       (para "Everything but abstraction")
       (subitem "Bierman et al 2010"))



(slide
 #:title "Conclusions"
 (para (t "Propositions can") (it "relate") (t "types and terms"))
 (blank 30)
 'next
 (para (t "Existing programs are a source of type system ideas"))
 )



(thanks)


