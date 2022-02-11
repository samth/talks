#lang racket

(require slideshow "lib.ss" (except-in unstable/gui/slideshow shade) slideshow/code "config.rkt" "ts-intro.rkt")

(provide sum2 sum1 pic)

(def-repeated (n1 n2 n3 n4) (code tr))
(define tN (code Tree))
(define tZ (code Number))
(define tS (code (Pair Tree Tree)))
(define zero (code n))
(define sym? (code (number? #,n2)))
(define els (code (+ (sum (first #,n3)) (sum (rest #,n4)))))

(define pic
  (code 
   (define-type Tree (U #,(launder tZ) #,(launder tS)))
   ||
   (: sum : Tree -> Number)
   (define (sum #,n1)
     (cond [#,sym? #,zero]
           [else #,els]))))

(require "ts-intro.rkt" ppict/2)

(define (sum1)
  (staged
   [one two three four five]
   (define (build n ty )
     (define ty* (code n : #,ty))
     (connect (mini-slide (shaded pic) (blank 50) (pict-if #t ty* (launder (code x : #,tS))))
              n ty*)) 
   (tr-slide "Occurrence Typing"
              #:go (coord .5 .5 'cc)
          (pict-case stage-name
            [(one) pic]
            [(two) (build n1 tN)]
            [(three) (build n2 tN)]
            [(four) (build zero tZ)]
            [(five)
             (let ()
               (define ty* (code n : #,tS))
               (connect (connect (mini-slide (shaded pic) (blank 50) (pict-if #t ty* (launder (code x : #,tS))))
                                 n3 ty*)
                        n4 ty*))]))))

;; example 1, formal

(define (sum2)
  (staged 
   [one two three four six seven]      
   (define ty1 (code n : #,tZ))
   (define ty2 (code n : #,tS))
   (define prop1 (code ⊢ #,tZ @ t))
   (define prop2 (code ⊢ #,tS @ t)) 
   (define prop3 (code ⊢ #,(overbar (code Number @ n))))
   (define prem0 (code ⊢  Tree @ t |  | #,prop3))
   (define prem (code ⊢  (U #,tZ #,tS) @ t |  | #,prop3))
   (define deriv
     (vc-append
      prem
      (hline (pict-width prem) 5)
      prop2))
   (define prop2*
     (pin-over (ghost deriv) prop2 lt-find prop2))
   (define prop4 (pict-case stage-name
                   [(four)
                    (vc-append
                     (ghost prem)
                     (hline (pict-width prem) 5)
                     prop2)]
                   [(six)
                    (vc-append
                     (rt-superimpose prem0 (ghost prem))
                     (hline (pict-width prem) 5)
                     prop2)]
                   [(seven)
                    deriv])) 
   (tr-slide  "Occurrence Typing, Formally"
              #:go (coord .5 .5 'cc)
    (pict-case stage-name
            [(one) (mini-slide pic
                               (blank 50)
                               (ghost prop4))]
            [(two) (connect 
                    (mini-slide (shaded pic)
                                (blank 50)
                                (pin-over (ghost prop2*) prop2 lt-find ty2))
                    els ty2)]
            [(three) 
             (connect 
              (mini-slide (shaded pic)
                          (blank 50)
                          prop2*)
              els prop2)]
            [(four five six seven) 
             (connect 
              (mini-slide (shaded pic)
                          (blank 50)
                          prop4)
              sym? prop3)]))))


(define (rule [hide? #f])
  (define h  (if hide? ghost values))
  (define prem1 (code Γ ⊢ |op | : x:σ → τ))
  (define prem2 (code Γ ⊢ arg : σ #,(h (code \;)) #,(h (red-code o))))
  (define concl (code Γ ⊢ (op arg) : #,(values (code τ))#,(inset (h (red-code[x ↦ o])) (- (pict-width (code | |))) 0 0 0)))
  (define line (hline (apply max (map pict-width (list prem1 prem2 concl))) 5))
  (scale (vc-append (vl-append prem1 prem2) line concl) 1.3))

(def-repeated (x1 x2 x3 x4 x5) (code x))

(define the-xl (inset x5 (- (pict-width (code x))) 0 0 0))
(define the-xr (inset x4 0 0 (- (pict-width (code x))) 0))
(define types
  (code τ ::= Int \| #,(values the-xr):τ → σ \| (Refine #,x3 Ψ) ...))
(define props
  (code Ψ ::= Ψ ∧ Φ \| #,x2 ∈ τ \| a #,the-xl + ... < by + ...))
(define objects
  (list (code o ::= ∅ \| x)
        (vl-append types props (code o ::= ∅ \| #,x1))
        (vl-append types props (code o ::= ∅ \| #,x1 \| (fst o) \| (snd o)))
        (vl-append types props (code o ::= ∅ \| #,x1 \| (+ (* n1 o1) ...) \| (len o)))))


(define (norm-slide ref)
  (tr-slide "Linear Refinements"
            (scale
             (code
              (: norm : (Vectorof Real) → Real)
              (define (norm x)
                (let loop ([i : Nat 0] [res : Real 0])
                  (cond [(< i (length x))
                         (define xᵢ #,ref)
                         (loop (+ 1 i) ( + res (expt xᵢ 2)))]
                        [else (sqrt res)]))))
             1.4)))
(provide refinements)
(define (refinements)
(norm-slide (code (vector-ref x i)))
(norm-slide (red-code (safe-vector-ref x i)))


(tr-slide "Linear Refinements"
          #:alt (#:go (coord .5 .6 'cc) (rule #t))
          #:go (coord .5 .6 'cc) (rule #f)
          #:go (coord .2 .3 'lb)
          #:alt ((first objects))
          #:alt ((second objects))
          #:alt ((third objects))
          (highlight-on (fourth objects) x1 x2 x3 x4 x5)
          ))


;(module+ main (sum1) (sum2))
