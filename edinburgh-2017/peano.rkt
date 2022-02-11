#lang racket

(require slideshow "lib.ss" (except-in unstable/gui/slideshow shade) slideshow/code "config.rkt" "ts-intro.rkt")

(provide peano2 peano1 pic)

(def-repeated (n1 n2 n3) (code n))
(define tN (code Peano))
(define tZ (code 'Z))
(define tS (code (List 'S Peano)))
(define zero (code 0))
(define sym? (code (symbol? #,n2)))
(define els (code (add1 (convert (rest #,n3)))))

(define pic
  (code 
   (define-type Peano (U #,(launder tZ) #,(launder tS)))
   ||
   (: convert : Peano -> Number)
   (define (convert #,n1)
     (cond [#,sym? #,zero]
           [else #,els]))))

(define (peano1)
  (staged
   [one two three four five]
   (define (build n ty)
     (define ty* (code n : #,ty))
     (connect (mini-slide (shaded pic) (blank 50) (pict-if #t ty* (launder (code x : #,tS))))
              n ty*)) 
   (slide #:title (title-t "Occurrence Typing, Informally")
          (pict-case stage-name
            [(one) pic]
            [(two) (build n1 tN)]
            [(three) (build n2 tN)]
            [(four) (build zero tZ)]
            [(five) (build n3 tS)]))))

;; example 1, formal

(define (peano2)
  (staged 
   [one two three four six seven]      
   (define ty1 (code n : #,tZ))
   (define ty2 (code n : #,tS))
   (define prop1 (code ⊢ #,tZ @ n))
   (define prop2 (code ⊢ #,tS @ n)) 
   (define prop3 (code ⊢ #,(overbar (code Symbol @ n))))
   (define prem0 (code ⊢  Peano @ n |  | #,prop3))
   (define prem (code ⊢  (U #,tZ #,tS) @ n |  | #,prop3))
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
   (slide #:title (title-t "Occurrence Typing, Formally")
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

