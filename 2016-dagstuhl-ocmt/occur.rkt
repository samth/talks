#lang slideshow

(require slideshow/step slideshow/code slideshow/face (only-in slideshow/slide title-size)
         (except-in "beamer.rkt" title) "lib.rkt" racket/gui "thanks.ss" 
         "tslide.ss" "config.rkt"  "peano.rkt" "combine.rkt"
         racket/runtime-path mzlib/etc (except-in unstable/gui/slideshow at))


#|

Outline:

- Why types for untyped languages
- What's interesting/hard about occurrence typing?
- How does it work

|#


(define rey-quote
  (vr-append 10 (vl-append (t "“Some account should be taken of the premises in conditional expressions.” "))
             (t "    — John Reynolds, 1968")))
(define heng-quote
  (vr-append 10 (vl-append (t "“Type testing predicates aggravate the loss of static type information.” "))
             (t "    — Fritz Henglein, 1995")))

(slide/staged
 [rey+heng]
 (vr-append 60 rey-quote heng-quote))

(tslide "So what?")

(slide #:title "Unions" #:layout 'center
       (scale (bitmap "UnionYesLogo.gif") 0.7)
       'next
       (t "Occurrence typing is an elimination principle for union types."))

(slide #:title "Unions are everywhere"
       (para "Scripting languages use unions pervasively")
       (subitem (vl-append ((current-code-tt) "JSON = null or true or false or")
                           ((current-code-tt) "     JSONNumber or JSONString or")
                           ((current-code-tt) "     JSONObject or JSONArray")))
       (subitem "S-expressions")
       (subitem ((current-code-tt) "open(\"somefile\") || die"))
       (subitem "..."))

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

(tslide "A Surprising Application")

(slide
 (bitmap "numbers.jpg"))

(slide
 (vl-append 
  (code 1)
  (blank 20)
  (code 17/2)
  (blank 20)
  (code 40000000000000000000000000000000000000000)
  (blank 20)
  (code 3.141592653589793)
  (blank 20)
  (code 4.2+5i)))

(slide #:title "The Numeric Tower"
       (scale (bitmap "tower1-2.jpg") .5))

(slide #:title "Unions of numbers"
       (para "Built of simple base types:")
       (blank 20)
       (code Zero Exact-Positive-Index)
       (code Negative-Float Float-Complex)
       (blank 20) 'next
       (para "Plus a lattice generated by" ((current-code-tt) "U"))
       (code (define-type Nonnegative-Integer 
               (U Zero Positive-Index 
                  Positive-Fixnum/Index Positive-Bignum))))

(define sqr-a (code (sqr a)))
(define sqr-b (code (sqr b)))

(define plus (code (+ #,sqr-a #,sqr-b)))

(slide/staged [one two three] #:title "Mathematical properties"
       (pict-case stage-name
         [(one)
          (code 
           (: pythagorean : Real Real → Nonnegative-Real)
           (define (pythagorean a b)
             (sqrt (+ #,sqr-a #,sqr-b))))]
         [(two)
          (let ()
            (define pos (code Positive-Real))
            (define pos* (launder pos))
            (connect 
             (connect 
              (mini-slide
               (code 
                (: pythagorean : Real Real → Nonnegative-Real)
                (define (pythagorean a b)
                  (sqrt #,plus)))
               (blank 100)
               (hbl-append 60 pos pos*))
              sqr-a pos 8)
             sqr-b pos* 8))]
         [(three)
          (let ()
            (define pos (code Positive-Real))
            (define pos* (launder pos))
             (connect 
              (mini-slide
               (code 
                (: pythagorean : Real Real → Nonnegative-Real)
                (define (pythagorean a b)
                  (sqrt #,plus)))
               (blank 100)
               (hbl-append 60 pos (ghost pos*)))
              plus pos 8))])
       )


(slide #:title "Occurrence typing"
       (code 
        (: abs : Real → Nonnegative-Real)
        (define (abs x) 
          (if (positive? x) x (- x))))
       (blank 100)
       (ghost (code > : (x:Integer y:Nonneg-Integer -> Bool
                  : Pos-Integer @ x))))

(define gt (code >))
(define gt* (code >))

(define gt-t (code (x:Integer y:Nonneg-Integer -> Bool
                     : Pos-Integer @ x)))

(slide/staged 
 [one two]
 #:title "Occurrence typing"
 (pict-case stage-name
   #:combine lt-superimpose
   [(one)
    (mini-slide
     (code 
      (: abs : Real → Nonnegative-Real)
      (define (abs x) 
        (if #,(red-code (#,gt x 0))|     | x (- x))))
     (blank 100)
     (ghost gt-t))]
   [(two)
    (connect 
     (mini-slide
      (code 
       (: abs : Real → Nonnegative-Real)
       (define (abs x) 
         (if #,(red-code (#,gt x 0))|     | x (- x))))
      (blank 100)
      gt-t)
     gt gt-t 5)])
 )

(define idx (t "Index computation"))
(define i1 (code i))
(define i2 (launder i1))
(define i3 (launder i1))

(define pic2 (mini-slide
             (code
              (: sum-vector : (Vectorof Integer) → Integer)
              (define (sum-vector v)
                (define n (vector-length v))
                (let loop ([i 0] [sum 0])
                  (if (< #,i1 n) 
                      (loop (+ #,i2 1) (+ sum (vector-ref v #,i3)))
                      sum))))
             (blank 50)
             (hbl-append idx (t " entirely in fixnums"))))

(slide/staged 
 [one two three]
 #:title "Fixnum Optimization"
 (pict-case stage-name
   [(one)
    (code
     (: sum-vector : (Vectorof Integer) → Integer)
     (define (sum-vector v)
       (define n (vector-length v))
       (let loop ([i 0] [sum 0])
         (if (< #,i1 n) 
             (loop (+ #,i2 1) (+ sum (vector-ref v #,i3)))
             sum))))]
   [(two) pic2]
   [(three)
    (connect
     (connect
      (connect
       pic2
       idx i1 6)
      idx i2 6)
     idx i3 6)]))




(slide
 #:title "Conclusions"
 (para "Unions are important for scripting languages")
 (para "Occurrence typing is an elimination rule for unions.") 
 (para (t "Propositions can") (it "relate") (t "types and terms"))
 )



;(thanks)


