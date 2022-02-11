#lang slideshow

(require slideshow/step slideshow/code slideshow/face 
         (only-in slideshow/slide title-size)
         (except-in "beamer.ss" title) "lib.ss" racket/gui  "thanks.ss" 
         "tslide.ss" "config.ss" "langs/main.rkt"
         "ts-intro.rkt" "stages.rkt" unstable/gui/slideshow)

(provide (rename-out [go vulns]))

(define vulns (map symbol->string
                   '(VU#510208
                     VU#784855
                     VU#186131
                     VU#187297
                     VU#13145
                     VU#16532
                     VU#196945
                     VU#203611
                     VU#325431
                     VU#327633
                     VU#542971
                     VU#572183
                     VU#738331
                     VU#794580
                     VU#803539
                     VU#844360
                     VU#868916
                     VU#183657
                     VU#800113
                     VU#706148
                     VU#252735
                     VU#457875
                     VU#697164
                     VU#927905
                     VU#458659
                     VU#109475
                     VU#418861
                     VU#484649
                     VU#734644
                     VU#319331
                     VU#531342
                     VU#715973
                     VU#718460
                     VU#915404
                     VU#938617
                     VU#229595
                     VU#559980
                     VU#360341
                     VU#739123
                     VU#955777
                     VU#837744
                     VU#795694
                     VU#714121
                     VU#198355
                     VU#23495
                     VU#581682
                     VU#725188
                     VU#852283)))

(define unsafe 
  (map symbol->string '(VU#13145
                        VU#16532
                        VU#196945
                        VU#203611
                        VU#325431
                        VU#327633
                        VU#542971
                        VU#572183
                        VU#738331
                        VU#794580
                        VU#803539
                        VU#844360
                        VU#868916
                        VU#715973
                        VU#718460)))

(define bitsyntax
  (map symbol->string '(VU#531342
                        VU#183657
                        VU#198355
                        VU#23495
                        VU#581682
                        VU#725188
                        VU#852283)))

(define dns-spec
  (map symbol->string '(VU#800113
                        VU#458659
                        VU#109475
                        VU#418861
                        VU#484649
                        VU#734644
                        VU#360341
                        VU#457875
                        VU#837744)))

(define vulns* (shuffle vulns))



(define (go)
  (define (t* s) (lt-superimpose (t s) (ghost (t "Bit Syntax DSL"))))
  (define bs (block (t* "Bit Syntax DSL")))
  (define safe (block (t* "Safe Language")))
  (define con (block (t* "Contracts")))
  (define dsl (block (t* "DSLs for Policy")))
  (define dots (block (t* "...")))
  (slide/staged 
   [zero one #;one* two #;two* three three* four five six]
   #:title "DNS Vulnerabilities" #:layout 'center   
   (hc-append
    (pict-case stage-name
      [(one) (block (t "DNS Semantics"))]
      [(two two*) safe]
      [(three three*) (vl-append 10 safe bs)]
      [(four) (vl-append 10 safe bs con)]
      [(five) (vl-append 10 safe bs con dsl)]
      [(six) (vl-append 10 safe bs con dsl dots)]
      [else (blank)])
    (blank 20)
    (let () 
      (define (t* s)
        (cond 
          [(and (>= stage three*) (member s dns-spec)) (blank)]
          [(and (>= stage one) (member s dns-spec)) (colorize (t s) "gray")]
          [(and (>= stage three*) (member s unsafe)) (blank)]
          [(and (>= stage two) (member s unsafe)) (ghost (t s))]
          [(and (>= stage three*) (member s bitsyntax)) (blank)]
          [(and (>= stage three) (member s bitsyntax)) (ghost (t s))]
          [else (t s)]))
      (define (format-v n vlns [t* t*])
        (if (<= (length vlns) n)
            (apply vl-append (map t* vlns))
            (ht-append 40 (apply vl-append (map t* (take vlns n)))
                       (format-v n (drop vlns n)))))
      (pict-case stage-name #:combine ct-superimpose
        [(zero) (format-v 16 vulns* t)]
        [else (format-v 16 vulns* t*)])))
   ))
