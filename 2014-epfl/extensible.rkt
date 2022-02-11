#lang slideshow

(require slideshow/step slideshow/code slideshow/face 
         unstable/gui/ppict unstable/gui/pslide
         (only-in slideshow/slide title-size)
          "config.ss"
         (except-in "beamer.ss" title) "lib.ss" racket/gui  "thanks.ss" 
         "tslide.ss" lang-slide "contracts.rkt"
         "ts-intro.rkt" "stages.rkt"
         "helper.rkt"
         racket/runtime-path (except-in mzlib/etc identity) unstable/gui/slideshow)

(tslide* (vl-append (t/section "Extensible Languages,")
                    (t/section "Extensible Compilers"))
         '("With Culpepper, St-Amour, Flatt, Felleisen"
           "[SFP 08, PLDI 11]"))


(pslide/title 
 "The Traditional Approach to Compilers"
 #:go (coord 0.0 0.2 'lt)
 (bitmap "dragonbook.jpg")
 #:next
 #:go (coord 0.7 0.2 'ct)
 (t/cant "Produces impressive results" 40)
 10
 (scale (bitmap "gcc.jpg") .45))
                   

(slide/staged 
 [one two] #:title (title-t "The Macro Approach to Compilers")
 (hc-append 150
            (code 
             (define-syntax and 
               (syntax-parser
                [(_ e1 e2)
                 #'(if e1 e2 #false)])))
            (show (column 400
                          (vl-append 20
                                     (t/cant "Supports linguistic reuse" 40)
                                     (t "Scoping")
                                     (code if)
                                     (t "...")
                                     (t "Functions")
                                     (t "Classes")
                                     (t "Modules")                                     
                                     ))
                  (> stage 1))))

(parameterize (#;[current-font-size (+ 4 (current-font-size))])
 (slide #:title (title-t "The Typed Racket approach:")
  (blank 50)
  (para "Linguistic reuse of the macro approach")
  (subitem "Re-uses existing language extensions")
  (para "Capabilities of the traditional approach")
  (subitem "Including typechecker and optimizer")
  (blank 30)
  'next
  (para "By exposing compiler tools to library authors")))

(define ack1 (ack-def))
(define bigm (code (#,(red-code module-begin)
                    #,ack1
                    ||
                    (ack 2 3))))

(define (bod p) (pin-over (ghost bigm) ack1 lt-find p))






(slide/staged [one two three mb] #:title (pict-if (not (>= stage mb)) (title-t "Static Checking") (code module-begin))
       (pict-case stage-name
         [(one) (smod #:name "ack" 
                      (bod (code #,(ack-def #:typed #f)
                                 ||
                                 (ack 2 3))))]
         [(two) (tmod #:name "ack" #:lang (hbl-append (red-code typed/) (code racket))
                      (bod (code #,(ack-def)
                                 ||
                                 (ack 2 3))))]
         [(three) (tmod #:name "ack"
                        (bod (code #,(ack-def #:colon (red-code :) #:define (red-code define))
                                   ||
                                   (#,(red-code ack) 2 3))))]
         [(mb) (tmod #:name "ack"
                      bigm)])
       (pict-case stage-name
         [(one two) (t "")]
         [(three) (para (t "Type checking is a") (it "global") (t "process"))]
         [(mb) (t "Languages control the whole module")]))

(define tr-mod 
  (vl-append
   (red-block 
    (t "Module Semantics")
    (pict-if #t (code (define-syntax module-begin ...)) (t "Standard Functions")))
   (blank 20)
   (block (pict-if #t (t "Core Syntax") (code (define-syntax module-begin ...)))
          (code (define-syntax λ ...)))
   (blank 20)
   (block (pict-if #t (t "Standard Functions") (code (define-syntax module-begin ...)))
          (code (define + ...)))))

(slide #:title (title-t "Implementing a language")
       (smod #:name "typed/racket" #:lang (code racket) #:sizeof (inset bigm 0 -50)
             tr-mod))

(slide #:title (title-t "Implementing a language")
       (smod #:name "typed/racket" #:lang (code racket) #:sizeof (cc-superimpose (inset bigm 0 -50) tr-mod)
             (code
              (define-syntax module-begin
                (syntax-parser
                 [(_ forms ...)
                  #,(red-code (for ([form #'(forms ...)])
                                (typecheck form)))
                  ||
                  #'(forms ...)])))))

(slide #:title (title-t "The Typechecker")
       (smod #:name "typechecker" #:lang (code racket) #:sizeof (cc-superimpose (inset bigm 0 -50) tr-mod)
             (code (define (typecheck form)
                     (syntax-parse form
                       [v:identifier 
                        ...]
                       [(λ args body)
                        ...]
                       [(define v body)
                        ...]
                       #,(blank 30)
                       #,(t "... other syntactic forms ..."))))))

(slide #:title (title-t "Why Intermediate Languages?")
       (vr-append 10
        (t "“The compiler serves a broader set of programmers than")
        (t "it would if it only supported one source language”")
        (t "    — Chris Lattner")))

(slide #:title (title-t "Why Intermediate Languages?")
       (t "Most forms come from libraries")
       (ack-def #:define (red-code define) #:cond (red-code cond))
       'next
       (para "Also: pattern matching, keyword arguments, classes, loops, comprehensions, any many more")
       (subitem "Can't know static semantics ahead of time"))

(slide/staged [one] #:title (title-t "Core Racket") #:layout 'center
              
              (para "Racket defines a common subset that expansion targets")
              (blank 20)
              (code
               |expr ::=| identifier
               #,(ghost (code |expr ::=|)) (plain-lambda args expr)
               #,(ghost (code |expr ::=|)) (app expr ...+)
               #,(ghost (code |expr ::=|)) #,(t "...")
               #,(ghost (code |expr ::=|)) #,(t "a dozen core expressions"))
              (blank 20)
              (code
               |def ::=| expr
               #,(ghost (code |def ::=|)) (define-values ids expr)
               #,(ghost (code |def ::=|)) (require spec)
               #,(ghost (code |def ::=|)) #,(t "..."))
              )

(slide #:title (code local-expand)
       (smod #:name "typed/racket" #:lang (code racket) #:sizeof (cc-superimpose (inset bigm 0 -50) tr-mod)
             (code
              (define-syntax module-begin
                (syntax-parser
                 [(_ forms ...)
                  #,(code (define expanded-forms
                                #,(red-code (local-expand #'(forms ...)))))
                  (for ([form #,(red-code expanded-forms)])
                    (typecheck form))
                  ||
                  #,(red-code expanded-forms)])))))

(slide #:title (title-t "The Revised Typechecker") #:layout 'center
       (smod #:name "typechecker" #:lang (code racket) #:sizeof (cc-superimpose (inset bigm 0 -50) tr-mod)
             (code (define (typecheck form)
                     (syntax-parse form
                       [v:identifier 
                        ...]
                       [(#,(red-code plain-lambda) args body)
                        ...]
                       [(#,(red-code define-values) vs body)
                        ...]
                       #,(blank 30)
                       #,(t "... two dozen core forms ..."))))))


(slide #:title (title-t "Adding Optimization")
       (smod #:name "typed/racket" #:lang (code racket) #:sizeof (cc-superimpose (inset bigm 0 -50) tr-mod)
             (code
              (define-syntax module-begin
                (syntax-parser
                 [(_ forms ...)
                  #,(code (define expanded-forms
                                #,(code (local-expand #'(forms ...)))))
                  (for ([form #,(code expanded-forms)])
                    (typecheck form))
                  #,(red-code (define opt-forms (map optimize expanded-forms)))
                  #,(red-code opt-forms)])))))


(slide/staged 
 [one three]
 #:title (title-t "Adding Optimization")
 
 (pict-case stage-name
   [(one) (para "Problem: generic arithmetic is slow")]
   [(three) (para "Express guarantees as rewritings")])
 (pict-case 
     stage-name
   [(one) (code (: norm : Float Float -> Float)
                (define (norm x y)
                  (sqrt (+ (sqr x) (sqr y)))))]
   [(two) (code (: norm : Float Float -> Float)
                (define (norm x y)
                  (#,(red-code flsqrt) 
                   (fl+ (fl* x x) (fl* y y)))))]
   [(three) (code (: norm : Float Float -> Float)
                  (define (norm x y)
                    (#,(red-code unsafe-flsqrt) 
                     (#,(red-code unsafe-fl+) (#,(red-code unsafe-fl*) x x) 
                                              (#,(red-code unsafe-fl*) y y)))))])
 (pict-case stage-name
   [(three) (t "Low-level operations expose code generation to libraries")]
   [else (blank)]))


(slide #:title (title-t "Performance results")
 (bitmap "padl12/shootout-benchmarks.png")
 (bitmap "padl12/macro-benchmarks.png")
 (t "Smaller is better"))

(slide #:title (title-t "Lessons")
       (para "Language extensibility makes compilers into libraries")
       (blank 50)
       (para "See LMS, Scala macros, ..."))
