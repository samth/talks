#lang slideshow

(require slideshow/step slideshow/code slideshow/face 
         (only-in slideshow/slide title-size)
         (except-in "beamer.ss" title) "lib.ss" racket/gui  "thanks.ss" 
         "tslide.ss" "config.ss" "langs/main.rkt"
         "ts-intro.rkt" "stages.rkt"
         racket/runtime-path (except-in mzlib/etc identity) unstable/gui/slideshow)

(title '("Languages as Libraries")
       '("or, implementing the next" "700 programming languages")
       `((,(vr-append 
            (colorize (text "Sam Tobin-Hochstadt" 'decorative small-text-size) "darkred")
            (text "Vincent St-Amour   Ryan Culpepper" 'decorative small-text-size)
            (text "Matthew Flatt   Matthias Felleisen" 'decorative small-text-size)
            (blank 10))
          "PLT @ Northeastern & Utah"))
       "June 6, 2011    PLDI")
(set-page-numbers-visible! #f)
(do-start? #f)


(code-colorize-enabled #t)
(langs)

(define (hl p) (cc-superimpose (inset (cellophane (colorize (filled-rectangle (pict-width p) (pict-height p)) "pink") .7) -20)
                               p))

(define (as-string s)
  (colorize ((current-code-tt) s) literal-color))
(define (as-comment s)
  (colorize ((current-code-tt) s) comment-color))

(define (as-paren s)
  (colorize ((current-code-tt) s) keyword-color))

(define atsign
  (inset (as-paren "@") 0 0 (- (pict-width (code | |))) 0))

(define neg
  (inset (as-paren "") 0 0 (* 2 (- (pict-width (code | |)))) 0))

(define (as-datalog s)
  (apply hbl-append
         (for/list ([c s])
           (case c
             [(#\( #\) #\: #\- #\, #\. #\= #\?) (colorize ((current-code-tt) (string c)) keyword-color)]
             [else (colorize ((current-code-tt) (string c)) id-color)]))))

(define (example-langs)
  (slide/staged 
   [racket slide web lazy scribble datalog typed]
   (pict-case stage-name
     [(racket) (code 
                |#lang racket| (code:comment "An echo server")
                (define listener (tcp-listen 12345))
                (define (run)
                  (define-values (in out) (tcp-accept listener))
                  (thread (λ () (copy-port in out)
                            (close-output-port out)))
                  (run))
                (run))]
     [(slide)
      (code |#lang slideshow|
            (slide #:title "Hello PLDI"
                   (item "Intro")
                   (item "Racket")
                   (item "Typed Racket")))]
     [(web) 
      (code |#lang web-server/insta|
            (code:comment "A simple web server")
            (define (start request)
              (response/xexpr
               '(html
                 (body "Hello PLDI")))))]
     [(lazy) 
      (code |#lang lazy|
            (code:comment "An infinite list:")
            (define fibs
              (list* 1 1 (map + fibs (cdr fibs))))
            
            (code:comment "Print the 1000th Fibonacci number:")
            (print (list-ref fibs 1000)))]
     [(scribble) (code |#lang scribble/base|
                       #,(as-comment "@; Generate a PDF or HTML document")
                       #,atsign title #,neg{#,(as-string "Bottles ---") #,atsign italic #,neg{#,(as-string "Abridged")}}
                       #,atsign(apply itemlist
                                      (for/list ([n (in-range 100 0 -1)])
                                        #,atsign item #,neg{#,atsign(format "~a" n) #,(as-string "bottles.")})))]
     [(datalog)
      (code |#lang datalog|
            #,(as-datalog "ancestor(A, B) :- parent(A, B).")
            #,(as-datalog "ancestor(A, B) :-")
            #,(as-datalog "  parent(A, C), D = C, ancestor(D, B).")
            #,(as-datalog "parent(john, douglas).")
            #,(as-datalog "parent(bob, john).")
            #,(as-datalog "ancestor(A, B)?"))]
     [(typed)
      (code |#lang typed/racket|
            (struct: person ([first : String]
                             [|last | : String]))
            (: greeting (person -> String))
            (define (greeting n)
              (format "~a ~a"
                      (person-first n) (person-last n)))
            (greeting (make-person "Bob" "Smith")))])
   (pict-case stage-name #:combine cc-superimpose
     [(typed) (t "Racket with static types and full interoperation")]
     [(scribble) (t "A domain-specific language (and syntax) for documentation")]
     [(datalog) (t "Integrated logic programming")]
     [(racket) (t "A modern programming language")]
     [(web) (t "A language for writing web servers")]
     [(lazy) (t "Lazy evaluation")])))

#;
(example-langs)

#;(tslide "Macros Matter")

#;
(slide/staged
 [ex ty]
 #:title "All built with macros"
 #:layout 'center
 (ht-append 50
            (column 
             400
             (mini-slide
              (item "Comprehensions")
              (item "Recursive Modules")
              (item "Logic Programming")
              (item "Classes, Mixins, Traits")
              (item "Generic Methods a la CLOS")
              (item "Documentation")))
            (column 
             300
             (mini-slide
              (item "Contracts")
              (item "Lazy Programming")
              (item "Web Programming")
              (item "Lexing + Parsing")
              (item "Teaching")
              (pict-if (= stage 2) (colorize (item "Typechecking") "red") (blank))))))


(slide (titlet "How can we build so many languages?"))
(start)

(slide/staged 
 [one two] #:title "The Traditional Approach"
 (hc-append 150
            (bitmap "dragonbook.jpg")
            (column 400 (show
                         (mini-slide
                          (titlet "Produces impressive results")
                          (scale (bitmap "gcc.jpg") .45))
                         (> stage 1)))))

(slide/staged 
 [one two] #:title "The Macro Approach"
 (hc-append 150
            (code 
             (define-syntax and 
               (syntax-parser
                [(_ e1 e2)
                 #'(if e1 e2 #false)])))
            (show (column 400
                          (vl-append 20
                                     (titlet "Supports linguistic reuse")
                                     (t "Scoping")
                                     (code if)
                                     (t "...")
                                     (t "Functions")
                                     (t "Classes")
                                     (t "Modules")                                     
                                     ))
                  (> stage 1))))

(parameterize ([current-font-size (+ 4 (current-font-size))])
 (slide
  (para (titlet "Our approach:"))
  (blank 50)
  (para "Linguistic reuse of the macro approach")
  (para "Capabilities of the traditional approach")
  (blank 30)
  'next
  (para "By exposing compiler tools to library authors")))

(tslide "Providing the tools")

(slide/staged
 [intro control scrbl link focus]
 ((case stage-name    
    [(link) vr-append]
    [(scrbl) vl-append]
    [else vc-append])
   20
   (stages (list (= stage scrbl) (= stage focus) (= stage focus) (= stage focus) (= stage link)))
   
   (case stage-name
     [(control)(t "Language authors control each stage")]
     [(scrbl) (t "[Flatt et al, 2009]")]
     [(link) (t "In the paper")]
     [(focus) (t "Illustrated by Typed Racket")]
     [else (t "")])
 ))


(start)
(tslide (scale (static #f) 2))

(define ack1 (ack-def))
(define bigm (code (#,(red-code module-begin)
                    #,ack1
                    ||
                    (ack 2 3))))

(define (bod p) (pin-over (ghost bigm) ack1 lt-find p))

(slide/staged [one two three mb] #:title (pict-if (not (>= stage mb)) (titlet "Static Checking") (code module-begin))
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

(slide #:title "Implementing a language"
       (smod #:name "typed/racket" #:lang (code racket) #:sizeof (inset bigm 0 -50)
             tr-mod))

(slide #:title "Implementing a language"
       (smod #:name "typed/racket" #:lang (code racket) #:sizeof (cc-superimpose (inset bigm 0 -50) tr-mod)
             (code
              (define-syntax module-begin
                (syntax-parser
                 [(_ forms ...)
                  #,(red-code (for ([form #'(forms ...)])
                                (typecheck form)))
                  ||
                  #'(forms ...)])))))

(slide #:title "The Typechecker"
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
(start)

(tslide (scale (il #f) 2))

(slide #:title "Why Intermediate Languages?"
       (vr-append 10
        (t "“The compiler serves a broader set of programmers than")
        (t "it would if it only supported one source language”")
        (t "    — Chris Lattner")))

(slide #:title "Why Intermediate Languages?"
       (t "Most forms come from libraries")
       (ack-def #:define (red-code define) #:cond (red-code cond))
       'next
       (para "Also: pattern matching, keyword arguments, classes, loops, comprehensions, any many more")
       (subitem "Can't know static semantics ahead of time"))

(slide/staged [one] #:title "Core Racket" #:layout 'center
              
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

(slide #:title "The Revised Typechecker" #:layout 'center
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
                       #,(t "... two dozen core forms ...")))))
       ;'next
       (t "Communication between levels — see paper"))

(start)

(tslide (scale (codegen #f) 2))





(define mmod  
  (code (module ack typed/racket
          || 
          ||
          #,(ack-def)             
          ||
          (ack 2 3))))

(define tr (launder (code typed/racket)))
(define #%mb (launder (code module-begin)))
(define mmod2  
  (tmod #:name "ack" #:lang tr
   (code
    (#,#%mb           
     #,(ack-def)             
     ||
     (ack 2 3)))))



(define (big-mod1 hl?)
  (define f (code f))
  (define hf (highlight f))
  
  (define c
    (code
     (define-syntax (module-begin stx)
       (syntax-parse stx
         [(_ forms ...)
          (for ([f #'(forms ...)])
            (typecheck #,f))
          ||
          #'(core-module-begin forms ...)]))))
  (if hl? (highlight-on c f) c))

(define (big-mod2)
  (define unsyntax #f)
  (define c
    (code
     (define-syntax (module-begin stx)
       (syntax-parse stx
         [(_ forms ...)
          (define forms* (local-expand #'(forms ...)))
          (for ([f forms*]) (typecheck f))
          ||
          ||
          #'(core-module-begin #,forms*)]))))
  c)

(define (big-mod3)
  (define unsyntax #f)
  (define c
    (code
     (define-syntax (module-begin stx)
       (syntax-parse stx
         [(_ forms ...)
          (define forms* (local-expand #'(forms ...)))
          (for ([f forms*]) (typecheck f))
          ||
          (define forms** (optimize forms*))
          #'(core-module-begin #,forms**)]))))
  c)

(require (for-syntax syntax/parse racket/syntax racket/base) racket/stxparam)

(define-syntax (staged/def stx)
  (syntax-parse stx
    [(staged/def [nm ...] . body)
     #`(begin #,@(for/list ([n (syntax->list #'(nm ...))]
                            [i (in-naturals 1)])
                   #`(define (#,(format-id n "slide-~a" n))
                       (staged [#,n] (syntax-parameterize 
                                      ([stage (lambda (s) (datum->syntax #'here #,i))])
                                      . body)))))]))

(staged/def
 [mac mac* def-three def-four]
 (slide 
  #:title "Defining A Language"
  (pict-case stage-name
    [(mac) (big-mod1 #f)]
    [(mac*) (big-mod1 #t)]
    [(def-three) (big-mod2)]
    [(def-four) (big-mod3)])))



(staged/def [one two three]
        (slide #:title "Typechecking"
               (code 
                (define (typecheck f)
                  (syntax-parse f
                    (code:comment "variables")
                    [v:identifier
                     (#,(if (= stage 2) (red-code lookup-type) (code lookup-type)) #'v)]
                    (code:comment "abstractions")
                    [(lambda (x) e)
                     (define t #,(if (= stage 3) (red-code (syntax-property #'x 'type-label)) (code (syntax-property #'x 'type-label))))
                     (#,(if (= stage 2) (red-code set-type!) (code set-type!)) #'x t)
                     (typecheck #'e)]
                    (code:comment "about 10 more cases")
                    ....)))
               (pict-case stage-name
                 [(three) (para "Syntax properties provide side-channels")]
                 [else (blank)])))
#|
(slide-mac)
(slide-one)
(slide-two)
(slide-mac*)
|#
#;
(slide #:title (hbl-append (code local-expand) (make-title-text " "))
       (para "Core forms support arbitrary macros")
       (make-red (ack-def) ack-define ack-cond)
       (t "Discover static semantics by expansion"))

#|
(slide-def-three)

(slide-three)

(slide-def-three)

(slide-def-four)
|# 

(slide/staged 
 [one three]
 #:title "Code generation"
 
 (pict-case stage-name
   [(one) (para "Problem: optimizing generic arithmetic")]
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


(slide #:title "Results" #:layout 'center
       (scale (bitmap "benchmarks2.png") .5))

#|
(staged
 [one two three]
 (define mb1 (code module-begin))
 (define mb2 (code module-begin))
 (define def
   (code
    (define-syntax (#,mb1 stx)
      ....)))
 (define tr-mod
   (transparent-block
    (mod/lang "racket         " #:name "typed/racket" #:name-frame (name-back "red")
              def)))
 
 (define main
   (transparent-block; #:size-of def
    (pict-case stage-name
      [(one)
       (mod/lang "typed/racket   " #:name "name" #:name-frame (name-back "blue")
                 (ltl-superimpose
                  (ghost def)
                  (code ....)))]
      [(two three)
       (code
        (module name typed/racket
          #,(ltl-superimpose
             (ghost def)
             (code (#,mb2
                    ....)))))]))) 
 (slide
  #:title "Languages as Libraries"
  (pict-case stage-name
    [(one two) (mini-slide (ghost tr-mod) main)]      
    [(three) (connect (mini-slide tr-mod main) mb1 mb2)])))




|#

(define (wrap-up [thanks? #f])
  (slide #:title "The take-away"
         (item "Languages are powerful abstractions")
         (item "Racket enables full-scale languages as libraries")
         (item "Key idea: expose compiler pipeline to language authors")
         (blank 40)
         (show (scale (titlet "Thank you") 2) thanks?)
         (show ((current-code-tt) "racket-lang.org") thanks?)))



(wrap-up)

(parameterize ([current-background-pict (bitmap plt-background-path)])
  (wrap-up #t))







