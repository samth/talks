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

(title '("Typed Racket as a research agenda")
       '()
       '(("Sam Tobin-Hochstadt" "Indiana University"))
       "April 15, 2014    EPFL")
(set-page-numbers-visible! #t)
(do-start? #f)


(code-colorize-enabled #t)
;(slide (langs-pict #t))

(dynamic-require "intro.rkt" #f)

(require "class-slide.rkt")
(class-slide '(1 2 3 4))


(tslide* "Enter Typed Racket")

(class-slide '(5))


(define (titlet s)
  (t/quat s size2))

(pslide/title 
 "Typed Racket Goals"
 ;; FIXME: use `cascade` here, but needs lt instead of cc in bounding box
 #:go (coord 0.3 0.3)
 (shadow-frame (t "Typed sister language to Racket"))
 #:go (coord 0.45 0.5)
 (shadow-frame (t "Sound interoperability with untyped code"))
 #:go (coord 0.6 0.7)
 (shadow-frame (t "Easy porting of existing programs and idioms"))
 )


(tslide* "Typed Racket in 3 Slides")

(ts-intro)

(tslide* "Idiomatic Types"
         '("With Takikawa, Strickland, Felleisen"
           "[POPL 08, ESOP 09, ICFP 10, OOPSLA 12, ESOP 13]"))

(define narrow1 (t/cant "Racket"))
(define wide1 (t/cant "JavaScript"))
(define narrow2 (t/cant "Scala"))
(define wide2 (t/cant "Haskell"))
(define x-diff1 (- (pict-width wide1) (pict-width narrow1)))
(define x-diff2 (- (pict-width wide2) (pict-width narrow2)))

(slide/staged [one many after] #:title (titlet "How do Racket programmers think?")
       (hc-append
        (pict-case 
         stage-name #:combine cc-superimpose
         [(one) (t/cant "Racket")]
         [(many after) 
          (inset 
           (vc-append 
            (t/cant "Ruby")
            (t/cant "Python")
            (t/cant "Racket")
            (t/cant "JavaScript")
            (t/cant "Lua"))
           (/ x-diff1 -2) 0)])
        (t/cant " programs are not secretly ")
        (pict-case
         stage-name #:combine cc-superimpose
         [(one) (t/cant "Scala")]
         [(many after) 
          (inset 
           (vc-append (t/cant "Java")
                      (t/cant "ML")
                      (t/cant "Scala")
                      (t/cant "Haskell")
                      (t/cant "C++"))
           (/ x-diff2 -2))])
         (t/cant " programs")
        )
       (blank 30)
       (pict-case
        stage-name
        [(after)
         (t "Consider the native idioms of a language")]
        [else (blank)]))

(start)

(define forall (lift-above-baseline (code ∀) -3))

(slide/staged 
 [occur union varar #;local]
 #:title (titlet "Types for Racket Idioms")
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
     (define-type Peano (U 'Zero (List 'S Peano)))
     (: convert : Peano -> Number)
     (define (convert n)
       (cond [(symbol? n) 0]
             [else (add1 (convert (rest n)))])))])))

(define (esq-text)              
  (code
   (define-type Esq-Text%
     (Class #:implements Text%
            [new-prompt (-> Void)]
            [output (String -> Void)]
            [reset (-> Void)]))))
(define (mixin-ty)
  (code
   (: esq-mixin
      (All (r #:row)
           (-> (Class #:row-var r #:implements Text%)
               (Class #:row-var r #:implements Esq-Text%))))))

(define (mixin-impl)
  (code
   (code:comment "add REPL functions to `base-class`")
   (define (esq-mixin base-class)
     (class base-class
       (super-new)
       (inherit insert last-position get-text erase)
       ||
       (define/public (new-prompt) ...)
       (define/public (output s) ...)
       (define/public (reset) ...)))))

(slide #:title (titlet "Mixins in Racket")
       #:layout 'center
       (parameterize ([current-font-size 25])
         (smod #:name "racket-esq"
               (ghost (esq-text))
               (ghost (mixin-ty))
               (mixin-impl))))

(slide #:title (titlet "Mixins in Typed Racket")
       #:layout 'center
       (parameterize ([current-font-size 25])
         (tmod #:name "racket-esq"
               (esq-text)
               (mixin-ty)
               (mixin-impl))))


(require "peano.rkt" "combine.rkt")

(peano1) (combine1)
(peano2) ;(combine2)

(define t/dosis t/cant)

#;
(pslide/title
 "Mixins in the DrRacket IDE"
 #:go (coord 0.0 0.55 'lc)
 (parameterize ([current-font-size 25])
   (code
    (define drracket-frame%
      (online-expand-frame-mixin
       (frame-mixin
        (drracket:frame:mixin
         (drracket:frame:basics-mixin 
          (frame:size-pref-mixin
           (frame:searchable-text-mixin 
            (frame:searchable-mixin
             (frame:text-info-mixin 
              (frame:delegate-mixin
               (frame:status-line-mixin
                (frame:info-mixin
                 (frame:text-mixin
                  (frame:editor-mixin
                   (frame:standard-menus-mixin
                    (frame:register-group-mixin
                     (frame:focus-table-mixin
                      (frame:basic-mixin
                       frame%))))))))))))))))))))
 #:go (coord 0.5 0.45 'lc)
 (shadow-frame
  (vl-append
   (t/dosis "Layered development" 35)
   (t/dosis "DrRacket frame: 17 mixins" 35)
   (t/dosis "≥ 49 mixins in codebase" 35))))


;(start)
;; Occurrence Typing + Classes

(slide #:title (title-t "Lessons")
       (para "Existing idioms are a source of type system ideas")
       (blank 50)
       (para "Repeated in TypeScript, Typed Clojure, Hack, ..."))

(tslide* "Effective Contracts"
         '("With Takikawa, Strickland, Flatt, Findler, Felleisen"
           "[DLS 06, ESOP 13, OOPSLA 12]"))

(multi-sound)

(define big-addx
  (code
   (provide addx-c)
   (define (addx-c x)
     (if (number? x)
         (contract (addx x) (-> number? number?))
         (error "blame the client")))
   (define (addx x) (lambda (y) (+ x y)))))

(define server3
  (tmod #:name "server" #:sizeof big-addx
        (code
         (: addx (Number -> (Number -> Number)))
         ||
         (define (addx x) (lambda (y) (+ x y))))))


(slide #:title (titlet "Contracts for functions")
       server3)

(slide #:title (titlet "Contracts for functions")
       (smod #:name "server" #:sizeof big-addx
        (code
         (provide/contract 
          [addx (-> number? (-> number? number?))])
         (define (addx x) (lambda (y) (+ x y))))))

(slide #:title (titlet "Contracts for functions")
       (smod #:name "server"
             big-addx))

(slide/staged [one two]
 #:title (titlet "Contracts for vectors")
 (pict-case 
  stage-name
  [(one)
   (tmod #:name "server"
         (code
          (provide primes)
          (: primes : (Vectorof Integer))
          (define primes (vector 2 3 5 7 11))))]
  [(two)
   (smod #:name "server"
         (code
          (provide/contract
           [primes (vector/c integer?)])
          (define primes (vector 2 3 5 7 11))))]
  )
 (pict-case 
  stage-name
  [(one) (blank)]
  [(two) (hbl-append (t/cant "But how does ") (code vector/c) (t " work?"))]))
(start)

(slide/staged 
 [one two]
 #:title (titlet "Chaperones")
 (smod 
  #:name "vector/c"
  (pict-case 
  stage-name
  [(one) (code (chaperone-vector 
                primes
                (lambda (v i res) 
                  (unless (number? res) (error "blame"))
                  res)
                ...))]
  [(two) (code (chaperone-vector 
                primes
                (lambda (v i res) 
                  (unless (number? res) (error "blame"))
                  17)
                ...))]))
 (pict-case 
  stage-name
  [(one) (blank)]
  [(two) (t/cant "Is this ok?")])
)

(slide #:title (titlet "The Chaperone Invariant")
       (shadow-frame
        (para "A chaperoned value behaves like the original value, but with extra errors.")
        #:shadow-descent 10)
       )

(slide #:title (titlet "Chaperones vs Impersonators")
       #:layout 'center
       (para "Chaperones")
       (subitem "Less expressive")
       (subitem "Apply to more values")
       (blank 20)
       (para "Impersonators")
       (subitem "No invariants")
       (subitem "Only apply to mutable values"))

(pslide/title 
 "Further Extension"
 #:go (cascade 120 'auto)
 (shadow-frame (t "Classes, Mixins, Objects")
               #:shadow-descent 20)
 (shadow-frame (t "Delimited Continuations")
               #:shadow-descent 20)
 (shadow-frame (t "Abstract Data Types")
               #:shadow-descent 20)
 (shadow-frame (t "Channels and Events")
               #:shadow-descent 20))

(start)


(slide #:title (titlet "Lessons")
       (para "Proxy mechanisms must be expressive while respecting invariants")
       (blank 50)
       (para "Now applied in JavaScript proxies"))

;; Chaperones + Continuations + Classes

(dynamic-require "extensible.rkt" #f)

;; PLDI + PADL

(tslide* "And more ..."
         '("With St-Amour, Dimoulas, Felleisen"
           "[DLS 06, ESOP 12, OOPSLA 12]"))

;(slide #:title (titlet "Proof Techniques"))

    (slide/staged
     [one two three four]
     #:title (title-t "Proofs and Techniques")
     (pict-case
      stage-name       
       [(one) (para "If the program raises a contract error, the blame is not assigned to a typed module.")]
       [(two) (para "Well-typed modules can't get blamed.")]
       [(three) (mini-slide
                 (para "Allows local reasoning about typed modules, without changing untyped modules.")
                 (para "Choose how much static checking you want."))]
       [(four) (mini-slide
                (para "Closely connected to contract semantics")
                (para "Proved by showing that all communication is monitored"))]))
   
(require "oc.rkt")
(do-prng)

(tslide* "Future Challenges")

(define id-use
  (code (require/typed poly [id (All (a) a -> a)])
        (id 5)))

(slide #:title (titlet "Polymorphic Contracts")
       #:layout 'center
       (blank 30)
       (smod #:name "poly"  #:width (pict-width id-use)
             (code
              (define (id x) 
                (cond [(number? x)
                       (+ x 1)]
                      [else x]))))
       (tmod #:name "checked"
             id-use)
       (shadow-frame (t/cant "A clear error")))

(slide #:title (titlet "Polymorphic Contracts")
       #:layout 'center
       (blank 30)
       (smod #:name "poly" #:width (pict-width id-use)
             (code
              (define (id x) 
                (cond [(number? x)
                       (display x) x]
                      [else x]))))
       (tmod #:name "checked"
             id-use)
       (shadow-frame (t/cant "What should this do?")))

(pslide/title
 "New Compiler Techniques"
 #:go (coord .1 .1 'lt)
 (scale-to-fit (bitmap "pycket1.png") 1000 1200)
 #:next #:go (coord 0 .4 'lt)
 (scale-to-fit (bitmap "pycket-bench.png") (* 2/3 1400) (* 2/3 500))
 )

(start)

;(slide #:title (titlet "Stronger Type Systems"))


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

#|
(define (example-langs)
  (slide/staged 
   [racket slide web lazy scribble datalog typed]
   (pict-case stage-name
     [(racket) (code 
                | #lang racket| (code:comment "An echo server")
                (define listener (tcp-listen 12345))
                (define (run)
                  (define-values (in out) (tcp-accept listener))
                  (thread (λ () (copy-port in out)
                            (close-output-port out)))
                  (run))
                (run))]
     [(slide)
      (code | #lang slideshow|
            (slide #:title "Hello PLDI"
                   (item "Intro")
                   (item "Racket")
                   (item "Typed Racket")))]
     [(web) 
      (code | #lang web-server/insta|
            (code:comment "A simple web server")
            (define (start request)
              (response/xexpr
               '(html
                 (body "Hello PLDI")))))]
     [(lazy) 
      (code | #lang lazy|
            (code:comment "An infinite list:")
            (define fibs
              (list* 1 1 (map + fibs (cdr fibs))))
            
            (code:comment "Print the 1000th Fibonacci number:")
            (print (list-ref fibs 1000)))]
     [(scribble) (code | #lang scribble/base|
                       #,(as-comment "@; Generate a PDF or HTML document")
                       #,atsign title #,neg{#,(as-string "Bottles ---") #,atsign italic #,neg{#,(as-string "Abridged")}}
                       #,atsign(apply itemlist
                                      (for/list ([n (in-range 100 0 -1)])
                                        #,atsign item #,neg{#,atsign(format "~a" n) #,(as-string "bottles.")})))]
     [(datalog)
      (code | #lang datalog|
            #,(as-datalog "ancestor(A, B) :- parent(A, B).")
            #,(as-datalog "ancestor(A, B) :-")
            #,(as-datalog "  parent(A, C), D = C, ancestor(D, B).")
            #,(as-datalog "parent(john, douglas).")
            #,(as-datalog "parent(bob, john).")
            #,(as-datalog "ancestor(A, B)?"))]
     [(typed)
      (code | # lang typed/racket|
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
|#

(require unstable/gui/pict)

(define (wrap-up [thanks? #f])
  (slide  #:layout 'center
         (shadow-frame
          (vl-append
           (t "Typed Racket is not just a nice language")
           (t "... it's also informing PL research at every level"))
          #:shadow-descent 10)
         (blank 15)
         (t "Runtimes, proofs, compilers, metaprogramming, contracts")
         (blank 25)
         (show (scale (t/section "Thank you") 2) thanks?)
         (blank 15)
         (show ((current-code-tt) "samth.github.io              racket-lang.org") thanks?)))



(wrap-up)

(parameterize ([current-background-pict (bitmap plt-background-path)])
  (wrap-up #t))
