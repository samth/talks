#lang slideshow

(require slideshow/step slideshow/code slideshow/face (only-in slideshow/slide title-size))

(require "../../../scheme/beamer/beamer.ss"
         "../dls06/utils.ss")



(current-keyword-list (list* "define:" "let:" "require/typed" "require/contract" ":" "→" 
                             "provide/contract" "..." "define-type-alias" "Refinement"
                             (current-keyword-list)))

;→

;∪

(set-page-numbers-visible! #f)

(define (tslide t)
  (slide #:layout 'center
                (text t (current-main-font) 50)))

(define (mk-itemize texts pics #:para [para para] #:block [block blockf]
                    #:overlay [overlay lt-superimpose])
  (define g-pics (map ghost (apply append (map (lambda (e) (if (list? e) e (list e))) pics))))
  (let loop ([seen-ts '()] [ts (map para texts)] [ps pics])
    (define (rest v)
      (append
        (map (lambda (p)
               (append (reverse seen-ts)
                       (list (car ts)) 
                       (map ghost (cdr ts))
                       (list (block (apply overlay p g-pics)))))
             v)
        (loop (cons (car ts) seen-ts)
              (cdr ts)
              (cdr ps))))
    (cond
      [(null? ts) null]
      [(list? (car ps)) (rest (car ps))]
      [else (rest (list (car ps)))])))

(define (mod/lang lang #:name [name #f] #:space [space #t]
                  #:sizeof [sz (blank 1)] . body)
  (define box
    (lt-superimpose
     (ghost sz)
     (apply
      vl-append 
      ((current-code-tt) (string-append "#lang " lang))
      (if space (cons (tt "") body) body))))
  (if name
      (rt-superimpose (frame ((current-code-tt) (string-append " " name " "))) box)
      box))

(define-syntax-rule (tmod . args)
  (parameterize ([current-block-background-color "lightgreen"])
    (block (mod/lang "typed-scheme   " . args))))

(define-syntax-rule (red-block . args)
  (parameterize ([current-block-background-color "Tomato"])
    (block . args)))

(define-syntax-rule (smod . args)
  (block (mod/lang "scheme         " . args)))
   
(title "The Design and Implementation of Typed Scheme")
(subtitle #f)
(author "Sam Tobin-Hochstadt and Matthias Felleisen" "Northeastern University" "")
;(affiliation "PLT @ Northeastern University")

#;
(with-steps (a b)
  (slide #:layout 'center
         (if (before? b)
             (titlet "The Design and Implementation of Typed Scheme")
             (titlet "Typing Untyped Languages"))
         (t "Sam Tobin-Hochstadt and Matthias Felleisen")
         (blank)
         (t "POPL 2008")))
  
(title-slide)


#|
NEW OUTLINE:

1. Renaissance in PL
 - But in Scripting Languages
   Describe SL success
 - Unfortunately, Scripting Languages have some problems
   Lack of Modularity
   Lack of Invariant Enforcement
   Lack of Migration Path
    - What can we do?
      Rewrite in ML - ha
      Type inference - failed
 - Fortunately, we have the tools to help them
   Program migration
   Scripting Language program evolves to use more static language
|#


;(tslide "The Challenge of Scripting Languages")

(with-steps (a b)
  (slide #:title "The PL Renaissance"
         #:layout (only a 'auto 'auto)
       
       (only a 
             (scale (bitmap "Sanzio_01.jpg") .85)
             'nothing)
       (only b
             (para "Programming languages are flourishing")
             'nothing)
       (only b
             (vc-append
              (hc-append (scale (bitmap "rhino50.jpg") .6)
                         (blank 20)
                         (scale (bitmap "perl_logo.jpg") .5)
                         (blank 20)
                         (scale (bitmap "python-logo.png") .5))
              (blank 20)
              (hc-append (scale (bitmap "ruby-400.png") .5)
                         (blank 20)
                         (scale (bitmap "tcl.gif") .4)
                         (blank 20)
                         (bitmap "php.gif")
                         (blank 20)
                         (bitmap "lua.png")))
             'nothing)))

;(start-at-recent-slide)

(slide #:title "What's good"
       (para "These languages are")
       
       (subitem "interactive")
       (subitem "designed for rapid development")
       (subitem "supported by an active community")
       (subitem "modular")
       (subitem "higher-order")
       (blank 10)
       ;'next
       (para "And they're exciting!"))

;(start-at-recent-slide)
#;
(slide #:title "What's not so good"
       (para "Always code as if the guy who ends up maintaining your code will be a violent psychopath who knows where you live.")
       (para "      - John F. Woods")       
       )

(define (class-code #:super [super #f] #:comment? [com? #f])
  (define c
    (code 
     #,(if com? (code (code:comment "Start here:")) (code ||))
     (define (main stx trace-flag super-expr 
                   deserialize-id-expr name-id
                   interface-exprs defn-and-exprs)
       #,(parameterize ([current-code-tt
                         (lambda (s) (text s (current-code-font) 6))])
           (hc-append (blank 120)
           (code
            (let-values ([(this-id) #'this-id]
                         [(the-obj) (datum->syntax (quote-syntax here) (gensym 'self))]
                         [(the-finder) (datum->syntax (quote-syntax here) (gensym 'find-self))])
              
              (let* ([def-ctx (syntax-local-make-definition-context)]
                     [localized-map (make-bound-identifier-mapping)]
                     [any-localized? #f]
                     [localize/set-flag (lambda (id)
                                          (let ([id2 (localize id)])
                                            (unless (eq? id id2)
                                              (set! any-localized? #t))
                                            id2))]
                     [bind-local-id (lambda (id)
                                      (let ([l (localize/set-flag id)])
                                        (syntax-local-bind-syntaxes (list id) #f def-ctx)
                                        (bound-identifier-mapping-put!
                                         localized-map
                                         id
                                         l)))]
                     [lookup-localize (lambda (id)
                                        (bound-identifier-mapping-get
                                         localized-map
                                         id
                                         (lambda ()
                                           ;; If internal & external names are distinguished,
                                           ;; we need to fall back to localize:
                                           (localize id))))])
                
                ;; ----- Expand definitions -----
                (let ([defn-and-exprs (expand-all-forms stx defn-and-exprs def-ctx bind-local-id)]
                      [bad (lambda (msg expr)
                             (raise-syntax-error #f msg stx expr))]
                      [class-name (if name-id
                                      (syntax-e name-id)
                                      (let ([s (syntax-local-infer-name stx)])
                                        (if (syntax? s)
                                            (syntax-e s)
                                            s)))])
                  
                  
                  ;; ------ Basic syntax checks -----
                  (for-each (lambda (stx)
                              (syntax-case stx (-init init-rest -field -init-field inherit-field
                                                      private public override augride
                                                      public-final override-final augment-final
                                                      pubment overment augment
                                                      rename-super inherit inherit/super inherit/inner rename-inner
                                                      inspect)
                                [(form orig idp ...)
                                 (and (identifier? (syntax form))
                                      (or (free-identifier=? (syntax form) (quote-syntax -init))
                                          (free-identifier=? (syntax form) (quote-syntax -init-field))))]))))))))))))
    (if super (cc-superimpose (titlet "+ 900 lines") c) c))

(slide #:title "What's not so good"
       #:layout 'center
       (blank 10)
       'alts
       (list
        ;(list (class-code))
        (list (class-code #:super #t))
        (list (class-code #:super #t #:comment? #t)))
       )
#;
(slide #:title "100,000 lines of Perl?"
       (para "So, you built your business around a script...")
       (para "What now?")
       (blank 10)
       'next
       (para "You need to:")
       (subitem "Improve maintainability")
       (subitem "Keep the system running")
       (subitem "Without a total rewrite")
       )

(slide #:title "Program Evolution"
       
       (para "How can we make our script evolve into a mature program?")
       (blank 10)
       (para "By adding statically checked design information piece by piece.")
       )

(define d1 (para "1. Module by module migration"))
(define d2 (para "2. Easy integration with untyped code"))
(define d3 (para "3. Sound guarantees from the type system"))
(define d4 (para "4. Avoid rewriting code"))

(slide #:title "What do we need"
       
      (red-block 
       d1
       (blank 5)
       d2
       (blank 5)
       d3
       (blank 5)
       d4)
      )

#|
2. Desiderata for Program evolution
 - Module by module
 - Soundness
 - No new language
 - Plays nicely with untyped code
 - Works with existing systems

|#


;; "our response"

;(start-at-recent-slide)
(tslide "Typed Scheme")

(slide #:title "Why PLT Scheme?" #:layout 'center
       (blank 10)
       'alts
       
       (list
        (list
         (para "PLT Scheme is a scripting language.")
         (subitem "Untyped, Modular, Built to script libraries")
         (blank 10)
         (para "We are facing the same dilemma.")
         (subitem "Lots of untyped code to maintain"))
        (list
         (para "PLT Scheme has advantages for implementing program evolution.")
         (subitem "Modules [Flatt 02]") 
         (subitem "Software Contracts [Findler 02]"))
        (list
         (para "Lessons from PLT Scheme apply elsewhere."))))

#|

3. Typed Scheme
 - Supports modular language specification
 - Soundness through contracts
   Emphasize HO
 - Occurrence Typing
 - Integration w/ untyped code: require/provide
 - Integrated with DrScheme, macros, etc

|#

;(start-at-recent-slide)

(define t-sq (code 
              (: sq (Num → Num))
              (define (sq x) 
                (* x x))))

(define t-sq/prov (code #,t-sq
                        (provide sq)))

(define s-sq (lb-superimpose (ghost t-sq) (code (define (sq x) 
                                                  (* x x)))))
(define s-sq/prov (code #,s-sq
                        (provide sq)))

(define hello (lt-superimpose (ghost t-sq) (code (printf "Hello World"))))

(define sz (code (sq "eleven") (code:comment "=> contract violation")))

(slide #:title "Modular migration" #:layout 'center
       (red-block d1)
       (blank 10)
       'alts
       (list 
        (list (para "Hello World in PLT Scheme")
              (smod #:name "print"  #:sizeof sz
               hello))
        (list (para "Hello World in Typed Scheme")
              (tmod #:name "print" #:sizeof sz
               hello))
        (list (para "Simple Arithmetic in PLT Scheme")
              (smod s-sq #:name "arith" #:sizeof sz))
        (list (para "Simple Arithmetic in Typed Scheme")
              (tmod t-sq #:name "arith" #:sizeof sz))
        (list (para "Multi-module programs")
              (vl-append
               (tmod #:name "arith" #:sizeof sz
                     t-sq/prov)
               (blank 10)
               (lt-superimpose (ghost (tmod #:sizeof sz (code 1
                                                              2
                                                              3)))
                               (tmod #:name "run" #:sizeof sz
                                     (code (require arith)
                                           (sq 11) (code:comment "=> 121")
                                           ||))))))
       )

(define-syntax-rule (red-code . e)
  (parameterize ([code-colorize-enabled #f])
    (colorize (code . e) "red")))

;(start-at-recent-slide)

(define t-addx (code 
                (: addx (Num → (Num → Num)))
                (define (addx x)
                  (lambda (y) (+ x y)))))
(define s-addx (code | |
                     (define (addx x)
                       (lambda (y) (+ x y)))))

(let* (
       [sz-long (code (require/typed addx (Num → (Num → Num)) addx))]
       [p0-t (tmod #:name "addx" #:sizeof sz-long
                   t-addx
                   (code (provide addx)))]
       
       [p0-s (smod #:name "addx" #:sizeof sz-long
                   s-addx
                   (code (provide addx)))]
       #;[p0 (tmod #:name "addx"
                 t-addx
                 (red-code (provide/contract 
                            [addx (→ Num? (→ Num Num?))])))]
       (p1 
        (vl-append
         (tmod #:name "arith" #:sizeof sz
               t-sq/prov)
         (blank 10)
         (smod #:name "run" #:sizeof sz
               (code (require arith))
               (code (sq 11))
               (code (sq "eleven")))))     
       (p2 
        (vl-append
         (tmod #:name "arith" #:sizeof sz
               t-sq/prov)
         (blank 10)
         (smod #:name "run" #:sizeof sz
               (code (require arith))
               (code (sq 11))
               (code (sq "eleven") (code:comment "=> contract violation")))))
       (p3
        (vl-append
         (smod #:name "arith" #:sizeof sz
               s-sq/prov)
         (blank 10)
         (tmod #:name "run" #:sizeof sz
               (code (require/typed sq (Num → Num) arith))
               (code (sq 11))
               (code (sq "eleven") (code:comment "=> type error")))))
       (p3*
        (vl-append
         (smod #:name "arith" #:sizeof sz
               s-sq/prov)
         (blank 10)
         (tmod #:name "run" #:sizeof sz
               (code (require/typed sq (Num → Num) arith))
               (code (sq 11))
               (code (sq "eleven") ))))
       #;(p3-0
        (vl-append
         (lt-superimpose p0-t (ghost p0))
         (tt "") 
         (smod #:name "runx"
                   (code ((addx 5) 6))
                   (code ((addx 5) "six")))))
       #;
       (p3-1
        (vl-append
         p0
         (tt "")
         (smod #:name "runx"
                   (code ((addx 5) 6))
                   (code ((addx 5) "six")))))
       (p3-2
        (vl-append
         p0-t
         (blank 10)
         (smod #:name "runx" #:sizeof sz-long
               (code (require addx))
               (code ((addx 5) 6))
               (code ((addx 5) "six") (code:comment "=> contract violation")))))
       (p3-3
        (vl-append
         p0-s
         (blank 10)
         (tmod #:name "runx" #:sizeof sz-long
               sz-long
               (code ((addx 5) 6))
               (code ((addx 5) "six") (code:comment "=> type error"))))))  
; (start-at-recent-slide)
  (slide #:title "Integration with plain PLT Scheme"  
         #:layout 'center
         'alts
         (list
          (list 
           (red-block d2)
           (blank 10)           
           (para "Typed to Untyped")
           p1)
          (list 
           (red-block d3)
           (blank 10)           
           (para "Typed to Untyped")
           p2)
          (list 
           (red-block d2)
           (blank 10)
           (para "Untyped to Typed")
           p3*)
          (list 
           (red-block d3)
           (blank 10)
           (para "Untyped to Typed")
           p3)
          #;
          (list
           (red-block d3)
           (blank 10)
           (para "Typed to Untyped - Higher Order")
           p3-2)
          #;
          (list
           (red-block d3)
           (blank 10)
           (para "Untyped to Typed - Higher Order")
           p3-3)))
    )

;(start-at-recent-slide)

(define (fn #:circle? [circle? (code circle?)]
            #:s [s (code s)]
            #:ps [ps (code s)]
            #:cs [cs (code s)]
            #:cr [cr (code (circle-radius s))]
            #:typed [typed #f]
            #:zero [zero (code 0)]
            #:crhs [crhs (code (* (sqr #,cr 2 ) pi))])
  (code (define (shape-area #,s)
          (cond
            [(position? #,ps) #,zero]
            [(#,circle? #,cs) #,crhs]
            ...))))

(define sa-type (code (: shape-area (Shape → Num))))

(define (c f #:typed [typed #f])
  (code 
   #,(if typed          
         (code (define-type-alias Shape 
                 (∪ Position Circle Rectangle ...)))
         (code (code:comment "Shape = Position ∪ Circle ∪ Rectangle ∪ ... ")
               ||))   
   #,(if typed sa-type (code (code:contract Shape → Num)))
   (code:comment "what is the area of shape s?")
   #,f
   ))

;(start-at-recent-slide)

(slide #:title "A Type System for PLT Scheme"
       #:layout 'center
       (red-block d4)
       (blank 10)
       'alts
       (append
        (list
         (list
          (para "PLT Scheme programmers do not write with any particular type system in mind.")
          (para "So Typed Scheme must capture their informal reasoning.")
          ))
        (mk-itemize #:block values #:overlay (lambda (a . b) a)
         (list (para "How PLT Scheme programmers reason"))
         (list
          (list
           (smod (c (fn)))
           (smod (c (fn #:cr (red-code (circle-radius s)))))
           (smod (c (fn #:cr (red-code (circle-radius s))
                        #:circle? (red-code circle?))))
           (vl-append (tmod (c (fn #:typed #t)
                               #:typed #t))
                      (blank 10)))))
        (list
         (list 
          (para "Occurrence Typing")
          (vl-append (tmod (c (fn #:typed #t)
                              #:typed #t))
                     (blank 10))))
        (values
         (map (λ (t f)
                (list 
                 (para "Occurrence Typing")
                 (vc-append
                  (vl-append (tmod 
                             sa-type
                             f)
                            (blank 10))
                  (if t 
                      (hc-append (code s : ||) t)
                      (blank 0)))))
              (list #f (code Shape) (code Shape) (code Position) (code Shape) (code Circle) (code Circle))
              (list (fn #:typed #t)
                    (fn #:typed #t #:s (red-code s))
                    (fn #:typed #t #:ps (red-code s))
                    (fn #:typed #t #:zero (red-code 0))
                    (fn #:typed #t #:cs (red-code s))
                    (fn #:typed #t #:crhs (red-code (* (sqr (circle-radius s) 2) pi)))
                    (fn #:typed #t #:cr (code (circle-radius #,(red-code s)))))))
        (list
         (list 
          (para "Occurrence Typing")
          (vc-append
           (vl-append (tmod 
                       sa-type
                       (fn #:typed #t #:circle? (red-code circle?)))
                      (blank 10))           
           (code circle? : (Any → Bool : Circle)))))
        (mk-itemize #:block values #:overlay (lambda (a . b) a)
         (list (para "How PLT Scheme programmers reason"))
         (list
          (list
           (smod (code (map rectangle-area
                            (filter rectangle? list-of-shapes))))
           (tmod (code (map rectangle-area
                            (filter rectangle? list-of-shapes))))
           (vc-append
            (tmod (code (map rectangle-area
                             (filter #,(red-code rectangle?) list-of-shapes))))
            (code rectangle? : (Any → Boolean : Rectangle)))
           (vc-append
            (tmod (code (map rectangle-area
                             (#,(red-code filter) rectangle? list-of-shapes))))
            (code filter : (∀ (a b) 
                              ((Listof a) (Any → Bool : b)
                               → (Listof b))))))))))

(slide #:title "A Type System for PLT Scheme"
       #:layout 'center
       (red-block d4)
       (blank 10)
       'alts        
        (list
         (list
          (blank 10)
          (para "Polymorphism")
          (para "Union Types")
          (para "Recursive Types")
          (para "Structures")
          (colorize (para "Occurrence Typing") "red")
          'next
          (colorize (para "Refinement Types") "red")
          (blank 100)))
        )




(define sql1
  (code (: sql-safe? (String → Bool))
        (define (sql-safe? s) ...)
        (: query ((Refinement sql-safe?) → Result))
        (define (query s)
          (string-append 
           "SELECT from Data where k = " s ";"))
        | |))

(slide #:title "Refinement Types"
       (red-block (t "Any predicate can define a type"))
       'alts
       (list
        (list
         (tmod #:name "SQL" #:sizeof sql1
               sql1
               ))
         (list
          (tmod #:name "SQL" #:sizeof sql1
               (code (: sql-safe? (String → Bool))
                     (define (sql-safe? s) ...)
                     (: check (String → (Refinement sql-safe?)))
                     | |
                     | |
                     | |
                     | |)))
         (list
          (tmod #:name "SQL" #:sizeof sql1
               (code (: sql-safe? (String → Bool))
                     (define (sql-safe? s) ...)
                     (: check (String → (Refinement sql-safe?)))
                     (define (check s)
                       (if (sql-safe? s)
                           s
                           (error "not safe"))))))
         (list
          (vc-append
           (tmod #:name "SQL" #:sizeof sql1
                 (code (: sql-safe? (String → Bool))
                       (define (sql-safe? s) ...)
                       (: check (String → (Refinement sql-safe?)))
                       (define (check s)
                         (if (#,(red-code sql-safe?) s)
                             s
                             (error "not safe")))))
           (code sql-safe? : (String -> Bool : (Refinement sql-safe?))))
          )
         (list
          (vc-append
           (tmod #:name "SQL" #:sizeof sql1
                 (code (: sql-safe? (String → Bool))
                       (define (sql-safe? s) ...)
                       (: check (String → (Refinement sql-safe?)))
                       (define (check s)
                         (if (#,(red-code sql-safe?) s)
                             s
                             (error "not safe")))))
           (code x : (String -> Bool : (Refinement x))))
          )))

;(start-at-recent-slide)

(tslide "Does it work?")

(slide #:title "Formal Validation"
       (para "We have a formal model of Typed Scheme")
       (subitem "Enjoys standard soundness properties")
       (para "We have an implementation of this model using PLT Redex")
       (subitem "500 lines")
       (para "We have a verified model in Isabelle/HOL")
       (subitem "5000 lines"))

;(start-at-recent-slide)

(slide #:title "Real Validation"
       'alts
       (list 
        (list (para "Implemented in PLT Scheme")
              (subitem "Support PLT Tools (Check Syntax, Debugger, etc)")
              (subitem "Integrates with macro and module system")
              (subitem "Standard libraries available"))
        (list 'alts
              (list ;(list (para "Ported 5000 lines of existing code"))
                    (list (para "Ported thousands lines of existing code")))
              (subitem "Games, scripts, libraries, educational code")
              (subitem "Even parts of DrScheme")
              (subitem "Not by the original author")
              (subitem "Very few changes to the code"))))


#|

4. Validation
 - Formal Validation
   Formal model
   Redex model
   Isabelle/HOL model
 - Real Validation
   Implementation
   Ported 5000 lines of code
   Real existing programs
   Mostly ported by Scheme newcomer
   Worked very well - give numbers

|#

;(start-at-recent-slide)

(tslide "Future Work")

#;
(slide #:title "Future work"
       (para "Using Typed Scheme as a target for inference")
       (para "Applying the lessons of Typed Scheme to other scripting languages"))

(define (subpara str)
  (hc-append (blank (* 2 gap-size))
                  (para #:width (- (current-para-width) (* 2 gap-size))
                        str)))

(slide #:title "Exploiting Soft Typing"
       (para "Soft typing attempted to typecheck untyped programs without programmer help.")
       #;(subpara "Fagan 92, Wright 94, Aiken 94, Flanagan 97, Meunier 06")
       (para "This project was hindered by complex type languages and unpredictable errors.")       
       (para "We believe that using Typed Scheme as a target will help."))

(slide #:title "Onward to JavaScript?"
       (para "Other scripting languages present new challenges.")
       (subitem "Object systems")
       (subitem "No contract systems")
       (subitem "Weak module systems")
       ;'next
       (para "And new opportunities."))

(slide #:layout 'center 
       (blank 40)
       (text "Thank You" (current-main-font) 50)
       (blank 60)
       (t "Implementation, Documentation")
       (tt "http://www.plt-scheme.org")
       (t "PLT Redex Model, Isabelle Model")
       (tt "http://www.ccs.neu.edu/~samth")
       (blank 20)
       (parameterize ([current-font-size 24])
         (t "Thanks to Ryan Culpepper, Matthew Flatt, Ivan Gazeau"))
       )

       

#|

5. End matter
 - Related work 1 - Formal
   Static analysis of untyped languages
    - lots
   Type system features
    - lots
 - Related work 2 - Design
   Strongtalk

 - Conclusions/Future work
   Works for Scheme
   Applying to other languages requires OO

|#


