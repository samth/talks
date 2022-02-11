#lang slideshow

(require slideshow/step slideshow/code slideshow/face (only-in slideshow/slide title-size))

(require beamer/beamer
         "../dls06/utils.ss")



(current-keyword-list (list* "define:" "let:" "require/typed" "require/contract" ":" "->" 
                             "provide/contract" "..." "define-type-alias"
                             (current-keyword-list)))

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
   
(title "                Typed Scheme                ")
(author "Sam Tobin-Hochstadt" "" #f)
(affiliation "NU CCIS PhD Open House")

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
(slide #:title "The PL Renaissance"
       #:layout 'auto
       
       'alts
       (list
        (list
         (scale (bitmap "Sanzio_01.jpg") .85))
        (list
         (para "Programming languages are flourishing")
         (vc-append
          (hc-append 
           #;#;
           (bitmap "javascript-logo.png")
           (blank 20)
           (scale (bitmap "perl_logo.jpg") .5)
           (blank 20)
           (scale (bitmap "python-logo.png") .8))
          (blank 20)
          (hc-append (scale (bitmap "ruby-400.png") .5)
                     (blank 20)
                     (scale (bitmap "tcl.gif") .4)
                     (blank 20)
                     (bitmap "php.gif")
                     (blank 20)
                     (bitmap "lua.png"))))))

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

(slide #:title "What's not so good"
       (para "Always code as if the guy who ends up maintaining your code will be a violent psychopath who knows where you live.")
       (para "      - John F. Woods")       
       )

(define (class-code #:super [super #f])
  (define c
    (code 
     (code:comment "Start here:")
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


#;
(slide #:title "What's not so good"
       #:layout 'center
       (blank 10)
       'alts
       (list
        (list (class-code))
        (list (class-code #:super #t)))
       )
                                    
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

(tslide "What can Academia Teach Industry?")

(slide #:title "We Write Code Too"       
       (para "PLT Scheme contains over 600,000 lines of Scheme code")
       (para "And plenty of maintenence problems"))

(slide #:title "We Have Tools"
       (para "Type Systems")
       (para "Software Contracts")
       (para "Module Systems")
       (para "..."))

(slide #:title "The Challenge"
       (para "Applying the tools of the PL research community to the problems of today's languages"))



(define d1 (para "1. Module by module migration"))
(define d2 (para "2. Easy integration with untyped code"))
(define d3 (para "3. Sound guarantees from the type system"))
(define d4 (para "4. Avoid rewriting code"))



#|
2. Desiderata for Program evolution
 - Module by module
 - Soundness
 - No new language
 - Plays nicely with untyped code
 - Works with existing systems

|#



;(start-at-recent-slide)
(tslide "Typed Scheme")

(slide #:title "Goals"
       (para "Allow programmers to migrate programs from PLT Scheme to Typed Scheme")
       (para "Integrate with existing code")
       (para "Support native programming styles"))


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

(define t-sq (code (: sq (Number -> Number))
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


(slide #:title "Adding Types to PLT Scheme" #:layout 'center
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
              (tmod t-sq #:name "arith" #:sizeof sz))))

(define-syntax-rule (red-code . e)
  (parameterize ([code-colorize-enabled #f])
    (colorize (code . e) "red")))

;(start-at-recent-slide)

(define t-addx (code (: addx (Num -> (Num -> Num)))                
                     (define (addx x)
                       (lambda (y) (+ x y)))))
(define s-addx (code (define (addx x)
                       (lambda (y) (+ x y)))))        
        
(let*
    (
       [sz-long (code (require/typed addx (Num -> (Num -> Num)) addx))]
       [p0-t (tmod #:name "addx" #:sizeof sz-long
                   t-addx
                   (code (provide addx)))]
       
       [p0-s (smod #:name "addx" #:sizeof sz-long                   
                   (lb-superimpose (ghost t-addx) s-addx)
                   (code (provide addx)))]
       #;[p0 (tmod #:name "addx"
                 t-addx
                 (red-code (provide/contract 
                            [addx (-> Num? (-> Num Num?))])))]
       (p1 
        (vl-append
         (tmod #:name "arith" #:sizeof sz
               t-sq/prov)
         (blank 10)
         (smod #:name "run" #:sizeof sz
               (code (require a))
               (code (sq 11))
               (code (sq "eleven")))))     
       (p2 
        (vl-append
         (tmod #:name "arith" #:sizeof sz
               t-sq/prov)
         (blank 10)
         (smod #:name "run" #:sizeof sz
               (code (require a))
               (code (sq 11))
               (code (sq "eleven") (code:comment "=> contract violation")))))
       (p3
        (vl-append
         (smod #:name "arith" #:sizeof sz
               s-sq/prov)
         (blank 10)
         (tmod #:name "run" #:sizeof sz
               (code (require/typed sq (Num -> Num) a))
               (code (sq 11))
               (code (sq "eleven") (code:comment "=> type error")))))
       (p3*
        (vl-append
         (smod #:name "arith" #:sizeof sz
               s-sq/prov)
         (blank 10)
         (tmod #:name "run" #:sizeof sz
               (code (require/typed sq (Num -> Num) a))
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
  (slide #:title "Scaling to Larger Systems" #:layout 'center
         'alts
         (list 
          (list (para "Multi-module programs")
                (vl-append
                 (tmod #:name "arith" #:sizeof sz
                       t-sq/prov)
                 (blank 10)
                 (lt-superimpose (ghost (tmod #:sizeof sz (code 1
                                                                2
                                                                3)))
                                 (tmod #:name "run" #:sizeof sz
                                       (code (require a)
                                             (sq 11) (code:comment "=> 121"))))))
          (list 
           (para "Typed to Untyped")
           p1)          
          (list 
           (para "Typed to Untyped")
           p2)
          (list 
           (para "Untyped to Typed")
           p3*)
          
          (list 
           (para "Untyped to Typed")
           p3))))

#;
(let* (
       [sz-long (code (require/typed addx (Num -> (Num -> Num)) addx))]
       [p0-t (tmod #:name "addx" #:sizeof sz-long
                   t-addx
                   (code (provide addx)))]
       
       [p0-s (smod #:name "addx" #:sizeof sz-long                   
                   (lb-superimpose (ghost t-addx) s-addx)
                   (code (provide addx)))]
       #;[p0 (tmod #:name "addx"
                 t-addx
                 (red-code (provide/contract 
                            [addx (-> Num? (-> Num Num?))])))]
       (p1 
        (vl-append
         (tmod #:name "arith" #:sizeof sz
               t-sq/prov)
         (blank 10)
         (smod #:name "run" #:sizeof sz
               (code (require a))
               (code (sq 11))
               (code (sq "eleven")))))     
       (p2 
        (vl-append
         (tmod #:name "arith" #:sizeof sz
               t-sq/prov)
         (blank 10)
         (smod #:name "run" #:sizeof sz
               (code (require a))
               (code (sq 11))
               (code (sq "eleven") (code:comment "=> contract violation")))))
       (p3
        (vl-append
         (smod #:name "arith" #:sizeof sz
               s-sq/prov)
         (blank 10)
         (tmod #:name "run" #:sizeof sz
               (code (require/typed sq (Num -> Num) a))
               (code (sq 11))
               (code (sq "eleven") (code:comment "=> type error")))))
       (p3*
        (vl-append
         (smod #:name "arith" #:sizeof sz
               s-sq/prov)
         (blank 10)
         (tmod #:name "run" #:sizeof sz
               (code (require/typed sq (Num -> Num) a))
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
          (list
           (red-block d3)
           (blank 10)
           (para "Typed to Untyped - Higher Order")
           p3-2)
          (list
           (red-block d3)
           (blank 10)
           (para "Untyped to Typed - Higher Order")
           p3-3)))
    )

;(start-at-recent-slide)

(define (fn #:circle? [circle? (code circle?)]
            #:cr [cr (code (circle-radius s))]
            #:typed [typed #f])
  (code (shape-area s)
          (cond
            [(position? s) 0]
            [(#,circle? s) (* (sqr #,cr 2 ) pi)]
            ...)))

;(start-at-recent-slide)

(define (c f #:typed [typed #f])
  (code 
   #,(if typed          
         (code (define-type-alias Shape 
                 (U Position Circle Rectangle ...)))
         (code (code:comment "Shape = Position U Circle U Rectangle U ... ")
               ||))   
   #,(if typed (code (: shape-area  (Shape -> Num))) (code (code:contract | shape-area| : Shape -> Num)))
   (code:comment "what is the area of shape s?")
   #,f
   ))

;(start-at-recent-slide)

(slide #:title "A Type System for PLT Scheme"
       #:layout 'center
       'alts
       (append        
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
                      (blank 10))
           #;#;
           (smod (code (map rectangle-area
                            (filter rectangle? list-of-shapes))))
           (tmod (code (map rectangle-area
                            (filter rectangle? list-of-shapes)))))))
        
        ))

(slide #:title "A Type System for PLT Scheme"
       'alts
       (list
         (list
          ;(blank 10)
          (para "Polymorphism")
          (para "Union Types")
          (para "Recursive Types")
          (para "Structures")
          (colorize (para "Occurence Typing") "red"))))

;(start-at-recent-slide)

(tslide "Does it work?")


;(start-at-recent-slide)

(slide #:title "Validation"
       'alts
       (list 
        (list (para "Implemented in PLT Scheme")
              (subitem "Support all PLT Tools")
              (subitem "Integrates with Macro and Module system")
              (subitem "All standard libraries available"))
        (list (para "Ported 5000 lines of existing code and documented the results")
              (subitem "Games, scripts, libraries, educational code")
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

(slide #:title "Conclusions"
       (para "Interlanguage migration works!")
       (para "Need a type system designed for your language")
       'next
       (para "We hope to apply this to more code, and other languages"))

(slide #:layout 'center 
       (blank 40)
       (text "Thank You" (current-main-font) 50)
       (blank 60)
       (tt "http://www.ccs.neu.edu/~samth/typed-scheme")
       (blank 20)
       (parameterize ([current-font-size 24])
         (t "Thanks to Matthias Felleisen, Ryan Culpepper, Matthew Flatt, Ivan Gazeau"))
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


