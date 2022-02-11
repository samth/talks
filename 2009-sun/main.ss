#lang slideshow

(require slideshow/step slideshow/code slideshow/face (only-in slideshow/slide title-size)
         (except-in "beamer.ss" title) "lib.ss" scheme/gui "ts-intro.ss"
         scheme/runtime-path mzlib/etc (planet cce/scheme:6/slideshow))

;; setup code
(current-keyword-list (list* "define:" "let:" "require/typed" "require/contract" ":" "→"  "∪" "∀" "." "=" "Any"
                             "else" "provide/contract" "..." "define-type-alias" "Refinement"
                             "->" "Number" "String" "Bool" "Number,"
                             (current-keyword-list)))
(set-page-numbers-visible! #f)
(code-scripts-enabled #t)

(define pnf (send the-font-list find-or-create-font 16 'default 'normal 'normal))
(current-page-number-font pnf)

(current-title-font " Nimbus Roman No9 L")
(current-main-font " Nimbus Sans L")
(current-code-font " Inconsolata")

(define dark? #f)

(current-background-pict (if #f #;printing?
                             (blank 1024 768)
                             (if dark?
                                 (bitmap plt-dark-background-path)
                                 (bitmap plt-background-path))))
(current-title-background-pict (bitmap gradient-path))
(current-base-color "black")


(title '("Typed Scheme")
       '("From Scripts to Programs")
       '(("Sam Tobin-Hochstadt" "Northeastern University")))
(set-page-numbers-visible! #t)

#|
NEW OUTLINE:
|#

#|
- Motivation
  - Scripting Languages are Popular
    - Ruby - used to write twitter
    - Lisp - used to write viaweb
    - JavaScript - used to write the web
    - Perl - used to write swedish pension system
    - Python, Lua, PHP, etc ...
  - Most metrics place these languages as 5 of the top 10 most popular
  - What's good about these languages
    - even compared to Java/C#/etc
    - no 'finger typing'
    - run it now
    - reason how you want
    - DWIM
|#

(define-runtime-path here ".")
(define (i bmp [sc 1])
  (let ([bmp* (if (string? bmp)
                  (bitmap (build-path here bmp))
                  bmp)])
    (cc-superimpose  (scale bmp* sc))))

(define ff-bmp (i "firefox.png" .8))
(define twitter-bmp (i "twitter_logo.gif" .5))
(define swe-bmp (i "Sweden_map.jpg" .75))


(define (g . args)
  (ghost (apply i args)))

(define (do-img i j)
  (list 'alts~ (list (list i) (list (vc-append i j)))))



(define (rest-imgs i1 i2 i3 i4 i5)
  (list (vc-append (hc-append (i1 "groovy-logo2.png" .8)
                              (i2 "lua.png"))
                   (blank 30 30)
                   (hc-append (i3 "tcl.gif" .5)
                              (blank 30 30)
                              (i4 "php.gif" 1.5)
                              (blank 30 30)
                              (i5 "vb-logo.png" 2.5)))))

(slide #:title "The Rise of Scripting Languages"
       #:layout 'center
       'alts
       (let ([both 
              (hc-append swe-bmp (blank 30 30) (i "perl-logo.png"))])
         (list
          (list (cc-superimpose
                 (text "Scripting languages are popular!" (current-main-font) title-text-size)
                 (ghost both)))
          (do-img (i twitter-bmp)
                  (i "ruby-400.png" .7))
          
          (do-img (i ff-bmp)
                  (i "rhino50.png" .9))

          (list (i "reddit.png"))
          (list (i "reddit.png")
                (i "CMUCL.png" 2))
          
          (list (i "reddit.png")
                (i "python-logo.png"))

          #;
          (do-img (i "reddit.png")
                  (i "CMUCL.png"))
          
          #;(do-img (i "viaweb_logo.gif" 1.5)
                  (i "clisp-logo.png" 1.2))
          
          (list (cc-superimpose (i "ppm-logo.gif" 3)
                                (ghost both)))
          (list swe-bmp)
          (list both)
          
          (list
           'alts~
           (list
            (rest-imgs i g g g g)
            (rest-imgs i i g g g)
            (rest-imgs i i i g g)
            (rest-imgs i i i i g)
            (rest-imgs i i i i i))))))

(define (benifits-list v)
  (list
   (para v)
   (subpara "Hack something together")
   (subpara "Test out new ideas")
   (subpara "Change just one thing")
   (subpara "Avoid making decisions")))

(tslide "Program Evolution")

;; not the empty string!
(define rarrow "⇒")


(let* ([scheme-code 
        (code (define username 
                (seventh (read data-file))))]
       [scheme-code2 
        (code (define username
                (seventh (read untrusted-data-file))))]
       [python-code (vl-append
                     (codett "def print_elems(x):")
                     (codett "  for i in x:")
                     (codett"    print x"))]
       [python-code2 (vl-append
                      (codett "def print_elems(x):")
                      (codett "  for i in x:")
                      (codett"    complicated_printing x"))]
       [js-code        
        (vl-append
         (codett "// not fixed yet")
         (codett "function auxiliary(a,b) {")
         (hc-append (blank 20 20) (cloud 100 30))
         (codett "}"))]
       [js-code2
        (vl-append
         (codett "// is this fixed yet? - joe 9/2005")
         (codett "function auxiliary(a,b) {")
         (hc-append (blank 20 20) (cloud 100 30))
         (codett "}"))]
       [perl-code 
        (vl-append
         (codett "sub display_str {")
         (codett "  \"name and age \" , $1 , $2")
         (codett "}"))]
       [perl-code2 
        (vl-append
         (codett "sub display_str {")
         (codett "  \"name and age \" , $1 + $2")
         (codett "}")
         (codett "display_val (\"nancy\", 7)"))]
       [tblock (lambda (e)
                 (transparent-block
                  (lt-superimpose (ghost scheme-code2) (ghost perl-code2) (ghost js-code) e)))])
  
  (define paras
    (list (para "No compiler errors")
          (para "No type annotations")
          (para "Refactor as needed")
          (para "Flexible semantics")))
  (define (mk-alts txt codes)
    (cons (benifits-list txt)
          (map list paras (map tblock codes))))
  
  (slide #:title "Scripts"
         'alts
         (mk-alts "Easy to:"
          (list scheme-code python-code js-code perl-code)))
  (slide #:title "Programs"
         'alts
         (mk-alts "Too easy to:"
          (list scheme-code2 python-code2 js-code2 perl-code2))))

(slide #:title "Rewrite it all!"
       #:layout 'center
       (para "All too common")
       (blank 20)
       'alts
       (list
        (list (i twitter-bmp .7)
              (i "viaweb_logo.gif" (* .7 1.5)))))

(slide #:title "The Baby and The Bathwater"
       
       (para "They did it by making the single worst strategic mistake"
             " that any software company can make:"
             " They decided to rewrite the code from scratch.")
       (t "                     -- Joel Spolsky")
       (blank 30))

(slide #:title "Instead ..."
       (para "When scripts grow up, their languages should grow with them.")
       (blank 10)
       'next
       (para "Scripts grow into maintainable programs by incremental addition of sound static information."))

(slide #:title "The Recipe"
       (para "Allow types and typechecking to be added per-module")
       (blank 10)
       (para "Protect typed modules from untyped ones")
       (blank 10)
       (para "Devise a type system for existing idioms"))



#|

(define (grid arg)
  (define (mk-one typed?)
    (colorize
     (rounded-rectangle 120 100)
     (if typed? "blue" "red")))
  (apply tabular (map (lambda (e) (map mk-one e)) arg)))
(define (chunk l)
  (list (take l 4)
        (take (drop l 4) 4)
        (take (drop l 8) 4)))
(start)
#;
(slide #:title "Incremental Enrichment"
       (para "A module at a time")
       'alts
       (let-values ([(res acc)
                     (for/fold ([res null]
                                [acc (build-list 12 (lambda _ #f))])
                       ([i (in-range 6)])
                       (display res) (newline)
                       (let* ([cnt 0]
                              [new-l
                               (map (lambda (v) (or v (and (< cnt 3)
                                                           (= 2 (random 5))
                                                           (set! cnt (add1 cnt))))) acc)])
                         (values (cons (list (grid (chunk acc))) res) new-l)))])
         (reverse res)))
|#

#|
  - What's bad about these languages
    - maintainability
    - predicability
    - refactorability
    - comprehensability
  - What causes these problems?
    - The same things that made them nice in the first place!
    - flexible semantics - hard to predict
    - no types - hard to remember
    - no reasoning - hard to change
    - no compiler errors - hard to refactor
  - Baby and Bathwater
    - we want maintainability
    - not a complete rewrite
  - But complete rewrites are all too common
    - ViaWeb
    - Twitter
    - too many others to name
  - What to do instead?
    - Incremental refactoring
    - Type Enrichment
    - Typed Scheme!
  - What do you get?
    - Static guarantees for reasoning and maintaining
    - Without too much work
|#

;(start)

(ts-intro)



(define server
  (code (define (add5 x) (+ x 5))
        (provide/contract 
         [add5 (number? |.| -> |.| number?)])))
(define server2
  (code (define (add5 x) "x plus 5")
        (provide/contract 
         [add5 (number? |.| -> |.| number?)])))
(define (client2 #:comment [comment #f])
  (code (require server)
        #,(code (add5 "seven")) 
        #,(if comment comment (code (code:comment "=> contract error")))))
(define (client3-fun #:comment [comment #t])
  (code (require server)
        #,(code (add5 "seven")) #,(if comment (code (code:comment "=> client broke the contract   ")) (code))))
(define client3 (client3-fun))


(define ho-server1
  (code (define ((add-blaster x) y)
          (+ x y))
        (provide/contract
         [add-blaster 
          (number? |.| -> |.| (number? |.| -> |.| number?))])))

(define ho-server2
  (code (define ((add-blaster x) y)
          "Not a number")
        (provide/contract
         [add-blaster 
          (number? |.| -> |.| (number? |.| -> |.| number?))])))

(define ho-client1
  (code (require server)
        ((add-blaster 1) "two") (code:comment "=> client broke the contract")))

(define ho-client2
  (code (require server)
        ((add-blaster 1) 2) (code:comment "=> server broke the contract")))

(define (contracts/blame)
  (begin-with-definitions
    (tslide "Contracts and Blame")
    
    (slide #:title "Contracts"
           (para "A contract is dynamically checked property")
           ;'next
           'alts
           
           (list
            (list
             (smod #:name "server" #:sizeof client3
                   server)
             (smod #:name "client" #:sizeof client3
                   (code (require server)
                         #,(code (add5 7)))))
            (list
             (smod #:name "server" #:sizeof client3
                   server)
             (smod #:name "client" #:sizeof client3
                   (client2)))
            (list
             (smod #:name "server" #:sizeof client3
                   server2)
             (smod #:name "client" #:sizeof client3
                   (code (require server)
                         #,(code (add5 7)) (code:comment "=> contract error"))))))
    
    (slide #:title "Blame"
           (para "A contract is an agreement between two parties")
           ;'next
           'alts
           
           (list
            (list
             (smod #:name "server" #:sizeof client3
                   server)
             (smod #:name "client" #:sizeof client3
                   client3)
             'next
             (t "[Findler & Felleisen 02]"))
            (list
             (smod #:name "server" #:sizeof client3
                   server2)
             (smod #:name "client" #:sizeof client3
                   (code (require server)
                         #,(code (add5 7)) (code:comment "=> server broke the contract"))))))
    
    
    (slide #:title "Contracts"
           (para "A contract is an agreement between two parties")
           'alts
           (list
            (list
             (smod #:name "server" #:sizeof ho-client1
                   ho-server1)
             (smod #:name "client" #:sizeof ho-client1
                   ho-client1))        
            (list
             (smod #:name "server" #:sizeof ho-client1
                   ho-server2)
             (smod #:name "client" #:sizeof ho-client1
                   ho-client2))))
    
    (slide #:title "Contracts"
           (para "A contract is")
           (subpara "a dynamically enforced")
           (subpara "agreement between two parties")
           (subpara "with blame assignment"))
    ))


(tslide "Multi-Language Soundness")

(slide #:title "Typed & Untyped" #:layout 'center
       (para "")
       (tmod #:name "server" #:sizeof client3
             (code
              (: add5 (Number -> Number))
              (define (add5 x) (+ x 5))))
       (smod #:name "client" #:sizeof client3
             (code (require server)
                   (add5 7)
                   ||))
       (ghost (colorize (it "+: expects type <number> as 1st argument") "red")))

(slide #:title "Typed & Untyped" #:layout 'center
       (para "Untyped code can make mistakes")
       (tmod #:name "server" #:sizeof client3
             (code
              (: add5 (Number -> Number))
              (define (add5 x) (+ x 5))))
       (smod #:name "client" #:sizeof client3
             (client2 #:comment (code ||)))
       'next
       (colorize (it "+: expects type <number> as 1st argument") "red"))

(slide #:title "Typed & Untyped" #:layout 'center
       (para "Catch errors dynamically")
       (tmod #:name "server" #:sizeof client3
               (code
                (: add5 (Number -> Number))
                (define (add5 x) (+ x 5))))
       (smod #:name "client" #:sizeof client3
             (client2 #:comment (code (code:comment "=> client violated add5's type"))))
       'next
       (t "The untyped module is at fault"))

(slide #:title "Typed & Untyped" #:layout 'center
       (para "Catch errors dynamically")
       (smod #:name "server" #:sizeof client3
             (code
              (define (add5 x) "x plus 5")))
       (tmod #:name "client" #:sizeof client3
             (code (require server 
                            [add5 (Number -> Number)])
                   (add5 7) 
                   (code:comment "=> server interface violated add5's type")))
       'next
       (t "The untyped module is at fault"))

(slide #:title "The Blame Theorem"
       'alts
       (list
        (list
         (para "If the program raises a contract error, the blame is assigned to an untyped module."))
        (list
         (para "Well-typed modules can't get blamed."))
        (list (para "Allows local reasoning about typed modules, without changing untyped modules.")
              (para "Choose how much static checking you want."))))
#|
- Blame Theorem
  - trouble with untyped code
    - why is this a problem?
    - untyped code can do bad things
    - in both directions (examples)
  - how do we fix it?
    - non-solutions:
      - change the untyped code
      - change the vm
    - solution:
      - dynamic checks
      - otherwise known as behavioral software contracts (or contracts for short)
      - with blame
  - Contracts & Blame
    - Contracts are old
    - blame is (relatively) new
  - Contracts examples
    - simple
    - rich predicates
    - higher order
  - Blame
    - just like in real contracts, there are two parties
    - if something goes wrong, someone violated their obligation
    - blame pinpoints who made the mistake
  - Contracts, Blame and Types
    - if contracts are generated from types, who gets blamed?
    - otherwise known as, who can violate the type system?
    - answer: only the untyped side
  - The Blame Theorem (Wadler & Findler)
    - in a system with multiple modules, only the untyped modules can be blamed
    - or "well-typed modules can't be blamed"
    - generalization of Milner's famous slogan
  - The gold standard for soundness in multi-language systems
|#


(tslide "Typing Scheme Idioms")

(define tree-code
  (code
   (code:comment "a tree is either a number")
   (code:comment "or a pair of trees")
   (define (sum t)
     (cond [(pair? t) (+ (sum (car t))
                         (sum (cdr t)))]
           [else t]))))

(staged 
 [java ml]
 (slide
  #:title "Pick a Type System ..."
  (pict-case stage-name
             [(java) (vc-append
                    (para "Java?")
                    (blank gap-size)
                    (smod #:name "predicate" #:sizeof tree-code
                          (code (if (number? x)
                                    (+ x 5)
                                    (string-append x " five")))))]
             [(ml) (vc-append
                      (para "ML?")
                      (blank gap-size)
                      (smod #:name "union" #:sizeof tree-code
                            (code (code:comment "arg is either a string or a number")
                                  (define (format arg)
                                    #,(cloud 100 20)))))])
  ))



(slide #:title "A Type System for Scheme"
       (para "Scheme has its own idioms")
       'alts
       (list
        (list
         (smod #:name "predicate"
               #:sizeof tree-code
               (code (if (number? x)
                         (+ x 5)
                         (string-append x " five")))))
        (list
         (smod #:name "map"
               #:sizeof tree-code
               (code (map vector-ref 
                          (list vec1 vec2 vec3)
                          (list 0 7 77)))))
        #;
        (list
         (smod #:name "apply"
               #:sizeof tree-code
               (code (apply + 1 2 3 (list 4 5 6)))))
        (list
         (smod #:name "trees"
               tree-code))
        #;
        (list
         (smod #:name "overload"
               #:sizeof tree-code
               (code
                (code:comment "strings")
                (regexp-match "a.*" "abc") 
                (code:comment "byte strings")
                (regexp-match #"a.*" #"abc"))))
        ))

(define (fn #:circle? [circle? (code circle?)]
            #:s [s (code s)]
            #:ps [ps (code s)]
            #:cs [cs (code s)]
            #:cr [cr (code (circle-radius s))]
            #:typed [typed #f]
            #:zero [zero (code (* (rectangle-width s) 
                                  (rectangle-height s)))]
            #:crhs [crhs (code (* (sqr #,cr) pi))])
  (code (define (shape-area #,s)
          (cond
            [(#,circle? #,cs) #,crhs]
            [else #,zero]))))

(define sa-type (code (: shape-area (Shape -> Number))))

(define (c f #:typed [typed #f])
  (code 
   #,(if typed          
         (code (define-type-alias Shape (∪ Circle Rectangle))
               ||)
         (code (code:comment "Shape = Circle ∪ Rectangle")
               ||))   
   #,(if typed sa-type (code (code:contract Shape -> Number)))
   (code:comment "what is the area of shape s?")
   #,f
   ))


(define circle-pred
  (code (code (Any -> Boolean : Circle))))

(slide #:title "Occurrence Typing"
       #:layout 'center
       'alts
       (append        
        (mk-itemize #:block values #:overlay (lambda (a . b) a)
                    (list (para "How a Scheme programmer reasons"))
                    (list
                     (list
                      (smod (c (fn)))
                      (smod (c (fn #:cr (red-code (circle-radius s)))))
                      (smod (c (fn #:cr (red-code (circle-radius s))
                                   #:circle? (red-code circle?))))
                      (smod (c (fn #:zero (code (* #,(red-code (rectangle-width s))
                                                   #,(red-code (rectangle-height s))))
                                   #:circle? (red-code circle?)))))))
        (list
         (list 
          (para "How Typed Scheme Reasons")
          (vl-append (tmod (c (fn #:typed #t)
                              #:typed #t))
                     (blank 10))))
        (map (λ (t f)
               (list 
                (para "How Typed Scheme Reasons")
                (vc-append
                 (vl-append (tmod 
                             (c f #:typed #t))
                            (blank 10))
                 t)))
             (map (lambda (t) 
                    (if (pict? t)
                        (hc-append (rb-superimpose (code s : ||)
                                                   (ghost (code circle? : ||)))
                                   (lbl-superimpose t (ghost circle-pred)))
                        (hc-append (car t) (cadr t))))
                  (list  (code Shape)
                         (code Shape)
                         (list (code circle? : ||) circle-pred)
                         (code Circle)
                         (code Shape - Circle)
                         (code Rectangle)))
             (list 
                   (fn #:typed #t #:s (red-code s))
                   ;(fn #:typed #t #:ps (red-code s))
                   
                   (fn #:typed #t #:cs (red-code s))
                   
                   (fn #:typed #t #:circle? (red-code circle?))
                   ;(fn #:typed #t #:crhs (red-code (* (sqr (circle-radius s) 2) pi)))
                   (fn #:typed #t #:cr (code (circle-radius #,(red-code s))))
                   (fn #:typed #t #:zero (code (* (rectangle-width #,(red-code s))
                                                  (rectangle-height #,(red-code s)))))
                   (fn #:typed #t #:zero (code (* (rectangle-width #,(red-code s))
                                                  (rectangle-height #,(red-code s)))))))))

(define wrap-ty
  (code (: wrap 
          ((A ... -> B) -> (A ... -> B)))))
(define map-ty
  (code (: map 
          ((A ... -> B) (Listof A) ... -> (Listof B)))))

(slide #:title "Variable-Arity Polymorphism"
       'alts
       (list
        (list
         (para "Scheme has variable-arity functions")
         (smod #:name "print" #:sizeof map-ty
               (code 
                (printf "hello ~a, my name is ~a" you me))))
        (list
         (para "Scheme has variable-arity functions")
         (tmod #:name "print" #:sizeof map-ty
               (code 
                (printf "hello ~a, my name is ~a" you me))))
        (list
         (para "Scheme has complicated variable-arity functions")
         (smod #:name "wrap" #:sizeof map-ty
               (code
                ||
                ||
                (define (wrap f)
                  (lambda args
                    (printf "args are: ~a\n" args)
                    (apply f args))))))
        #;
        (list
         (para "Scheme has complicated variable-arity functions")
         (tmod #:name "wrap" #:sizeof map-ty
               (code
                (: wrap ???)
                ||
                (define (wrap f)
                  (lambda args
                    (printf "args are: ~a\n" args)
                    (apply f args))))))
        (list
         (para "Scheme has complicated variable-arity functions")
         (tmod #:name "wrap" #:sizeof map-ty
               (code
                #,wrap-ty
                (define (wrap f)
                  (lambda args
                    (printf "args are: ~a\n" args)
                    (apply f args))))))
        (list
         (para "Scheme has complicated variable-arity functions")
         (tmod #:name "wrap" #:sizeof map-ty
               (code
                #,wrap-ty
                (define (wrap f)
                  #,(cloud 100 30))
                (wrap substring)))
         'alts~
         (list (list (code substring : (String Number Number -> String))
                     'next
                     (code A ... = String Number Number #,(ghost (code , B = String))))
               (list (code substring : (String Number Number -> String))                     
                     (code A ... = String Number |Number,| #,(code B = String)))))
        (list
         (para " ")
         (tmod #:name "map" #:sizeof wrap-ty
               (code
                #,map-ty
                ||
                (map vector-ref 
                     (list vec1 vec2 vec3)
                     (list 0 7 77)))))
       ))
;(start)

(slide #:title "S-Expressions"
       (para "S-Expressions are available for creating new structures")
       'alts
       (list
        (list
         (smod #:name "tree"
               (code
                (code:comment "a Tree is either a Number")
                (code:comment "or a pair of Trees")
                ||
                (code:comment "sum : Tree -> Number")
                (define (sum t)
                  (cond [(pair? t) (+ (sum (car t))
                                      (sum (cdr t)))]
                        [else t])))))

        (list
         (tmod #:name "tree"
               (code
                (define-type-alias Tree 
                  (U Number (Pair Tree Tree)))
                ||
                (: sum (Tree -> Number))
                (define (sum t)
                  (cond [(pair? t) (+ (sum (car t))
                                      (sum (cdr t)))]
                        [else t])))))))
;(start)

#|  
- Typechecking Scheme Idioms
  - The need for idiomatic typechecking
    - The Java type system wouldn't work for Scheme
    - Neither would the ML type system
    - Why not?
    - idioms:
      - examples: cond, map, s-exp trees, path-string?, overloaded functions etc
      - this is how people program in Scheme
  - We'd like a type system that understands this
    - why should one exist?
    - everyone reasons about their programs
    - and no one re-reasons about everything from first principles
    - we develop a reasoning framework, and use that
    - untyped languages allow you to develop your own (or many of them)
  - what features do we need - let's look at our examples
  - cond - occurrence typing
    - step through typing of different expressions
  - map - variable-arity polymorphism
    - show map at multiple arities
    - show type of map
    - DO NOT show definitions
  - path-string - ad-hoc unions
    - can be declared anywhere
    - occurrence typing is the elimination rule
  - sexp trees - recursive types
    - works just as well as trees made in a more conventional way
    - which also work, of course
  - + maps integers to integers, reals to reals, etc
    - define your own overloaded functions
|#

#|

- Put it all together:
  - easy integration, easy typing
  - handles scheme idioms, and soundly
  - works on real code
    - used in drscheme, untyped
  - available now

- do i need related work?
   

|#

(tslide "Wrapping Up")

(slide #:title "Related Work"
       'alts
       (list
        (list
         (para "Typed & Untyped")
         (subpara "type Dynamic: Abadi et al")
         (subpara "ProfessorJ: Gray et al, Matthews & Findler")
         (subpara "Gradual Typing: Siek & Taha, Herman et al, Wadler"))
        (list
         (para "Types for Untyped Languages")
         (subpara "Smalltalk: Bracha & Griswold")
         (subpara "Scheme: Wand, Haynes, Leavens")
         (subpara "Analyzers: Wright, Flanagan, Marlow, Aiken")
         (subpara "Ruby: Furr et al"))))

(slide #:title "Conclusion"
       (para "When scripts grow up, their languages should grow with them")
       (blank 10)
       (para "Typed and untyped code can interoperate soundly")
       (blank 10)
       (para "Type systems can understand the idioms of untyped languages"))

(current-background-pict (bitmap plt-title-background-path))

(slide #:layout 'center 
       (blank 40)
       (text "Try Typed Scheme" (current-title-font) title-text-size)
       (blank 60)
       (text "Installer and Documentation" (current-main-font) (current-font-size))
       (text "http://www.plt-scheme.org" `(bold . ," Inconsolata") (current-font-size))
       (blank 20)
       (parameterize ([current-font-size 24])
         (vc-append (t "Thanks to Matthias Felleisen, Stevie Strickland, ")
                    (t "Ryan Culpepper, Matthew Flatt, Ivan Gazeau")))
       )

