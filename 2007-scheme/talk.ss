
#lang slideshow
(require scheme/math
         (only-in slideshow/slideshow slide/title page-item page-para/c page-para page-subitem
                  slide/title/center slide/center [pin-arrow-line old-pin-arrow-line])
         scheme/match
         ;(planet cce/scheme:6/slideshow)
         slideshow/pict
         slideshow/code
         (only-in mzlib/etc begin-with-definitions)
         (except-in "util.ss" big small)
         "../config.ss"
         "../lib.ss"
         "../tslide.ss")

(code-colorize-enabled #t)

(define (label p size l)
    (ht-append (ltl-superimpose (blank size 0) p)
               (blank 20 0)
               l))

(define (dg bss xf yf)
  (let ([flattened (apply append bss)])
    (table (apply max 0 (map length bss))
           flattened
           cc-superimpose
           cc-superimpose
           (* xf (apply max (map pict-width flattened)))
           (* yf (apply max (map pict-height flattened))))))

(define (dgc bss arrows xf yf)
  (let ([p (dg bss xf yf)])
    (let loop ([p p] [arrows arrows])
      (cond [(pair? arrows)
             (let-values ([(from to linew linec)
                           (match (car arrows)
                             [(list from to linew linec)
                              (values from to linew linec)]
                             [(list from to linew)
                              (values from to linew "black")]
                             [(list from to)
                              (values from to 1 "black")])])
               (loop (old-pin-arrow-line
                      15 p
                      from ct-find
                      to cb-find
                      linew linec)
                     (cdr arrows)))]
            [else p]))))


(define (anontmod)
  (cc-superimpose (colorize (filled-rounded-rectangle 250 200) typed-color)
                  (t "typed")))
(define (anonumod)
  (cc-superimpose (colorize (filled-rounded-rectangle 250 200) untyped-color)
                  (t "untyped")))

(define typed-color "blue")
(define untyped-color "red")

(define soft-typed-color "lightblue")
(define soft-untyped-color "pink")

(provide plt-lang)

;; ----
(define (plt-lang)
  ;; PLT LANGUAGE FRAMEWORK
  (begin-with-definitions
    (tslide "PLT Language Framework"
            (list "Macros" (ghost (subtitle-pict "Modules")) "Modules"))
    
    ;; FIXME: turn into scatter graph
    
    (slide/title "Components of language framework"
                 (page-item "macro system")
                 'next
                 (page-item "module system")
                 (page-item "syntax properties")
                 (page-item "compile-time API")
                 (page-item "expander hooks")
                 (page-item "...")
                 
                 ;; these reflect the need to have MORE control over compilation,
                 ;; to represent MORE information,
                 ;; to give macros MORE control over expansion
                 ;; to use macros in MORE situations
                 
                 ;;   Let me tell you what all of this is,
                 ;;   and what part it plays in the language framework.
                 )
    
    (slide #:title "Module system"
           #:layout 'center
           (t "Modules specify language of contents")
           (vl-append 30
                      (bmod "scheme" #:space #f #:sizeof (code "                   ")
                            (code ___))
                      (bmod "frtime" #:space #f #:sizeof (code "                   ")
                            (code ___))
                      (bmod "setup/infotab" #:space #f #:sizeof (code "                   ")
                            (code ___))
                      (bmod "typed-scheme" #:space #f #:sizeof (code "                   ")
                            (code ___))))
    
    
    (define (figm c? e?)
      (vl-append 20
                 (cmod #:name "A"  
                       #:sizeof
                       (label
                        (code (require A)
                              ___ f ___
                              ___ m ___)
                        300
                        (colorize (t "compiling") "red"))
                       (code (require scheme)
                             (provide m f)
                             #,(if e? 
                                   (highlight (code (define-syntax m ___)) "yellow")
                                   (code (define-syntax m ___)))
                             (define f ___)))
                 (bmod "scheme"
                       #:name "B"
                       #:sizeof (label
                                 (code (require A)
                                       ___ f ___
                                       ___ m ___)
                                 300
                                 (colorize (t "compiling") "red"))
                       (if c?
                           (label
                            (code (require A)
                                  ___ f ___
                                  ___ m ___)
                            300
                            (colorize (t "compiling") "red"))
                           (code (require A)
                                 ___ f ___
                                 ___ m ___)))))
    
    (slide #:title "Module system" #:layout 'center
           (t "Modules import and export values and macros")
           'alts
           (list (list (figm #f #f))
                 (list (figm #t #f))
                 (list (figm #t #t))))
    
    (slide #:title "Module system" 
           (item "Independent compilation")
           (subitem "Separate run-time and compile-time instances of modules")
           (subitem "Every module compilation gets a fresh slate")
           (subitem "Side effects during one compilation don't affect others")
           (subitem "Interactive and batch modes behave the same"))
    
    ;; FIXME: illustrate!
    
    (slide #:title "Syntax objects"
           (page-item "Abstract datatype for \"decorated\" S-expressions")
           (page-item "Representation for")
           (page-subitem "initial programs")
           (page-subitem "partially-expanded programs")
           (page-subitem "fully-expanded programs")
           (page-subitem "instrumented or processed programs")
           (blank)
           (page-para/c (code (code:line expand : (syntax -> syntax)))))
    
    ;; Once you've got this abstract data type to hang hygiene info on,
    ;; why not hang other useful info too?
    
    (slide #:title "Syntax objects"
           (page-item "Support hygiene via lexical context information")
           (page-item "Track source locations")
           ;;   Once you've got it, why not extend it to contain other info
           (page-item "Syntax properties")
           (page-subitem "Associate arbitrary \"out-of-band\" data with terms")
           (page-subitem "Macros and language tools define protocols")
           (page-subitem "Guide analysis and instrumentation"))
    
    (define (figcs h? p?)
      (let-syntax ([h (syntax-rules () 
                        [(h e)
                         (pict-if h?
                                  (highlight (code e) "yellow")
                                  (code e))])])
        (vl-append 30
                   (nomod                    
                    #,(pict-if #t
                               (code (define-struct #,(h posn) (x y))
                                     (define (dist p)
                                       (match p
                                         [(struct #,(h posn) (x y))
                                          (sqrt (+ (sqr x) (sqr y)))])))
                               (code (define (dist p)
                                       #,(codep
                                          (if (#,(h posn?) p)
                                              (let ([x (#,(h posn-x) p)]
                                                    [y (#,(h posn-y) p)])
                                                (sqrt (+ (sqr x) (sqr y))))
                                              (error "match failed"))
                                          (if #t
                                              (list (list 'disappeared-use (code #'posn)))
                                              null))))))
                   (hc-append 10 (ghost (code (define (distp p)))) (downexpand))
                   (nomod
                    #,(pict-if
                       #t
                       (code (define (dist p)
                               #,(codep
                                  (if (#,(h posn?) p)
                                      (let ([x (#,(h posn-x) p)]
                                            [y (#,(h posn-y) p)])
                                        (sqrt (+ (sqr x) (sqr y))))
                                      (error "match failed"))
                                  (if p?
                                      (list (list 'disappeared-use (code #'posn)))
                                      null))))
                       (code (define (dist p)
                               #,(codep
                                  (if (#,(h posn?) p)
                                      (let ([x (#,(h posn-x) p)]
                                            [y (#,(h posn-y) p)])
                                        (sqrt (+ (sqr x) (sqr y))))
                                      (error "match failed"))
                                  (if #t
                                      (list (list 'disappeared-use (code #'posn)))
                                      null)))))))))
    
    
    (slide #:title "Syntax properties example" #:layout 'center
           (t "Hints to Check Syntax tool")
           'alts
           (list (list (figcs #t #f))
                 (list (figcs #f #t))))
    
    
    ;  (define (fig2 ?)
    ;    (list
    ;     (page-para/c
    ;      (vc-append 20
    ;                 (code (or X Y))
    ;                 (downexpand)
    ;                 (codep (if (verify-boolean X 'or)
    ;                            #t
    ;                            (if (verify-boolean Y 'or)
    ;                                #t
    ;                                #f))
    ;                        (if ?
    ;                            (list (list 'stepper-hint
    ;                                        (code 'came-from-or)))
    ;                            null))))
    ;     (page-para "Which reduction rule should the stepper use?")
    ;     (page-subitem "the" (code if) "rule?")
    ;     (page-subitem "the" (code cond) "rule?")
    ;     ((if ? (lambda (p) (highlight p "yellow")) values)
    ;      (page-subitem "the" (code or) "rule?"))))
    ;  
    ;  (slide/title "Stepper example"
    ;    (page-para "Language: Beginning Student")
    ;    'alts
    ;    (list (fig2 #f)
    ;          (fig2 #t)))
    
    (slide #:title "Compile-time API"
           (page-item (code local-expand))
           (page-subitem "Like" (code expand) ", but works within macros")
           (page-subitem "Can partially or completely expand"))
    
    (slide #:title "Local-expansion example"
           (page-para
            (code
             (class object%
               (init-field color)
               (define-get+set color)
               ___)))
           'next
           (page-para
            (hc-append (blank 200 1) (downexpand)))
           (page-para
            (code
             (class object%
               #,(labl (init-field color) "init field decl")
               #,(labl (public get-color) "access declaration")
               #,(labl (define get-color
                         (lambda () color)) "method definition")
               #,(labl (public set-color!) "access declaration")
               #,(labl (define set-color
                         (lambda (x)
                           (set! color x))) "method definition")
               ___))))
    
    (staged [zero one two three]
            (slide #:title "Module expansion hook"
                   (page-item (code #%module-begin))
                   (page-subitem "Wrapped around module's body")
                   (page-subitem "Determined by module's initial language")
                   (pict-case
                    stage-name
                    [(zero) (blank 1)]
                    [else
                     (bmod "scheme" #:sizeof ((current-code-tt) "                              ")
                           (pict-case
                            stage-name
                            [(zero one) (code ||
                                              ||(define x 1)
                                              ||(+ x 3))]
                            [(two) (code 
                                    (#%module-begin
                                     (define x 1)
                                     (+ x 3)))]
                            [(three) (code 
                                      (#%plain-module-begin
                                       (define x 1)
                                       (print-values (+ x 3))))]))])))))


;; TYPED SCHEME
(provide ts-impl)
(define (ts-impl)
  (begin-with-definitions
    
    (tslide "Implementing Typed Scheme")
    
    ;; IMPLEMENTING TYPED SCHEME
    
    (slide #:title "Implementation"
           (page-item "Typed Scheme operates on expanded code")
           (page-subitem "Cannot derive type rules for arbitrary macros")
           (page-item "Type-checking occurs at compile time")
           (page-subitem "Ill-typed programs do not compile"))
    
    (slide #:title "Implementation" #:layout 'center
           (let ([andthen (scale (arrow 20 (* 3/2 pi)) 1 2)])
             (big (vc-append 20
                             (t "expand")
                             andthen
                             (t "type-check")))))
    
    (slide #:title "One module" #:layout 'center
           (page-para/c (anontmod)))
    
    ;; First: expressions at typed-repl
    (slide #:title "From Macro to Type-Checker"
           (page-item "Use expansion hooks to get control")
           (page-subitem (code #%module-begin))
           (page-item "Use" (code local-expand) "to expand program to core Scheme")
           (blank)
           (page-para
            (code (define-syntax (#%module-begin stx)
                    (let ([expanded-code
                           (local-expand-completely stx)])
                      (type-check-contents expanded-code))))))
    
    ;; But now we have a problem.
    ;; How do we handle the typed binding forms?
    (slide #:title "Type-checker"
           (page-para "Recursive function on core syntax")
           (page-item "Non-binding forms are straightforward")
           (page-para/c (code (+ e1 e2)))
           (page-item "Typed binding forms")
           (page-para/c (code (lambda: ([x : Number]) (+ 1 x))))
           (blank)
           (page-subitem "Expansion must retain types")
           (page-subitem (code lambda) "has no place for types"))
    
    ;; Variable protocol
    (slide #:title "The Variable Protocol"
           (page-para "Every typed binding form decorates its bound variables"
                      "with their declared types attached to their"
                      (code 'type-label) "syntax property.")
           (blank)
           (vc-append 20
                      (code (lambda: ([x : s] ...) e))
                      (downexpand)
                      (blank 10)
                      (code (lambda (#,(codep x (list (list 'type-label (code s)))) ...) e)))
           (page-para "Type-checker picks up variable types as it traverses program"))
    
    ;; What about definitions?
    
    (slide #:title "Definitions"
           (page-item "Global variable table (at compile time)")
           (page-subitem "Initialized to Scheme primitives")
           (page-item "Definitions add to table")
           (page-para/c
            (vc-append 20
                       (nomod 
                        #,(pict-if
                           #t
                           (code (: x t)
                                 (define x e))
                           (code (declare-type! x t)
                                 (define x e))))
                       (blank 10)(downexpand)
                       (blank 10)
                       (nomod 
                        (declare-type! x t)
                        (define x e)))))
    ;; Untyped->Typed
    
    (slide/title/center "Typed module imports untyped module"
                        (let ([A (anonumod)]
                              [B (anontmod)])
                          (dgc (list (list A)
                                     (list B))
                               (list (list B A))
                               1
                               1)))
    
    (slide #:title "Typed module imports untyped module"
           (t "Enforce type invariants via contracts")
           (vl-append
            20
            (nomod 
             (require/typed scheme/file
                            [find
                             ((Path -> Boolean) Path -> (Listof Path))]))
            (hc-append (blank 200 1) (downexpand))
            (nomod
             #,(codep (require 
                       (rename-in scheme/file unsafe-find find))
                      (list (list 'trust (code #t))))
             (: find-files ((Path -> Boolean) Path -> (Listof Path)))
             (define find-files
               #,(codep (contract unsafe-find
                                  (type->contract
                                   ((Path -> Boolean) Path -> (Listof Path)))
                                  scheme/file
                                  '<typed-scheme>)
                        (list (list 'trust (code #t))))))))
    
    (slide/title/center "Typed module imports typed module"
                        (let ([A (anontmod)]
                              [B (anontmod)])
                          (dgc (list (list A)
                                     (list B))
                               (list (list B A))
                               1
                               1)))
    
    (slide #:title "Typed module imports typed module"
           (t "Foreign variable occurrences")
           (blank 20)
           (vl-append 40
                      (cmod #:name "plus1"
                            (code (require typed-scheme)
                                  (define plus1 (lambda (n) (+ 1 n)))))
                      (label
                       (tmod #:name "t" #:sizeof (code (define plus1 (lambda (n) (+ 1 n))))
                             (code (require plus1)
                                   (#,(highlight (code plus1) "yellow") 7)))
                       400
                       (colorize (t "compiling") "red")))
           ;; Need types for these in order to type-check!
           ;; Cannot inspect syntax properties now!
           )
    
    (slide #:title "Module Variable Protocol"
                        (page-para "During the compilation of a typed module,"
                                   "the global variable table"
                                   "contains the type associations for all of the"
                                   "typed modules that it (transitively) imports.")
                        ;; How can we make that true?
                        ;; Remember, each compilation starts out with a clean slate!
                        )
    
    (slide #:title "Implementation"
                        (let ([andthen (scale (arrow 20 (* 3/2 pi)) 1 2)])
                          (big (vc-append 20
                                          (t "expand")
                                          andthen
                                          (t "type-check")
                                          andthen
                                          (t "transform")))))
    
    (slide #:title "Module Variable Protocol"     
           #:layout 'center                   
           (tmod #:name "plus1" #:sizeof (code (begin-for-syntax 
                                                 (declare-type! #'plus1 (Number -> Number))))
                 (code (provide plus1)
                       (: plus1 (Number -> Number))
                       (define (plus1 n) (+ 1 n))))
           (hc-append (downexpand))
           (cmod #:name "plus1"
                 (code (require typed-scheme)
                       (provide plus1)
                       (define plus1 (lambda (n) (+ 1 n)))
                       #,(highlight
                          (code (begin-for-syntax 
                                  (declare-type! #'plus1 (Number -> Number))))
                          "yellow"))))
    
    (slide #:title "Module Variable Protocol" #:layout 'center
           (vl-append 40
                      (cmod #:name "plus1"
                            (code (require typed-scheme)
                                  (provide plus1)
                                  (define plus1 (lambda (n) (+ 1 n)))
                                  #,(highlight
                                     (code (begin-for-syntax 
                                             (declare-type! #'plus1 (Number -> Number))))
                                     "yellow")))
                      (label
                       (tmod #:name "t" #:sizeof (code (begin-for-syntax 
                                                         (declare-type! #'plus1 (Number -> Number))))
                             (code (require plus1)
                                   (#,(highlight (code plus1) "yellow") 7)))
                       400
                       (colorize (t "compiling") "red"))))
    
    
    (slide/title/center "Untyped module imports typed module"
                        (let ([A (anontmod)]
                              [B (anonumod)])
                          (dgc (list (list A)
                                     (list B))
                               (list (list B A))
                               1
                               1)))
    
    (slide #:title "Defensive exports" #:layout 'center
           (tmod #:name "plus1" #:sizeof (code (provide (rename defensive-plus1 plus1)))
                 (code (provide plus1)
                       (: plus1 (Number -> Number))
                       (define (plus1 n) (+ 1 n))))
           (hc-append (downexpand))
           (cmod #:name "plus1"
                 (code
                  (require typed-scheme)
                  (define plus1 (lambda (n) (+ 1 n)))
                  #,(highlight
                     (code (define/contract defensive-plus1
                             (type->contract (Number -> Number))
                             plus1))
                     "yellow")
                  #,(highlight
                     (code (provide (rename defensive-plus1 plus1)))
                     "yellow"))))
    
    ;; But that's surely not what we had earlier...
    ;; Do we want it to work right for typed context or an untyped context?
    (staged [contract p/uc itc]
    (slide #:title "Defensive exports" #:layout 'center
           (cmod #:name "plus1"
                 (pict-case stage-name
                  [(contract) 
                   (code
                    (require typed-scheme)
                    (define plus1 (lambda (n) (+ 1 n)))
                    #,(highlight
                       (code (define/contract defensive-plus1
                               (type->contract (Number -> Number))
                               plus1))
                       "yellow")
                    #,(highlight
                       (code (provide (rename defensive-plus1 plus1)))
                       "yellow"))]
                  [(p/uc)
                   (code
                    (require typed-scheme)
                    (define plus1 (lambda (n) (+ 1 n)))
                    #,(highlight
                       (code (define/contract defensive-plus1
                               (type->contract (Number -> Number))
                               plus1))
                       soft-untyped-color)
                    #,(highlight
                       (code (provide/untyped-context 
                              (rename defensive-plus1 plus1)))
                       soft-untyped-color)
                    #,(highlight
                       (code (begin-for-syntax
                               (declare-type! #'plus1 (Number -> Number))))
                       soft-typed-color)
                    #,(highlight
                       (code (provide/typed-context plus1))
                       soft-typed-color))]
                  [(itc)
                   (code 
                    (require typed-scheme)
                    (define plus1 (lambda (n) (+ 1 n)))
                    #,(highlight
                       (code (define/contract defensive-plus1 ___))
                       soft-untyped-color)
                    #,(highlight
                       (code (begin-for-syntax
                               (declare-type! #'plus1 (Number -> Number))))
                       soft-typed-color)
                    #,(highlight
                       (code (define-syntax export-plus1
                               (make-rename-transformer
                                (if (in-typed-context?)
                                    #'plus1
                                    #'defensive-plus1))))
                       "yellow")
                    #,(highlight
                       (code (provide (rename export-plus1 plus1)))
                       "yellow"))]))
           #;
           
                 (code (compiled-module plus1
                                        (require typed-scheme)
                                        (define plus1 (lambda (n) (+ 1 n)))
                                        #,(highlight
                                           (code (define/contract defensive-plus1
                                                   (type->contract (Number -> Number))
                                                   plus1))
                                           soft-untyped-color)
                                        #,(highlight
                                           (code (provide/untyped-context 
                                                  (rename defensive-plus1 plus1)))
                                           soft-untyped-color)
                                        #,(highlight
                                           (code (begin-for-syntax
                                                   (declare-type! #'plus1 (Number -> Number))))
                                           soft-typed-color)
                                        #,(highlight
                                           (code (provide/typed-context plus1))
                                           soft-typed-color)))))
    
    ;; Unfortunately, the module system won't do that
    #;
    (slide/center
     (page-para
      (vl-append 20
                 (code (module plus1 typed-scheme
                         (provide plus1)
                         (define: (plus1 [n : Number]) : Number
                           (+ 1 n))))
                 (hc-append (blank 200 1) (downexpand))
                 (code (compiled-module plus1
                                        (require typed-scheme)
                                        (define plus1 (lambda (n) (+ 1 n)))
                                        #,(highlight
                                           (code (define/contract defensive-plus1 ___))
                                           soft-untyped-color)
                                        #,(highlight
                                           (code (begin-for-syntax
                                                   (declare-type! #'plus1 (Number -> Number))))
                                           soft-typed-color)
                                        #,(highlight
                                           (code (define-syntax export-plus1
                                                   (make-rename-transformer
                                                    (if (in-typed-context?)
                                                        #'plus1
                                                        #'defensive-plus1))))
                                           "yellow")
                                        #,(highlight
                                           (code (provide (rename export-plus1 plus1)))
                                           "yellow"))))))
    
    (slide #:title "Implementing (in-typed-context?)"
           #:layout 'center
           (page-para "What is the difference between a typed module and an untyped module?")
           'next
           (page-para "A typed module executes the typed" (code #%module-begin) "macro.")
           'next
           (blank 20)
           (page-para "Typed Scheme defines mutable compile-time flag")
           (page-item "Flag is false by default (untyped context)")
           (page-item "Typed" (code #%module-begin) "macro sets it to true")
           (page-item (code (in-typed-context?)) "reports value of flag")
           (page-item "Works with arbitrary nesting"))
    
    ))


