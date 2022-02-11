#lang slideshow

(require slideshow/step slideshow/code slideshow/face)

(require beamer/beamer
         "../dls06/utils.ss")



#|
OUTLINE:

- title slide
- Program Evolution
  - It's happening, we need to support it
- A Model of Evolution
- Supporting Program Evolution
  - Piece by piece
  - new languages


|#

(current-keyword-list (list* "define:" "let:" "require/typed" "require/contract" ":" "->" 
                             "provide/contract" "..." "define-type-alias"
                             (current-keyword-list)))

(define (tslide t)
  (slide #:layout 'center
       (titlet t)))

(define (mk-itemize texts pics #:para [para para] #:block [block blockf]
                    #:overlay [overlay lt-superimpose])
  (define g-pics (map ghost (apply append (map (lambda (e) (if (list? e) e (list e))) pics))))
  (let loop ([seen-ts '()] [ts (map para texts)] [ps pics])
    (cond
      [(null? ts)
       null]
      [(list? (car ps))
       (append
        (map (lambda (p)
               (append (reverse seen-ts)
                       (list (car ts)) 
                       (map ghost (cdr ts))
                       (list (block (apply overlay p g-pics)))))
             (car ps))
        (loop (cons (car ts) seen-ts) 
              (cdr ts)
              (cdr ps)))]
      [else
       (cons
        (append (reverse seen-ts)
                (list (car ts)) 
                (map ghost (cdr ts))
                (list (block (apply overlay (car ps) g-pics))))
        (loop (cons (car ts) seen-ts) 
              (cdr ts)
              (cdr ps)))])))


   
(title "The Design and Implementation of Typed Scheme")
(author "Sam Tobin-Hochstadt" "Northeastern University" #f)

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



(tslide "Program Evolution")

(slide #:title "Program Evolution is Happening"
       
       'alts
       (mk-itemize #:block values #:overlay cc-superimpose
        (list "Scripting Languages are popular and useful"
              "But sometimes, you want types"
              "And rewriting in C++ is no fun")
        (list (list (bitmap "javascript-logo.png")
                    (scale (bitmap "perl_logo.jpg") .5)
                    (scale (bitmap "python-logo.png") .8)
                    (scale (bitmap "ruby-400.png") .5))
              (list (blank 20 20) (scale (bitmap "ariane5-2.png") .45))
              (list (bitmap "viaweb_logo.png")
                    (face 'unhappy)))))

(slide #:title "A Model of Program Evolution"
       #:layout 'center
       'next
       'alts
       (list
        (list (para "A program is composed of modules")
              (mk-diag untyped untyped #f #f))
        (list (para "The program evolves piece by piece")
              (mk-diag untyped typed #f #f))
        (list (para "Interactions are properly checked")
              (mk-diag untyped typed #t #t))))

(let* ([b1 (block
            (para "Creating A Whole New Series of Languages"))]
       [b2 (block
            (para
             "Combining Existing Languages")
            (subitem "Standard ML and Scheme?")
            (subitem "Java and Python?"))]
       [b3 (block
            (para "Designing a New Language ..."))]
       [b4 (block
            (para "Designing a New Language with an Existing Language"))]
       [mk (lambda (b) (apply lt-superimpose b (map ghost (list b1 b2 b3 b4))))])
  
  (slide #:title "Supporting Program Evolution"
         'alts
         (list
          (list
           (mk b1)
           'next
           (para "Lots of those already"))
          
          (list
           (mk b2)
           'next
           (para "Existing languages do not fit together"))
          (list
           (mk b3))
          (list
           (mk b4)
           'next
           (para "Scheme and Typed Scheme")))))
       
       

(define (mod/lang lang #:space [space #t] . body)
  (apply
   vl-append ((current-code-tt) (string-append "#lang " lang))
   (if space (cons (tt "") body) body)))

(define orig-code 
  (code 
   (define x 5)
   (if (number? x)
       (add1 x)
       #f)))

(define new-code
  (code 
   (define: x : Any 5)
   (if (number? x)
       (add1 x)
       #f)))



(slide #:title "Typing Individual Modules"        
       'alts
       (mk-itemize
        (list "Module level language decls"
              "Contracts"
              "Easy porting")
        (list (mod/lang "typed-scheme"
                        (code (code:comment "Module body ...")))
              (list               
               (code (require/typed    md5 (Any   -> |Bytes |)
                                       mzlib/md5))
               (code (require/contract md5 (any/c -> bytes?)
                                       mzlib/md5)))
              (list (mod/lang "scheme"
                              (lt-superimpose (ghost new-code) orig-code))
                    (mod/lang "typed-scheme"
                              (lt-superimpose 
                               (ghost orig-code)
                               new-code))))))
       

(tslide "Interoperating with Untyped Code")

#;
(slide #:title "Modules"
       'next
       (para "Unit of Design")
       'next
       (para "Unit of Compilation")
       'next
       (para "Unit of Language"))




(let* ([p0 (mod/lang "typed-scheme"
                     (code (define: (addx [x : Number]) : (Number -> Number)
                             (lambda: ([y : Number]) (+ x y))))
                     (code (provide/contract 
                            [sq (-> number? (-> number number?))])))]
       (p1 
        (vl-append
         (lt-superimpose
          (ghost p0)
          (mod/lang "typed-scheme"
                   (code (define: (sq [x : Number]) : Number (* x x)))
                   (code (provide sq))))
         (tt "")
         (mod/lang "scheme"
                   (code (sq 11))
                   (code (sq "eleven")))))
       (p2 
        (vl-append
         (lt-superimpose
          (ghost p0)
          (mod/lang "typed-scheme"
                    (code (define: (sq [x : Number]) : Number (* x x)))
                    (code (provide/contract [sq (-> number? number?)]))))
         (tt "")
         (mod/lang "scheme"
                   (code (sq 11))
                   (code (sq "eleven") (code:comment "=> contract violation")))))
       (p3
        (vl-append
         p0
         (tt "")
         (mod/lang "scheme"
                   (code ((addx 5) 6))
                   (code ((addx 5) "six") (code:comment "=> contract violation"))))))
  (slide #:title "Danger!"
         (para "Untyped code is dangerous")
         (para "")
         (para "")
         (block (lt-superimpose p1 (ghost p2) (ghost p3))))
  (slide #:title "Contracts"       
         'alts
         (mk-itemize
          (list "Untyped code is dangerous"
                "Contracts protect Typed Code"
                "Protection in both directions")
          (list p1 p2 p3))))

;(slide #:title "Contracts (HO)")


(define (example e)
  (code                 
   (define  (play-one-turn  player
                            deck           stck
                            |fst:discs        |)
     ||
     (define trn (create-turn (player-name player)
                              deck stck fst:discs))
     ;; — go play
     (define res (player-take-turn player trn))
     ;; the-return-card could be false
     #,e
     (define discards:squadrons (done-discards res))
     (define attacks (done-attacks res))
     (define et (turn-end trn))
     (values the-end the-return-card 
             discards:squadrons attacks et))))

(define (texample e)
  (code                 
   (define: (play-one-turn [player : Player]
                           [deck : Cards] [stck : Cards]
                           [fst:discs : Hand])
     : (values Boolean RCard Hand Attacks From)
     (define trn (create-turn (player-name player)
                              deck stck fst:discs))
     ;; — go play
     (define res (player-take-turn player trn))
     ;; the-return-card could be false
     #,e     
     (define discards:squadrons (done-discards res))
     (define attacks (done-attacks res))
     (define et (turn-end trn))
     (values the-end the-return-card
             discards:squadrons attacks et))))

(tslide "Types for Scheme")

(define (dv/kw #:cond [cnd (code cond)]
               #:ret-test [ret (code (ret? res))]
               #:end-test [end (code (end? res))]
               #:res1 [res1 (code res)]
               #:res2 [res2 (code res)]
               #:ret-card [ret-card (code ret-card)]
               #:end-card [end-card (code end-card)]
               #:tr [tr (code #t)]
               #:fl [fl (code #f)]
               #:els [els #f])
  (if els
      (code
       (define-values (the-end the-return-card)
         (#,cnd
          [#,ret (values #,fl (#,ret-card #,res1))]
          [#,end (values #,tr (#,end-card #,res2))]
          [else #,els])))
      (code
       (define-values (the-end the-return-card)
         (#,cnd
          [#,ret (values #,fl (#,ret-card #,res1))]
          [#,end (values #,tr (#,end-card #,res2))])))))

(define dv (dv/kw))

(define dv/red (dv/kw #:cond (colorize ((current-code-tt) "cond") "red")))

(define bdv (block dv))

(define dv* (block* dv))

(define dv-big 
  (lt-superimpose
   dv*
   (ghost bdv)))

(define just-dv (pin-over (ghost (texample dv-big)) dv-big lt-find bdv))

(slide #:title "A Typical Scheme Program" #:layout 'tall
       'alts
       (list (list (example dv-big))
             (list (texample dv-big))
             (list (texample
                    bdv))
             (list just-dv)))

(define r (lambda (x) (colorize ((current-code-tt) x) "red")))

(slide #:title "Occurence Typing"
       'alts
       (let* ([rcond (r "cond")])
         (mk-itemize 
          #:para values
          (list 
           (para "Variant Selection with " rcond)
           (para "Predicates determine types"))
          (list
           (dv/kw #:cond rcond)
           (list (dv/kw #:ret-test (r "(ret? res)") #:res1 (r "res"))
                 (dv/kw #:ret-test (r "(ret? res)") #:res1 (r "res")
                        #:ret-card (r "ret-card"))
                 (dv/kw #:end-test (r "(end? res)") #:res2 (r "res"))
                 (dv/kw #:end-test (r "(end? res)") #:res2 (r "res")
                        #:end-card (r "end-card")))))))

(slide #:title "True Unions"
       'alts
       (mk-itemize #:para values
        (list (para (r "res") "has type" (code (U ret end)))
              (para "So the" (code else) "clause is never executed"))
        (list (dv/kw #:ret-test (code (ret? #,(r "res"))))
              (dv/kw #:els (r "(void)")))))


(slide #:title "Values As Types"
       'alts
       (mk-itemize 
        #:para values
        (list (para (code #f) "has type" (code #f))
              (para "Supports widely-used Scheme idioms"))
        (list (dv/kw)
              (code (case light
                      [('red) (stop)]
                      [('yellow) (slow-down)]
                      [('green) (go)])))))



(slide #:title "Other Type System Features"
       'alts
       (mk-itemize
        (list "Polymorphism"
              "Recursive types"
              "Variable-arity functions and apply"
              "Multiple value returns")
        (list
         (code (define: (a b) (map [f : (a -> b)] 
                                   [l : (Listof a)])
                 : (Listof b)
                 #,(cloud 50 20)))
         (code (define-type-alias Sexp 
                 (mu s (U Number Symbol '() (Pair s s)))))
         (vl-append (code + : (Number ... -> Number))
                    (code ||)
                    (code (apply + (list 1 2 3))))
         (code (let-values ([(x y) (values 1 2)])
                 (+ x y))))))

;; BUG - not a good error message (tslide #:title "Putting it all together")
(tslide "Putting it all together")

(slide #:title "Scaling to a real language"
       (para "Designs are nice, but what about using it?")
       'next
       (para "Typed Scheme is a language on top of PLT Scheme")
       'alts
       (list
        (list (block
               (para "Rich variety of datatypes")
               (subitem "Vectors")
               (subitem "Parameters")
               (subitem "Input/output")
               (subitem "User-defined structures")))
        (list (block
               (para "Rich development environment")
               (subitem "Integrated with DrScheme")
               (subitem "Highlights type errors in the source")))
        (list (block
               (para "Language complexity")
               (subitem "Macros")
               (subitem "Modules")
               (subitem "...")))))

(slide #:title "Macros"
       (para "Every PLT Scheme program makes use of macros")
       (blank 10)
       (para "So do Typed Scheme programs")
       'next
       (blank 10)
       (para "Typechecking happens after macro expansion [Template Haskell]")
       (para "Typechecking is integrated with the macro and module systems [Flatt02]"))


(slide #:title "Implementation"
       (para "Typed Scheme is implented using the PLT Scheme macro system")
       
       (subitem "This allows errors to be reported statically")
       'next
       (blank 10)
       (para "This design also facilitates the integration with untyped code")
       'next
       (blank 10)
       (para "The implementation is described in a Scheme Workshop 2007 paper"))


(slide #:title "Validation" #:layout 'auto
       (block
        (para "Implemented and Available")
        (subitem "Reports of use from multiple continents"))
       'next
       (block 
        (para "Ported 5000 lines of Scheme code")
        (subitem "Games, Scripts, Educational code"))
       'next
       (block
        (para "PLT Redex model")
        (subitem "Visualizes static and dynamic semantics"))
       'next
       (block
        (para "Mechanized Type Soundness proof")
        (subitem "Isabelle HOL + Nominal package")))

;; interstital slide here?

(slide #:title "Related Work"
       
       (block
        (para "Occurrence Typing")
        (subitem "Weirich et al"))
       'next
       (block
        (para "Types for Untyped Languages")
        (subitem "Soft Typing (Flangan, Wright, Fagan)")
        (subitem "Erlang (Marlow and Wadler)"))
       'next
       (block
        (para "Gradual Typing")
        (subitem "Siek and Taha")
        (subitem "Herman et al, Flanagan et al, Wadler and Findler")))

(slide #:title "Conclusions"
       (para "Program evolution is happening, and needs PL support")
       'next
       (para "Type systems can be applied to existing untyped languages")
       'next
       (para "Typed Scheme Works!"))

(slide #:layout 'center
       (titlet "Thank You")
       'next
       (t "Implementation, PLT Redex Model, Isabelle Model")
       (tt "http://www.ccs.neu.edu/~samth/typed-scheme")
       'next 
       (tt "google \"typed scheme\""))


       