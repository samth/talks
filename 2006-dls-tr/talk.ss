(module talk slideshow/slideshow
  
  (require    (lib "kw.ss") (lib "symbol.ss" "texpict")
              (lib "face.ss" "texpict") (lib "etc.ss")
              (lib "code.ss" "slideshow") (lib "step.ss" "slideshow")
              "utils.ss")
  
  ;; make it pretty
  #;(require "setup.ss")
  
  
  (define-syntax smalltext
    (syntax-rules ()
      [(_ e) (parameterize ((current-font-size (* 3/4 
                                                  (current-font-size)))) e)]))
  (define-syntax tiny
    (syntax-rules ()
      [(_ e) (parameterize ((current-font-size (/ (current-font-size) 
                                                  2))) e)]))
  (current-keyword-list (list* "define:" "provide/contract" "/contract" ":" "<=" "->" (current-keyword-list)))

  
  (define (highlight pict color)
    (pin-under pict 0 0
               (colorize
                (filled-rectangle (pict-width pict) (pict-height pict))
                color)))
  
  (define (hy p) (highlight p "yellow"))
  
  (define (code/pc-fun)
    (hb-append (code provide) (hy (code /contract))))

  (define code/pc (code/pc-fun))
  (define smallcode/pc (smalltext (code/pc-fun)))

  
  (define (box-around pict color)
    (pin-under pict 0 0
               (colorize
                (thick (rectangle (pict-width pict) (pict-height pict)))
                color)))

  
  (define (slide/c x) (slide/center (colorize (page-para* x) "blue")))
  
  (define (slide/money n)
    (unless (string? n) (error "not a string"))
    (slide/center (big (colorize (bt (format "$~a" n)) "red"))))

  (define (header s) (slide/center (titlet s)))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;          Story/Intro         ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; title slide
  (slide/center
   (big (titlet "Interlanguage Migration"))
   (titlet "From Scripts to Programs")
   (t "Sam Tobin-Hochstadt and Matthias Felleisen")
   (t "Northeastern University")
   (blank)
   (big (text "DLS 2006")))
  
  (header "A Story")
  
  (slide/c "About a programmer")
  
  (slide/c "Who needed to manage his budget")
  
  (slide/c "And so, he wrote a simple little program")
  
  (slide/c "In his favorite (dynamic) language:")
  
  (slide/center (vc-append (bitmap "PLT-206.png") (colorize (page-para* "PLT Scheme") "blue")))
  
  (slide/c "He did well for himself, and now he needed to manage some investments as well")
  
  (slide/c "So he added more pieces to his program")
  
  (slide/c "Then he decided he wanted to access the system remotely")
  
  (slide/c "So he added a web front-end")
  
  (slide/c "He kept it all nicely organized")
  
  (slide/c "Since, after all, the program was managing")
  
  (slide/money "5,000")
  
  (slide/c "Soon, his friends noticed that he was making lots of money on the stock market")
  
  (slide/c "And they wanted to use his system as well")
  
  #;(slide/c "And he said yes")
  
  (slide/c "And soon the system was managing")
  
  (slide/money "50,000")
  
  (slide/c "Of course, having his friends use his system entailed new responsibilites")
  
  (slide/c "Like testing ...")
  
  (slide/c "And lots more code")
  
  (slide/c "Fortunately, he was very productive in his favorite language")
  
  (slide/c "Which was good - after all, the system managed")
  
  (slide/money "500,000")
  
  (slide/c "But his friends")
  
  (slide/c "(and their friends,")
  
  (slide/c "and their grandmothers,")
  
  (slide/c "and their grandmothers' friends)")
  
  (slide/c "kept wanting more features")
  
  #;(slide/c "So he had to hire some more developers")
  
  (slide/c "To help them manage")
  
  (slide/money "5,000,000")
  
  (slide/c "But he was still very productive")
  
  (slide/c "So the system handled")
  
  (slide/money "50,000,000")
  
  (slide/c "very nicely")
  
  (slide/c "Then, one day, the suits gave our hero a call")
  
  (slide/c "The suits paid him a lot of money for his application")
  
  #;(slide/c "And he took they money, and retired to Maui")
  
  (slide/c "But then the suits took a look at all the code")
  
  (slide/c "They said \"Some of this code is very important!\"")
  
  (slide/c "\"We need assurance that the key portions of this code are safe!\"")
  
  (slide/c "So, they rewrote the whole application in C++")
  
  (slide/center #;"aaaaaaaaaaaaaaaaah!"
                (big (face 'unhappiest)))
  
  (slide/center 
   (page-para "How can we avoid this (all-too-common) result?")
   'next
   (page-para "How can we statically check parts of our programs - without rewriting them?"))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;          Overview            ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (header "Overview")
  
  (slide/title
   "Goals"
   (page-item "Migrate a program in a dynamic language by adding some static checking")
   'next
   (page-item "Don't rewrite the whole thing")
   'next
   (page-item "Use the same language everywhere")
   'next
   (page-item "Continue maintaining the code")
   'next
   (page-item "Be sure of what we get in the end"))
  
  (slide/title 
   "Assumptions"
   (page-item "All code is in modules") 'next
   (page-item "Each module can be typed independently") 'next
   (page-item "We have a type system that can check lots of the code") 'next
   (page-item "We add types a module at a time"))
  
  
      
  (slide/title/center
   "Migration"
   (mk-diag untyped untyped #f #f)
   (t "A system built out of untyped modules")
   )
  
  (slide/title/center
   "Migration"
   (mk-diag typed typed #f #f)
   (t "Add types to some of the modules")
   )
  
  (slide/title/center
   "Migration"
    (mk-diag typed typed #t #f)
    (t "Untyped code depending on typed code")
   )
  (slide/title/center
   "Migration"
   (mk-diag typed typed #f #t)
   (t "Dependencies go both ways")
   )
  
  (with-steps (zero a b c)
     (let ([after-a (lambda (x) ((vafter a) x))]
           [after-b (lambda (x) ((vafter b) x))]
           [after-c (lambda (x) ((vafter c) x))])
       (slide/title "Questions"
                    (page-item "What do we check?")
                    (after-a (page-subitem "Precisely what modern type systems can check:"))
                    (after-a (page-subitem "That we don't misapply operations - those we define, or those the language defines"))
                    (page-item "How much code change is acceptable?")
                    (after-b (page-subitem "As little as possible, as much as neccessary"))
                    (page-item "How do we integrate typed and untyped code?")
                    (after-c (page-subitem "Flows in both directions"))
                    (after-c (page-subitem "Callbacks")))))
  
  #;(slide/title "What Gets Checked?"
               (page-para "Precisely what modern type systems can check:") 'next
               (page-para "That we don't misapply operations - those we define, or those the language defines"))
  
  #;(slide/title "Rewriting?"
               (page-para "To be avoided")
               (page-para "We need a type system that checks the code we actually write")
               'next
               (page-para "But sometimes, it can't be avoided"))
  
  #;(slide/title "Integration"
               (page-para "Calls from typed to untyped code")
               'next
               (page-subitem "Anything can happen!")
               'next
               (page-para "Calls from untyped to typed code")
               'next
               (page-subitem "What are those arguments, anyway?")
               'next
               (page-para "Callbacks!"))
  
  (slide/title "How do we do it?"
               (page-para "Specify the language of particular modules")
               'next
               (page-para "Enforce contracts at module boundaries")
               'next
               (page-para "Infer required contracts"))
  
  (slide/title "Modules"
               (page-para "A group of definitions, with explicit export of some of them")
               (page-para "Imports specified explicity")
               (page-para "Internal linking")
               'next
               (blank)
               (page-para "A close resemblance to the {PLT Scheme, Python, Ruby, ...} module systems"))
  
  (slide/title "Modules"
               (page-para "Each module is either typed or untyped")
               (page-para "Typed modules specify the types of their exports")
               (page-para "Either kind of module can refer to the other kind"))
  
  (slide/title "Contracts"
               (page-para "Dynamic checks on steroids")
               (page-para "Allow us to check both data and functions")
               (page-para "Higher-order contracts allow callbacks (and objects) to work in both directions")
               (page-para "Contracts allow richer specifications")
               'next
               (blank)
               (page-para "See [Findler & Felleisen, OOPSLA 2001]"))
  
  (slide/title "Contracts"
               (page-para "When we encounter a boundary-crossing, one of the sides must have a type")
               (page-para "Convert that type to a contract")
               (page-para "Add the contract to the interface of the exporting module"))
  
  
  (define untyped-fact-mod
    (page-para
     (code
      (module interest mzscheme
        (define (interest x)
          (+ x (fast-mul x 0.05)))))))
 
  
  
  (define-syntax typed-code
    (syntax-rules ()
      [(_ . args) (box-around (page-para (code . args)) "blue")]))
  
  (define fact-mod
    (typed-code
     (module interest typed-scheme
       (define: (interest [x : number]) : number
         (+ x (fast-mul x 0.05))))))
    
  (define fast-mul-mod 
    (page-para
     (code
      (module fast-mul mzscheme
        (provide fast-mul)
        #,(ghost (code 3))
        (define (fast-mul a b) (if (zero? a) 0 (* a b)))))))
  (define fast-mul-mod/contract
    (page-para
     (code
      (module fast-mul mzscheme
        (#,code/pc fast-mul 
                   #,(hy (code (number number |.| -> |.| number))))
        (define (fast-mul a b) (if (zero? a) 0 (* a b)))))))
  
  (define (codes . args)
    (list (apply vl-append 15 args)))
  
  (header "Examples")
  
  (slide/title/center 
   "Simple Example"
   'alts
   (list
    (codes fast-mul-mod)
    (codes fast-mul-mod untyped-fact-mod)
    (codes fast-mul-mod fact-mod)
    (codes fast-mul-mod/contract fact-mod))
   'next
   (page-para "But how did we know the type of" (code fast-mul) "?")
   'next
   (page-para "From how" (code fast-mul) "is used in the typed module, we can infer the required type and contract.")
   )

      
  #;(slide/title
   "Inference"
   (page-para "Notice that I didn't ever say what the type of" (code fast-mul) "was.")
   'next
   (page-para "In fact, the system can figure that out from how we use" (code fast-mul) ".")
   'next
   (page-para "What if untyped code is used in multiple ways?"))
  
  (define ai-mod/typed 
    (page-para
     (typed-code
      (module add-interest-mod typed-scheme 
        (require inc-mod interest)
        (define: (add-interest [balance : number]) : number
          (increment (interest balance)))))))
  
  (define ai-mod 
    (page-para
     (code (module add-interest-mod mzscheme 
             (require inc-mod interest)
             (define (add-interest balance)
               (increment (interest balance)))))))
    
  (define inc-mod
    (page-para
     (code
      (module inc-mod mzscheme
        (provide increment)
        (define increment 999)))))
  
  (define inc-mod/contract
    (page-para
     (code
      (module inc-mod mzscheme
        (#,code/pc increment #,(hy (code (number |.| -> |.| number))))
        (define increment 999)))))

  (define main-mod
    (page-para
     (code (module main mzscheme
             (require add-interest-mod)
             (add-interest 10000.0)))))
    
    
  (slide/title/center
   "Contracts that fail"
   'alts
   (list
    (codes ai-mod inc-mod main-mod)
    (codes ai-mod/typed inc-mod main-mod)
    (codes ai-mod/typed inc-mod/contract main-mod))
     'next
     (page-para "Now " (code main) "will fail when run, because" (code increment) "does not meet its contract."))
    
  
  (define line-cloud (cloud 150 30))
  
  (define n-mod/typed
    (page-para
      (typed-code
       (module n-mod typed-scheme
         (require inverse-mod)
         (define: n : number
           (if (not (inverse true))
               (inverse 5)
               7))))))
  
  (define n-mod/untyped
    (page-para
      (code
       (module n-mod mzscheme
         (require inverse-mod)
         (define n
           (if (not (inverse true))
               (inverse 5)
               7))))))

  (define (g-mod pc cnt)
    (page-para 
     (code
      (module inverse-mod mzscheme
        #,(if cnt (code (#,pc inverse #,cnt))
              (code (#,pc inverse)))
        (define (inverse x)
          (if (boolean? x) (not x) (* x -1)))))))


  (define n-mod/casts
    (page-para
     (typed-code
      (module n-mod typed-scheme
        (require inverse-mod)
        (define: n : number
          (if (not #,(hy (code [boolean <= #,(highlight (code (inverse true)) "white")])))
              #,(hy (code [number <= #,(highlight (code (inverse 5)) "white")]))
              7))))))
  
  (define n-mod/proxy
    (page-para
      (typed-code
       (module n-mod typed-scheme
         (require #,(hy (code inverse1 inverse2)))
         (define: n : number
           (if (not (#,(hb-append (code inverse) (hy (code |1|))) true))
               (#,(hb-append (code inverse) (hy (code |2|))) 5)
               7))))))

  (define inverse1/2
    (hy
     (code
      (module inverse1 mzscheme
        (require inverse-mod)
        (provide/contract inverse1 (boolean |.| -> |.| boolean))
        (define inverse1 inverse))
      (module inverse2 mzscheme
        (require inverse-mod)
        (provide/contract inverse2 (number |.| -> |.| number))
        (define inverse2 inverse)))))

  
  (define g-mod1 (g-mod (code provide) #f))
  (define g-mod2 (g-mod code/pc line-cloud))
  (define g-mod3 (g-mod code/pc (hy (code ((or/c boolean number) 
                                           |.| -> |.|
                                           (or/c boolean number))))))
  (define g-mod3/tiny (smalltext (g-mod smallcode/pc 
                                        (hy (t "---")) #;(code ((or/c boolean number) |.| -> |.| (or/c boolean number))))))
  (slide/title/center 
   "Handling incompatible uses"
   'alts
   (list
    (codes n-mod/untyped g-mod1)
    (codes n-mod/typed g-mod1)
    (append
    (codes n-mod/typed g-mod2)
     (list      
      (page-para "What contract could we add to" (code inverse) "?")))
    (append 
     (codes n-mod/typed g-mod3)
     (list
      'next
      (page-para "But that's insufficient for safety")
      'next
      (page-para (code (define (inverse x)
                         (if (boolean? x) 1 true))))))
    
    (append
     (codes n-mod/casts g-mod3)
     (list
      (page-para "Adding casts recovers safety")
      'next
      (page-para "Can we avoid casts?")))
         
    (codes
     n-mod/proxy inverse1/2 g-mod3/tiny)))
      
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;          Technical Stuff     ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (header "Theoretical Contributions")
  
  (slide/title "Modeling our system"
               (page-para "Start with the λ-calculus with numbers")
               'next
               (page-para "Add modules and contracts")
               'next
               (page-para "Add simple types and typed modules")
               'next
               (page-para "Define a migration process with inference"))
  
  (slide/title "Theorems"
               (page-para "What can we prove about such a system?")
               'next
               (page-subitem "Programs in the untyped portion can go wrong")
               (page-subitem "But the typed portions should be safe")
               'next
               (page-para "Use the blame annotations from contracts to track where errors occur")
               (page-para "Prove that all runtime type errors are blamed on untyped code"))
  
  #;(slide/title "Limits of our model"
               (page-para "The λ-calculus with numbers is a pretty simple language")
               (page-para "Our type system isn't very rich")
               (page-para "Our modules are very simple"))
  
  #;(slide/title "What does our model tell us?"
               (page-para "We can write mixed programs")
               (page-para "And get a form of type-safety")
               'next
               (page-para "We believe our model will scale to real languages and type systems"))
  
  #;(slide/title/center "New Type System work"
               (page-para "Developing a new type system that will allow us to check existing Scheme programs"))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;          End Matter          ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title "Contributions"
               (page-para "Theoretical Contributions")
               (page-subitem "A solid foundation for interlanguage migration")
               (page-subitem "Reformulating type soundness for mixed programs")
               'next
               (page-para "Practical Contributions")
               (page-subitem "A framework for designing systems")
               'next
               (page-subitem "An implementation of the system for PLT Scheme"))
  
  #;(slide/title "Next Steps"
               (page-para "Developing type systems for existing dynamic languages")
               (page-para "Implementing our system")
               (page-para "Porting existing code"))
  
  (slide/title "Related Work"
               (page-para "Soft Typing")
               (page-subitem "Fagan, Wright, Henglein, Flanagan, Meunier, Aiken, and many more")
               'next
               (page-para "Type Dynamic")
               (page-subitem "Abadi et al, Siek & Taha, Baars & Sweirstra, Leroy & Mauny")
               'next
               (page-para "Type systems for dynamic languages")
               (page-subitem "Strongtalk [Bracha], Erlang [Marlow & Wadler]")
               )
  
  (slide/title "Conclusion"
               (page-para "We can avoid C++ and keep using our languages")
               (page-para "Modular migration of programs allows for flexibility")
               (page-para "Need for new type systems to support dynamic languages")
               'next
               (page-subitem "Create one for your favorite language!"))
  
  ;; last slide
  (slide/center (t "Thank You")
                (colorize (tt "http://www.ccs.neu.edu/home/samth") "blue"))
  
  
  
  )