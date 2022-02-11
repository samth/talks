(module slam (lib "slideshow.ss" "slideshow")
  (require 
           (lib "etc.ss")
           (lib "list.ss")
           (lib "code.ss" "slideshow")
           (lib "step.ss" "slideshow"))
  (define big
    (opt-lambda (str (color green))
      (colorize (text str (current-main-font) (* 2 (current-font-size)))
                color)))
  
  (define (subscript a txt)
    (hb-append a (colorize (text txt `(bold . modern)  (/ (current-font-size) 2)) green)))
  
  (define (secprop one two) (format "(~a,~a)" one two))
  
  (define (sub a one two)
    (subscript a (secprop one two)))
  
  (define H 'H)
  (define L 'L)
  
  (current-keyword-list 
   (append (filter (lambda (x) (not (equal? "," x))) (current-keyword-list) )
           '("cons" "proj" "inj"  "protect" "unit" "*" "+"  "->" ":" "rec" "|" "of")))
  
  (define ltrue (sub (code #t) L L))
  (define htrue (sub (code #t) H H))
  (define lfalse (sub (code #f) L L))
  (define hfalse (sub (code #f) H H))
  (define (alt-slide . args)
    (slide/center 'alts (map list args)))
  
  (code-scripts-enabled #t)
  
  (slide/center
   (titlet "The SLam Calculus")
   (blank)
   (t "Nevin Heintze and Jon Riecke")
   (blank)
   (text "Presented by: Sam Tobin-Hochstadt")
   (ht-append (text "CSG 399") (ghost (t "blank")) (text "April 27, 2006")))
  
  ;; motivation
  
  (with-steps 
   (one two three four)
   (slide/center
    (cond [(only? two) (big "BAD!" red)]
          [(only? four) (big "Rejected by typechecker" green)]
          [else (ghost (big "foo" red))])
    (before three 
            (code (print-to low-security-port 
                            (if (read high-security)
                                #t #f)))
            (code (print-to low-security-port 
                            (if (read #,(sub (code high-security) H H))
                                #,ltrue #,lfalse))))))
  
  (define (tslide str) (slide/center (titlet str))) 
  
  ;; overview
  
  (slide/center 
   (titlet "Fundamental Idea:")
   'next
   (titlet "Add security properties to types"))
  
  (alt-slide (titlet "Security properties:")
             (page-para/c "Who can read this data")
             (page-para/c "Who can be influenced by this data"))
  
  (define (as . args)
    (apply alt-slide
           (cons (titlet (car args))
                 (map (lambda (x) 
                        (cond [(string? x) (page-para/c x)]
                              [(list? x) (apply page-para/c x)]
                              [else x]))
                      (cdr args)))))
  
  (as "(r,ir)"
      (list (code r) ": readers")
      (list (code ir) ": indirect readers")
      "readers can directly inspect an object"
      "indirect readers can be given acess to some information about an object")
  
  (as "Security Levels"
      "We will just consider two"
      (code L)
      (code H)
      "More complex: unix users and groups, roles, etc")
  

  ;; formalism section
  
  (tslide "The Lambda Calculus")
  
  
  (alt-slide
   (page-para/c "Functions:" (code (lambda (x) e)))
   (page-para/c "Variables:" (code x))
   (page-para/c "Function Application:" (code (e_1 e_2)))
   (titlet "That's it!")
   )
  
  (tslide "But not quite ...")
  
  (alt-slide
   (page-para/c "Pairs:" (code (cons e_1 e_2)))
   (page-para/c "Projection:" (code (proj_i e)))
   (page-para/c "Recursive Functions:" (code (rec f e)))
   (page-para/c "Case:" (code (case e of (inj_1 e_1) $ (inj_2 e_2)))))
  
  (tslide "Now we add types ...")
  
  (tslide "Where do types go?")
  
  (alt-slide
   (page-para/c "Functions:" (code (lambda (x : s) e)))
   (page-para/c "Recursive Functions:" (code (rec f : s e)))
   )
  
  (tslide "What are types?")
  
  (alt-slide
   (page-para/c "Unit:"  (code t = ()))
   (page-para/c "Sum Type:"  (code $ (s + s)))
   (page-para/c "Product Type:"  (code $ (s * s)))
   (page-para/c "Function Type:"  (code $ (s -> s)))
   )
  
  (tslide "But what was s?")
  
  (tslide "Security Properties")
  
  (slide/center
   (page-para/c "Security properties:" (code k =) (secprop 'r 'ir))
   'next
   (page-para/c "Types:" (code s =) (secprop 't 'k))
   )
  
  (as "Now we use these in the grammar"
      (list "Basic values: " (code bv = () $ (inj_i v) $ (cons v v) $ (lambda (x : s) e)))
      (list "Labeled Values: " (code v = #,(sub (code bv) "r" "ir")))
      (list "Labled Constructors: " (sub (code (inj_i e)) "r" "ir"))
      (list "Labeled Destructors: " (subscript (code (e e)) "r"))
      )
  
  (define comma ((current-code-tt) ","))
  
  (as "One More Construct")
  
  (slide/center 
   (code (protect_ir e))
   'next
   (page-para/c "Increases the security level on e"))
  
  (tslide "How Do We Check?")
  
  (tslide "Constructors:")
  
  (slide/center
   (code #,(subscript (code (cons e_1 e_2)) "k") : (s_1 * s_2 #,comma k))
   'next
   (page-para/c "provided " (code e_1 : s_1) "and" (code e_2 : s_2))
   )
  
  (slide/center
   (code #,(subscript (code (inj_i e)) "k") : (s_1 + s_2 #,comma k))
   'next
   (page-para/c "provided " (code e : s_i))
   )
    
  (tslide "Destructors:")
  
  (slide/center
   (code #,(subscript (code (e_1 e_2)) "r*") : s_2 #,bullet ir)
   'next
   (page-para/c "provided " (code e_1 : (s_1 -> s2) #,comma (r #,comma ir)) "and" (code e_2 : s_1))
   'next
   (page-para/c "and " (code r <= r*))
   )
  
  (slide/center (hb-append (titlet "Wait, what was that ") bullet))
  
  (slide/center (code s #,bullet ir)
                'next
                (page-para/c "means increase the security of" (code s) "to " (code ir)))
  
  (slide/center (titlet "So checking a destructor involves 3 things:")
                'next
                (page-para/c "That the basic types match")
                'next
                (page-para/c "That the reader has sufficent access to read the value")
                'next
                (page-para/c "That the result has the appropriate indirect security"))
  
  (tslide "And that's it!")
  
  (slide/center (titlet "We can now prove non-interference:")
                'next
                (page-para/c "That is, the result of a (low-security) program does not depend on its high-security portions"))
  
  (slide/center (titlet "Also, we can prove an erasure property:")
                'next
                (page-para/c "We don't need to do any security checking at runtime"))

  (tslide "Some Weaknesses")
  
  (tslide "Nontermination - not considered")
  
  (slide/center
   (code (let ([halt-if-true 
                (lambda (x : #,(sub (code bool) 'H 'H))
                  (if x #,(sub (code ()) 'H 'H)
                      (halt-if-true x)))])
           (halt-if-true secret-bool)
           #,ltrue)))
  
  (tslide "Timing - not considered")
  
  (slide/center
   (code (let* ([t1 : #,(sub (code int) 'L 'L) (get-time)]
                [tmp (if secret-bool 
                         (long-comp)
                         (short-comp))]
                [t2 : #,(sub (code int) 'L 'L) (get-time)])
           (> (- t2 t1) time-for-short-comp))))
  
  (tslide "Extending the system")
  
  (tslide "Mutation")
  (tslide "and Concurrency")
  (as "and Bears, Oh My!"

   "Everything is harder with concurrency")
  
  (as "Add reference cells, and spawn to the language"
      (code (box e))
      (code (set-box! e_1 e_2))
      (code (unbox e))
      (code (spawn_ir e)))
  
  (as "Add reference types"
      (code (ref s)))
  
  
  (as "Add an effect system to the type system"

      "Add latent effects to function types"
      "Basically, what will happen when this function runs")
  
  (as "Also take into account the context of actions")
  
  (slide/center
   (code (set-box! e_1 e_2) : s)
   (page-para/c "in context" (code ir))
   'next
   (page-para/c "if" (code e_1 : (ref s)) "and" (code e_2 : s))
   'next
   (page-para/c "and" (code s #,bullet ir = s))
   )
  
 (slide/center
   (code (let ([halt-if-true 
                (lambda (x : #,(sub (code bool) 'H 'H))
                  (if x #,(sub (code ()) 'H 'H)
                      (halt-if-true x)))])
           (halt-if-true secret-bool)
           (set-box! y #,ltrue)))
   'next
   (hb-append (titlet "Only high-security readers can read ") (code y)))
  
  (tslide "Non-termination still not fixed")
  
  (as "No non-interference result"
      "What is non-interference for parallel systems?"
      "\"Timing attacks\" are part of the point")
  
  (as "Integrity checking"
      "Adds creators and indirect creators"
      "So now security properties are a 4-tuple"
      "I'll spare you the details"
      "But this system obeys non-interference")

  (as "Conclusions"
      "We can apply standard PL techniques to design a secure language"
      "And to prove theorems about it")
  
  (tslide "Questions?")

  )
