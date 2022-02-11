#lang racket
(require slideshow "lib.rkt" (except-in "beamer.rkt" title) "tslide.rkt" "config.rkt")

(title '("Modern PL Research") '() '(("Sam Tobin-Hochstadt" "")))

(slide #:title "The Goal"
       (para "Using language technology to make program[ming/s]:")
       (blank 40)
       (item "Fast")
       (item "Expressive")
       (item "Correct")
       'next
       (item "Maintainable"))

(slide (titlet "A word about mathematics"))

(slide #:title "Conferences"
       'alts
       (list
        (list
         (item "POPL")
         (item "PLDI")
         'next
         (item "ICFP")
         (item "OOPSLA")
         (item "ESOP")
         (item "ECOOP"))
        (list
         (item "ASPLAS")
         (item "CC")
         (item "LCTES")
         (item "TACAS")
         (item "PPDP")
         (item "LICS"))
        (list
         (item "FSE")
         (item "ICSE")
         (item "ASE")
         (item "VMCAI")
         (item "SAS"))))

(tslide "(Some) Topics")

(define (mk s topic)
  (slide (scale (titlet s) 1.5)
         'next
         (blank 50)
         (titlet topic)))

(define fast-topics
  (map
   (λ (e) (list e "Fast"))
   '("Garbage Collection"
     "Software Transactional Memory"
     "Lock-free algorithms"
     "Compiler Optimization"
     "Type Reconstruction"
     "Memory Models"
     "Parallel programming constructs"
     "JIT Compilation"
     "Dynamic adaptation"
     "Dynamic languages"
     "Partial Evaluation"
     "Functional Data Structures")))

(define expressive-topics  
  (map
   (λ (e) (list e "Expressive"))
   '("Macros"
     "Modules"
     "Mixins"
     "Reflection"
     "Traits"
     "Continuations"
     "Monads/Arrows"
     "Type Classes"
     "Functional Reactive Programming"
     "Generalized Algebraic Data Types"
     "GUIs"
     "Parsing"
     "Type Inference"
     "Domain-Specific Languages"
     "Naming/Binding"
     "Databases"
     "Concurrency & Synchronization"
     "Program Synthesis/Derivation")))

(define correct-topics  
  (map
   (λ (e) (list e "Correct"))
   '("Dependent Types"
     "Ownership types"
     "Typestate"
     "Contracts"
     "Type theory"
     "Theorem Proving"
     "Program Analysis"
     "Actors"
     "Testing"
     "SAT Solvers"
     "Symbolic Execution"
     "Security"
     "Information Flow"
     "Linear Types"
     "Lenses"
     "Logical Relations"
     "Compiler Correctness"
     "Semantics"
     "Separation Logic"
     "Sandboxing")))

(define maintainable-topics  
  (map
   (λ (e) (list e "Maintainable"))
   '("User Interfaces"
     "Error Reporting"
     "Refactoring"
     "Gradual Types"
     "Software Product Lines")))
  
(define topics (shuffle (append fast-topics expressive-topics correct-topics maintainable-topics)))
(for-each (λ (e) (apply mk e)) topics)

