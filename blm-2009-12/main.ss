#lang slideshow

(require slideshow/step slideshow/code slideshow/face (only-in slideshow/slide title-size)
         (except-in "beamer.ss" title) "lib.ss" scheme/gui "class-slide.ss" "thanks.ss" "ts-intro.ss"
         "contracts.ss" "intro.ss" "tslide.ss" "config.ss" "occur-seq.ss"
         "scheme07/talk.ss"
         scheme/runtime-path mzlib/etc (planet cce/scheme:6/slideshow))

(title '("Types in Scheme, for Scheme")
       '()
       '(("Sam Tobin-Hochstadt" "Northeastern University"))
       "Boston Lisp Meeting")
(set-page-numbers-visible! #t)



;OUTLINE:

;Motivation
; - Growth of Scripting Languages

; - Problems of Scripting Languages
; - Types as maintenence tool

(class-slide #:title "Why Types?")

(slide #:title "Typed Scheme is"
       (para "A typed dialect of Scheme")
       (para "A combination of \"dynamic\" and \"static\"")
       (para "A type system that thinks like you")
       (para "A case study in macrology")
       'next
       (blank 10 50)
       (para "Available Now!"))


;Thesis
; Module-by-module porting of code from an untyped language to a
; typed sister language allows for an easy transition from untyped
; scripts to typed programs.

; - What does this mean: 
;  * Module-by-module
;  * Typed sister language
;  * Easy transition

;; why Scheme?

;; Intro to TS

(ts-intro)

; - Technique for interlanguage interop
;  * With proof of soundness/blame thm


; - Type system for Scheme
;  * Occurrence Typing
;  * Variable-arity Polymorphism
;  * Refinement Types
;  * Local type inference


 


;Details of Support #1: Interlanguage Interop

(multi-sound)

; - The problem
;  * Untyped code can violate type invariants
;  * Two directions
;  * Higher-order
; - The solution
;  * Contracts
;  * Blame
;  * Boundaries/Interfaces
; - The theorem
;  * Intuition
;  * Formal Statement
;  * Why should you care?

;Details of Support #2: Occurrence Typing

(tslide "Types for Scheme"
        (list "Occurrence Typing" "Variable-Arity")
        (list "Ad-Hoc Data" "Refinement Types"))
(define forall (lift-above-baseline (code ∀) -3))


(define tree-code
  (code
   (code:comment "a tree is either a number")
   (code:comment "or a pair of trees")
   (define (sum t)
     (cond [(pair? t) (+ (sum (car t))
                         (sum (cdr t)))]
           [else t]))))

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





#|
(slide/stage 
 [occur  refine union varar #;local]
 #:title "Types for Scheme"
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
     (define-type-alias BT 
       (U Number (Pair BT BT)))
     (: sizeof (BT -> Number))
     (define (sizeof b)
       (if (number? b)
           1
           (+ 1 (sizeof (car b)) (sizeof (cdr b))))))])))

(parameterize ([current-keyword-list (cons "+" (current-keyword-list))])
  (occur-seq))

(slide #:title "All Together Now"
       (tmod
        (code
         (: f ((U Number String) (Pair Any Any) -> Number))
         (define (f input extra)
           (cond
             [(and (number? input) (number? (car extra)))
              (+ input (car extra))]
             [(number? (car extra))
              (+ (string-length input) (car extra))]
             [else 0])))))
|#
; - Importance of type system for Scheme
; - Example Scheme idioms (number?, member, else, sexpr)
; - Supporting these idioms:
;  * True Unions
;  * Explicit recursive types
;  * Occurrence typing
; - number? or string? example
;  * Show both true and false filters, and objects

;Details of Support #3: Typed Scheme Software + Validation
(plt-lang)

(ts-impl)

(tslide "Easy Integration"
        (list "Implementation" "Validation"))



;Support of Thesis
; - Full-scale implementation
;  * Integrated into PLT Scheme
;  * Implemented using PLT macros
;  * Numerous special-purpose features

(define big-mod
  (code
   (provide (rename-out
             [module-begin #%module-begin]))
   (define-syntax (module-begin stx)
     (set! typed-context? #t)
     (let ([expanded
            (local-expand stx 'module-begin null)])
       (typecheck expanded)
       expanded))))

(slide/stage 
 [integrate impl]
 #:title "Implementation" #:layout 'center
 (case stage-name
   [(integrate)
    (bitmap "large-letters.png")]
   [(impl)
    (tmod #:name "tslide"
     (code (: subtitle-pict : (String -> Pict))
           (define (subtitle-pict s)
             (text s (current-title-font) large-text-size))))]))



; - Experimental Validation
;  * Ported existing scripts
;  * Numeric Results
;  * Used in classes


(define (table-slide)
  
  (define left-col
    (tabular #:gap 12
             (list "Lines")
             (list "Increase")
             (list "Fixes (Good)")
             (list "Problems (Bad)")))
  
  (define (mytt txt header)
    (rb-superimpose
     (ghost (t header))
     (if (pict? txt) 
         txt
         ((current-code-tt) txt))))
  (define (mytts . txts)
    (map mytt txts headers))
  
  (define headers (list "Squad" "Metrics" "Acct" "Spam" "System" "Rand" "     Total"))
  
  (define top-row
    (tabular #:gap 12 headers))
  
  (define 6-1-8 (rb-superimpose (ghost (t "Rand")) ((current-code-tt) "618")))
  (define one ((current-code-tt) "1"))
  
  (slide 
   #:layout 'center
   #:title "Validation"
   (hbl-append
    left-col
    (blank 20);(vline 20 (pict-height left-col))
    (vl-append
     top-row
     (blank 20);(hline (pict-width top-row) 20)
     (frame 
      (hc-append 
       12
       (pin-line
        (tabular #:gap 12
        (mytts "2369"  "511"     "407"  "315" "1290" 6-1-8 "5510")
        (mytts "7%"    "25%"     "7%"   "6%"      "1%" "3%" "7%")
        (mytts "5"     "3"       "4"     "5"      "8" "0"  "25") 
        (mytts "7"     "4"       "3"     "1"      "0" one  "16"))
        6-1-8
        (move-find-left rt-find -35)
        one
        (move-find-left rb-find -35))
       (blank 10)))))))

(table-slide)


(staged
 [assert assert* div div*]
 (slide 
  #:title "Sample Fixes"
  (let ([body (pict-case 
               stage-name
               [(assert) (code (+ 10 (string->number str)))]
               [(assert*) (code (+ 10 (#,(red-code assert) (string->number str))))]
               [(div) (code (define (divs . args) 
                              (* -1 (apply / args))))]
               [(div*) (code (define (divs #,(red-code arg) . args)
                               (* -1 (apply / #,(red-code arg) args))))])])
    (case stage-name
      [(assert div) (smod #:name (symbol->string stage-name)
                          body)]
      [(assert*) (tmod #:name "assert"
                       body)]
      [(div*) (tmod #:name "div"
                    body)]))))

(slide/stage 
 [cond cond* mutate mutate*]
 #:title "Sample Problems"
 (let ([body (pict-case 
              stage-name
              ;; NB: `x' must be exact for this to work
              [(cond) (code (cond [(< x 0) 'negative]
                                  [(= x 0) 'zero]
                                  [(> x 0) 'positive]))]
              [(cond*) (code (cond [(< x 0) 'negative]
                                   [(= x 0) 'zero]
                                   [#,(red-code else)    'positive]))]
              [(mutate) (code (define pr (make-pair x y))
                              (when (string? (pair-left pr))
                                (set-pair-left! pr (string->symbol (pair-left pr)))))]
              [(mutate*) (code (define pr (make-pair 
                                           (if (string? x) (string->number x) x)
                                           y)))])])
   (case stage-name
     [(cond mutate) (smod #:name (symbol->string stage-name)
                          body)]
     [(cond*) (tmod #:name "cond"
                    body)]
     [(mutate*) (tmod #:name "mutate"
                      body)])))

;(tslide "Empirical Validation")

; - Implemented and Available
; - Validation
;  * Ported existing PLT software
;  * Measured required changes
;  * Show table
;  * Discuss major changes

;Related Work


; - Interlanguage integration
; - Types for untyped languages
; - Prior work on contract/modules
; - Gradual Typing



(thanks (list "Matthias Felleisen" "Ryan Culpepper" "Stevie Strickland" "Eli Barzilay" "Matthew Flatt" "Robby Findler"))

