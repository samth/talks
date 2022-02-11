#lang slideshow

(require slideshow/step slideshow/code slideshow/face (only-in slideshow/slide title-size))

(require "beamer.ss"
         "utils.ss")

(define start-at-recent-slide void)


(define (subpara str)
  (hc-append (blank (* 2 gap-size))
                  (para #:width (- (current-para-width) (* 2 gap-size))
                        str)))

(current-keyword-list (list* "define:" "let:" "require/typed" "require/contract" ":" "->" 
                             "provide/contract" "..." "define-type-alias" "define-struct:"
                             (current-keyword-list)))

(define (red e) (colorize (t e) "red"))

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
    (block (mod/lang "typed-scheme   " . args)))

(define-syntax-rule (red-block . args)
  (parameterize ([current-block-background-color "Tomato"])
    (block . args)))

(define-syntax-rule (smod . args)
  (parameterize ([current-block-background-color "white"])
    (block (mod/lang "scheme         " . args))))
  
(title "Typed Scheme")
(subtitle "Bringing Types to Untyped Languages")
(author "Sam Tobin-Hochstadt" "Northeastern University" #f)

#;
(with-steps (a b)
  (slide #:layout 'center
         (if (before? b)
             (titlet "Typed Scheme: Bringing Types to Untyped Languages"))
         (t "Sam Tobin-Hochstadt")))
  
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
                         (bitmap "lua.png")))
             'nothing)))

;;(start-at-recent-slide)

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

;;(start-at-recent-slide)

(slide #:title "What's not so good"
       (para "Always code as if the guy who ends up maintaining your code will be a violent psychopath who knows where you live.")
       (para "      - John F. Woods")       
       )

(define (class-code #:super [super #f] #:comment [comment #f])
  (define c
    (code 
     #,(if comment (code (code:comment "Start here:")) (code ||))
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
        (list (class-code))
        (list (class-code #:comment #t))
        
        (list (class-code #:super #t #:comment #t)))
       )
                                    
(slide #:title "100,000 lines of Perl (or Scheme)?"
       (para "So, you built your business around a script...")
       (para "What now?")
       (blank 10)
       'next
       (para "You need to:")
       (subitem "Improve maintainability")
       (subitem "Keep the system running")
       (subitem "Without throwing away code")
       )
#;
(slide #:title "Program Evolution"
       
       (para "How can we make our script evolve into a mature program?")
       )

(slide #:title "Thesis"
       'alts
       (list
        (list
         (para "Module-by-module porting of code from an untyped language"
               "to a typed sister language allows for an easy transition"
               "from untyped scripts to typed programs."))
        (list
         (para (red "Module-by-module") "porting of code from an untyped language"
               "to a typed sister language allows for an easy transition"
               "from untyped scripts to typed programs."))
        (list
         (para "Module-by-module porting of code from an untyped language"
               "to a" (red "typed sister language") "allows for an easy transition"
               "from untyped scripts to typed programs."))
        (list
         (para "Module-by-module porting of code from an untyped language"
               "to a typed sister language allows for an" (red "easy transition")
               "from untyped scripts to typed programs."))))


;(start-at-recent-slide)
(tslide "Modular porting")

(slide #:title "Modular porting"
       (para "Individual modules are either typed or untyped")
       (blank 10)
       (para "Typed modules are protected from the untyped modules"))

(define t-sq (code 
              (: sq (Number -> Number))
              (define (sq x) 
                (* x x))))

(define provide-sq (code sq))
(define use-sq (code sq))

(define t-sq/prov (code #,t-sq
                        (provide #,provide-sq)))

(define s-sq (lb-superimpose (ghost t-sq) (code (code:contract sq : Number -> Number) 
                                                (define (sq x) 
                                                  (* x x)))))

(define s-sq/wrong
  (lb-superimpose (ghost t-sq) (code (code:contract sq : Number -> Number) 
                                     (define (sq x) 
                                       "whoops!"))))
(define s-sq/prov (code #,s-sq
                        (provide sq)))

(define s-sq/prov-wrong (code #,s-sq/wrong
                              (provide sq)))

(define hello (lt-superimpose (ghost t-sq) (code (display "Hello World"))))

(define sz (code (sq "eleven") (code:comment "=> contract violation (???)")))

(slide #:title "Simple modules"
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

(define multi-mod-1
  (vl-append
   (tmod #:name "arith" #:sizeof sz
         t-sq/prov)
   (blank 10)
   (lt-superimpose (ghost (tmod #:sizeof sz (code 1
                                                  2
                                                  3)))
                   (tmod #:name "run" #:sizeof sz
                         (code (require arith)
                               (#,use-sq 11) (code:comment "=> 121"))))))
(define NumNum1 (code cplx))
(define NumNum2 (code cplx))
(define NumNum3 (code cplx))

(define multi-mod-2
  (vl-append
   (tmod #:name "cplx-ty" #:sizeof sz
         (code (define-struct: cplx 
                 ([r : Number] [i : Number]))
               (provide #,NumNum1)
               | |))
   (blank 10)
   (tmod #:name "arith" #:sizeof sz
         (code (require cplx-ty)
               (: sq (#,NumNum2 -> #,NumNum3))
               (define (sq x) #,(cloud 100 30))))))

(start-at-recent-slide)

(define (cs-line p start end)
  (pin-arrow-line
   25 p start cc-find end cc-find #:color "red" #:line-width 5))

(slide #:title "Multiple Modules" #:layout 'center
       'alts
       (list 
        (list multi-mod-1)
        (list 
         (cs-line multi-mod-1 provide-sq use-sq))
        (list 
         multi-mod-2)
        (list
         (cs-line (cs-line multi-mod-2 NumNum1 NumNum2) NumNum1 NumNum3)
         )))


(define t-addx (code (: addx (Number -> (Number -> Number)))
                     (define ((addx x) y) (+ x y))))
(define s-addx (code (code:contract addx : Number -> Number -> Number)
                     (define ((addx x) y) (+ x y))))

(let* (
       [sz-foo (code (require/typed addx 
                          [addx (Number -> (Number -> Number))]))]
       [sz-long (code ((addx 5) "six") (code:comment "=> contract violation (runx)"))]
       [p0-t (tmod #:name "addx" #:sizeof sz-long
                   t-addx
                   (code (provide addx)))]
       
       [p0-s (smod #:name "addx" #:sizeof sz-long
                   s-addx
                   (code (provide addx)))]
       #;[p0 (tmod #:name "addx"
                 t-addx
                 (red-code (provide/contract 
                            [addx (-> Num? (-> Num Num?))])))]
       (p1 
        (vl-append
         (tmod #:name "arith" #:sizeof sz-long
               t-sq/prov)
         (blank 10)
         (smod #:name "run" #:sizeof sz-long
               (code (require arith))
               (code (sq 11))
               (code (sq "eleven")))))     
       (p2 
        (vl-append
         (tmod #:name "arith" #:sizeof sz-long
               t-sq/prov)
         (blank 10)
         (smod #:name "run" #:sizeof sz-long
               (code (require arith))
               (code (sq 11))
               (code (sq "eleven") (code:comment "=> contract violation (run)")))))
       (p3
        (vl-append
         (smod #:name "arith" #:sizeof sz-long
               s-sq/prov)
         (blank 10)
         (tmod #:name "run" #:sizeof sz-long
               (code (require/typed arith
                         [sq (Number -> Number)]))
               (code (sq 11))
               (code (sq "eleven") (code:comment "=> type error")))))
       (p3-last
        (vl-append
         (smod #:name "arith" #:sizeof sz-long
               s-sq/prov-wrong)
         (blank 10)
         (tmod #:name "run" #:sizeof sz-long
               (code (require/typed arith
                         [sq (Number -> Number)]))
               (code (sq 11) (code:comment "=> contract violation (arith)"))
               (code ||))))
       (p3*
        (vl-append
         (smod #:name "arith" #:sizeof sz-long
               s-sq/prov)
         (blank 10)
         (tmod #:name "run" #:sizeof sz-long
               (code (require/typed arith
                         [sq (Number -> Number)]))
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
               (code ((addx 5) "six") (code:comment "=> contract violation (runx)")))))
       (p3-3
        (vl-append
         p0-s
         (blank 10)
         (tmod #:name "runx" #:sizeof sz-long
               sz-foo
               (code ((addx 5) 6))
               (code ((addx 5) "six") (code:comment "=> type error"))))))
  
  (slide #:title "Integration with Untyped Code"  
         #:layout 'center
         'alts
         (list

          (list 
           (blank 8)
           (para "Untyped to Typed")
           p3*)
          (list 
           (blank 8)
           (para "Untyped to Typed")
           p3)
          (list 
           (blank 8)
           (para "Untyped to Typed")
           p3-last)
          (list 
           (blank 8)
           (para "Typed to Untyped")
           p1)
          (list 
           (blank 8)
           (para "Typed to Untyped")
           p2)))
  
  (slide #:title "Higher-order Integration"  
         #:layout 'center
         'alts
         (list  
          (list
           (para "Untyped to Typed - Higher Order")
           p3-3)
          (list
           (para "Typed to Untyped - Higher Order")
           p3-2)
          ))
    )

(slide #:title "Soundness"
       (para "Contract errors blame untyped modules")
       (blank 10)
       'next
       (para "If"(code e)" is a program with typed modules"(code T)" and untyped modules"(code U)", then if execution"
             "of"(code e)" signals an error concerning primitive operations, that error blames some"
             "untyped module" (code U_i) "."))
             
;(start-at-recent-slide)

(tslide "Typed Scheme")

(slide #:title "A Typed Sister Language"
       'alts
       (list
        (list
         (para "PLT Scheme programmers do not write with any particular type system in mind.")
         (para "So Typed Scheme must capture their informal reasoning."))
        (list
         (para "How do we accomodate existing Scheme programs?")
         (subitem "Programming Idioms: Unions, Occurrence Typing")
         (subitem "Linguistic Features: Varargs, Keywords"))))



;(start-at-recent-slide)

(define bg-sz (code (code:comment #,(hc-append (colorize ((current-code-tt) "find : X Listof[Cons[X,Y]] -> ")
                                                       (current-comment-color))
                                             (red-code #f or Y)))))


(define fty (code (: find 
                     (∀ (X Y) (X (Listof (Pair X Y)) -> (U #f Y))))))






(define (occur1)
  #;
  (slide #:title "Idioms"
         (para "Idioms are especially important for Scheme")
         (blank 10)
         (smod #:name "idiom" #:sizeof fty
               (code 
                #,(ghost bg-sz)
                | |
                (define (find x l)
                  (cond [(assq x l) => cdr]
                        [else #f])))))
  (slide #:title "True Unions"
         (para " ")
         (blank 10)
         'alts
         (list
          (list (smod #:name "idiom" #:sizeof fty
                      (code 
                       #,(ghost bg-sz)
                       | |
                       (define (find x l)
                         (cond [(assq x l) => cdr]
                               [else #f])))))
          (list (smod #:name "idiom" #:sizeof fty
                      (code 
                       #,bg-sz
                       | |
                       (define (find x l)
                         (cond [(assq x l) => cdr]
                               [else #f])))))
          (list (tmod #:name "idiom" 
                      (code 
                       #,fty
                       (define (find x l)
                         (cond [(assq x l) => cdr]
                               [else #f]))))
                 (code ||))))
  (slide #:title "Occurrence Typing"       
         (para " ")
         (blank 10)
         'alts
         (list
          (list
           (tmod #:name "idiom" #:sizeof fty
                 (code 
                  #,fty
                  (define (find x l)
                    (let ([tmp (assq x l)])
                      (if tmp (cdr tmp) #f)))))
           (ghost (code foo)))
          (list
           (tmod #:name "idiom"
                 (code 
                  #,fty
                  (define (find x l)
                    (let ([tmp #,(red-code (assq x l))])
                      (if tmp (cdr tmp) #f)))))
           (code (U #f (Pair X Y))))
          (list
           (tmod #:name "idiom"
                 (code 
                  #,fty
                  (define (find x l)
                    (let ([tmp (assq x l)])
                      (if #,(red-code tmp) (cdr tmp) #f)))))
           (code (U #f (Pair X Y))))
          (list
           (tmod #:name "idiom"
                 (code 
                  #,fty
                  (define (find x l)
                    (let ([tmp (assq x l)])
                      (if tmp (cdr #,(red-code tmp)) #f)))))
           (code (Pair X Y)))
          (list
           (tmod #:name "idiom"
                 (code 
                  #,fty
                  (define (find x l)
                    (let ([tmp (assq x l)])
                      (if tmp #,(red-code (cdr tmp)) #f)))))
           (code Y))
          (list
           (tmod #:name "idiom"
                 (code 
                  #,fty
                  (define (find x l)
                    (let ([tmp (assq x l)])
                      #,(red-code (if tmp (cdr tmp) #f))))))
           (code (U #f Y))))))

(define-syntax-rule (red-code . e)
  (parameterize ([code-colorize-enabled #f])
    (colorize (code . e) "red")))



;;(start-at-recent-slide)
;rt-at-recent-slide)

(define (fn #:circle? [circle? (code circle?)]
            #:cr [cr (code (circle-radius s))]
            #:s [s (code s)]
            #:typed [typed #f])
  (code (define (shape-area s)
          (cond
            [(position? s) 0]
            [(#,circle? #,s) (* (sqr #,cr) 2 pi)]
            ...))))

(define (c f #:typed [typed #f])
  (code 
   #,(if typed          
         (code (define-type-alias Shape 
                 (U Position Circle Rectangle ...)))
         (code (code:comment "Shape = Position U Circle U Rectangle U ... ")
               ||))   
   #,(if typed (code (: shape-area (Shape -> Number))) (code (code:contract shape-area : Shape -> Number)))
   (code:comment "what is the area of shape s?")
   #,f
   ))

(define shape-sz (c (fn)))

(define shape-header (code (define (shape-area s) #,(cloud 100 30))))

;(start-at-recent-slide)
(define (occur2-1)
  (slide #:title "True Unions" #:layout 'center
         'alts
         (mk-itemize #:block values #:overlay (lambda (a . b) a)
                     (list "")
                     (list
                      (list
                       (smod (c shape-header) #:name "shape" #:sizeof shape-sz)
                       (tmod (c shape-header #:typed #t) #:name "shape"  #:sizeof shape-sz)

               )))
         ))

(define (occur2-2)
  (slide #:title "Occurrence Typing" #:layout 'center
         'alts
         (mk-itemize #:block values #:overlay (lambda (a . b) a)
                     (list "")
                     (list
                      (list
                       
                       (smod (c (fn)) #:name "shape")

                       (smod (c (fn #:cr (red-code (circle-radius s)))) #:name "shape")
                       (smod (c (fn #:cr (red-code (circle-radius s))
                                    #:circle? (red-code circle?)))
                             #:name "shape")
                       (vc-append (tmod (c (fn #:typed #t)
                                           #:typed #t)
                                        #:name "shape")
                                  (blank 10))
                       (vc-append (tmod (c (fn #:typed #t
                                               #:cr (red-code (circle-radius s)))
                                           #:typed #t)
                                        #:name "shape")
                                  (blank 10))
                       
                       (vc-append (tmod (c (fn #:typed #t
                                               #:circle? (red-code circle?))
                                           #:typed #t)
                                        #:name "shape")
                                  (blank 10)
                                  (code (Any -> Boolean : Circle)))
                       (vc-append (tmod (c (fn #:typed #t
                                               #:s (red-code s))
                                           #:typed #t)
                                        #:name "shape")
                                  (blank 10)
                                  (code Shape))
                       (vc-append (tmod (c (fn #:typed #t
                                               #:cr (code (circle-radius #,(red-code s))))
                                           #:typed #t)
                                        #:name "shape")
                                  (blank 10)
                                  (code Circle))
               )))
         ))

(define (occur3)
  (slide #:title "Occurrence Typing" #:layout 'center
         'alts
         (mk-itemize #:block values #:overlay (lambda (a . b) a)
                     (list "")
                     (list
                      (list
                       (smod (code (map rectangle-area
                                        |                  | |list-of-shapes |)))
                       (smod (code (map rectangle-area
                                        (filter rectangle? list-of-shapes))))
                       (vc-append
                        (tmod (code (map rectangle-area
                                         (filter rectangle? list-of-shapes))))
                        (blank 10)
                        (code (∀ (a b) ((a -> Boolean : b) 
                                        (Listof a)
                                        -> (Listof b)))))
                       (vc-append
                        (tmod (code (map rectangle-area
                                         (filter rectangle? list-of-shapes))))
                        (blank 10)
                        (code (∀ (a b) (#,(red-code (a -> Boolean : b)) 
                                        (Listof a)
                                        -> (Listof b)))))
                       (vc-append
                        (tmod (code (map rectangle-area
                                         (filter rectangle? list-of-shapes))))
                        (blank 10)
                        (code (#,(red-code (Any -> Boolean : Rectangle)) 
                               (Listof Shape)
                               -> (Listof Rectange)))))))
         ))



(occur2-1)
(occur2-2)
(start-at-recent-slide)
(occur1)
(occur3)

(slide #:title "Features"
       (para "Every language has distinctive features")
       (blank 20)
       (para "Typed Scheme must accomodate the distinctive features of Scheme"))

(slide #:title "Variable-arity Functions"
       (block
        (code (+ 1 2 3 4 5))
        (blank 10)
        (code (map add1 (list 1 2 3 4 5)))
        (blank 10)
        (code (map make-vector (list 1 2 3) (list 'a 'b 'c)))
        (blank 10)
        (code (apply + (list 1 2 3 4 5)))))

(slide #:title "Simple Variable-arity"
       (code (: + (Number * -> Number)))
       (blank 20)
       (tmod
       (code (+ 1 2 3 4 5))
       (code (apply + (list 1 2 3 4 5)))
       (code (apply + 1 2 (list 3 4 5)))))

(slide #:title "Variable-arity Polymorphism"
       'alts
       (list
        (list 
         (tmod (code (values 1))
               (code (values 3 "four")))
         (code ||))
        (list          
         (tmod (code (values 1))
               (code (values 3 "four")))
         (code (: values (∀ (α) (α ... -> (Values α ...))))))
        (list          
         (tmod (red-code (values 1))
               (code (values 3 "four")))
         (code (: values (∀ (α) (α ... -> (Values α ...)))))
         (code values @ Number =
               (Number -> (Values Number))))
        (list          
         (tmod (code (values 1))
               (red-code (values 3 "four")))
         (code (: values (∀ (α) (α ... -> (Values α ...)))))
         (code values @ Number String =
               (Number String -> Number String)))))

(slide #:title "Variable-arity Polymorphism"
       'alts
       (list
        (list
         (tmod
          (code (: f (∀ (α) (Boolean ... | | -> (α ... | | -> Void)))))))
        (list
         (tmod
          (code (: f (∀ (α) (Boolean ... α -> (α ... α -> Void)))))))
        (list
         (tmod
          (code (: f (∀ (α) (Boolean ... α -> (α ... α -> Void))))))
         (code f @ Number =
               (Boolean -> (Number -> Void))))
        (list
         (tmod
          (code (: f (∀ (α) (Boolean ... α -> (α ... α -> Void))))))
         (code f @ Number String =
               (Boolean Boolean -> (Number String -> Void))))))

(slide #:title "Variable-arity Polymorphism"              
       (tmod
        (code
         (map add1 (list 1 2 3 4 5))
         (map make-vector (list 1 2 3) (list 'a 'b 'c))))
       (blank 20)
       'next
       (code (: map 
                (∀ (α γ #,(red-code β ...))
                   (α #,(red-code β ...) -> γ) (Listof α) #,(red-code (Listof β) ...) 
                   -> (Listof γ)))))

(define fl
  (code
   (define 
     (cond [(or (null? as) (ormap null? bss))
            (error ’fold-left "wrong length lists")]))))

(slide #:title "Variable-arity Polymorphism"
       'alts
       (list
        (list
         (tmod #:sizeof fl
          (code
           (: fold-left
              (∀ (γ α β ...)
                 ((γ α β ... -> γ) γ 
                   (Listof α) (Listof β) ... -> γ)))))
          (code fold-left @ Boolean Number =
                ((Boolean Number -> Boolean) Boolean
                 (Listof Number) -> Boolean)))
        (list
         (tmod
          (code
           (: fold-left
              (∀ (γ α β ...)
                 ((γ α β ... -> γ) γ 
                  (Listof α) (Listof β) ... -> γ)))
           (define (fold-left f c as . bss)
             (cond [(and (null? as) (andmap null? bss)) c]
                   [(or (null? as) (ormap null? bss))
                    (error ’fold-left "wrong length lists")]
                   [else (apply 
                          fold-left
                          (apply f c (car as) (map car bss))
                          (cdr as)
                          (map cdr bss))]))
           )))
        (list
         (tmod
          (code
           (: fold-left
              (∀ (γ α β ...)
                 ((γ α β ... -> γ) γ 
                  (Listof α) (Listof β) ... -> γ)))
           (define (fold-left f c as . #,(red-code bss))
             (cond [(and (null? as) (andmap null? bss)) c]
                   [(or (null? as) (ormap null? bss))
                    (error ’fold-left "wrong length lists")]
                   [else (apply 
                          fold-left
                          (apply f c (car as) #,(red-code (map car bss)))
                          (cdr as)
                          (map cdr bss))]))
           )))
        
        ))

;(start-at-recent-slide)

                        

(slide #:title "Macros"
       'alts
       (list
        (list
         (tmod #:name "when" #:sizeof (code 
                                       (if (number? x) (display (+ x 1))
                                           
                                           (void)))
               (code 
                (when (number? x)
                  (display (+ x 1)))
                ||)))
        (list
         (tmod #:name "when" #:sizeof (code 
                                       (if (number? x) (display (+ x 1))
                                           (void)))
               (code 
                (if (number? x)
                    (display (+ x 1))
                    (void)))))
        (list
         (smod #:name "scheme/match" #:sizeof fty
               (code ...))
         (tmod #:name "matching" #:sizeof fty
               (code
                (require scheme/match)
                (define-struct: posn ([x : Number] [y : Number]))
                | |
                (match (make-posn 1 2)
                  [(struct posn (x y)) (+ x y)]))))))


(slide #:title "Keyword Arguments"
       (code (: open-output-file
                (String #:mode (U ’binary ’text) -> Output-Port)))
       (tmod
        (code
         (open-output-file "foo.txt")
         (open-output-file "foo.txt" #:mode ’binary)
         (open-output-file #:mode ’text "foo.txt"))
        ))

(tslide "Is It Easy?")

(slide #:title "Porting Experience"
       (item "Simple programs")
       (item "Substantial applications")
       (item "Bits of a large project"))

(slide #:title "Educational software"
       (para "HtDP")
       (subitem "no changes to code")
       (para "The Little Schemer")
       (subitem "small number of changes")
       'next
       (para "PLAI")
       (subitem "currently in use"))

;(start-at-recent-slide)
(slide #:title "Squadron Scramble"
       (para "Squadron Scramble")
       (subitem "Multi-player game written for Northeastern SE class")
       (subitem "Approx 2500 LoC")
       (subitem "One module (33 lines) left untyped"))

(slide #:title "DrScheme"
       (para "DrScheme Internals")
       (subitem "Porting work ongoing"))

(tslide "Related Work")

(slide #:title "Static Type Systems for Scheme"
       (para "Typed Lisp [Cartwright 76]")
       (para "Semantic Prototyping System [Wand 84]")
       (para "Infer [Haynes 95]")
       (para "Typedscm [Leavens et al 05]")
       'next
       (para "Strongtalk [Bracha & Griswold 93]")
       )

(slide #:title "Soft Typing et seq."
       (para "Fagan, Wright, Flanagan, Meunier [88-06]")
       (para "Agesen, Aiken, Heintze, Henglein, Marlow"))

(slide #:title "Variable-arity Polymorphism"
       (para "Infer [Dzeng and Haynes 95]")
       (para "Zip Calculus [Tullsen 01]")
       (para "Many other Haskell hacks"))

(slide #:title "Occurrence Typing"
       (para "Intensional Polymorphism [Crary et al 98]")
       (para "If-splitting [Shivers 88, Flanagan 98]"))

(slide #:title "Other Type Features"
       (para "Polymorphism, Recursive Types, ...")
       (para "Too many to list"))

(tslide "Conclusion")

(slide #:title "Research Plan"
       (table 3
              (map t
                   (list 
                    "Oct-Nov ""2008: ""Porting"
                    "Nov-Jan ""2009: ""Keywords and Varargs"
                    "Feb-Mar ""2009: ""Journal Paper"
                    "Apr-May ""2009: ""Experience Paper"
                    "Jun-Jul ""2009: ""Writing"
                    "Aug     ""2009: ""Defense"))
              ltl-superimpose
              ltl-superimpose
              (list 5 25)
              5))
(start-at-recent-slide)
(slide #:title "Publications"
       (para "DLS 2006: Modular Interoperation")
       (para "POPL 2008: Occurrence Typing")
       (para "In preparation: Variable-arity polymorphism")       
       (inset (para "(with Stevie Strickland)")
              (* 4 gap-size) 0 0 0))

(slide #:title "Conclusion"
       (para "When scripts grow up, they want to be programs.")
       (blank 20)
       (para "Typed Scheme can make this easier."))

(tslide "Thank You")

#|
#|
2. Desiderata for Program evolution
 - Module by module
 - Soundness
 - No new language
 - Plays nicely with untyped code
 - Works with existing systems

|#




#|

3. Typed Scheme
 - Supports modular language specification
 - Soundness through contracts
   Emphasize HO
 - Occurrence Typing
 - Integration w/ untyped code: require/provide
 - Integrated with DrScheme, macros, etc

|#

;;(start-at-recent-slide)


       


;;(start-at-recent-slide)

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
        (list
         (list
          (blank 10)
          (para "Polymorphism")
          (para "Union Types")
          (para "Recursive Types")
          (para "Structures")
          (colorize (para "Occurence Typing") "red")))
        ))

;;(start-at-recent-slide)

(tslide "Does it work?")


;;(start-at-recent-slide)

(slide #:title "Real Validation"
       'alts
       (list 
        (list (para "Implemented in PLT Scheme")
              (subitem "Support all PLT Tools")
              (subitem "Integrates with Macro and Module system")
              (subitem "All standard libraries available"))
        (list 'alts
              (list (list (para "Ported 5000 lines of existing code"))
                    (list (para "Ported 5000 lines of existing code \u2014 and documented the results")))
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

;;(start-at-recent-slide)

(tslide "Future Work")

(slide #:title "Future work"
       (para "Using Typed Scheme as a target for inference")
       (para "Applying the lessons of Typed Scheme to other scripting languages"))

(define (subpara str)
  (hc-append (blank (* 2 gap-size))
                  (para #:width (- (current-para-width) (* 2 gap-size))
                        str)))

(slide #:title "Inferring Types"
       (para "Soft typing attempted to typecheck untyped programs without programmer help.")
       #;(subpara "Fagan 92, Wright 94, Aiken 94, Flanagan 97, Meunier 06")
       (para "This project was hindered by complex type languages and unpredictable errors.")       
       (para "We believe that using Typed Scheme as a target will help."))

(slide #:title "Onward to Python?"
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
       (tt "http://www.ccs.neu.edu/~samth/typed-scheme")
       (blank 20)
       (parameterize ([current-font-size 24])
         (t "Thanks to Ryan Culpepper, Matthew Flatt, Ivan Gazeau, Stevie Strickland"))
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


|#