#lang slideshow

(require slideshow/step slideshow/code slideshow/face (only-in slideshow/slide title-size))

(require beamer/beamer
         "../dls06/utils.ss")

(current-keyword-list (list* "define:" "let:" "require/typed" "require/contract" ":" "->" 
                             "provide/contract" "..." "define-type-alias" "define-struct:"
                             (current-keyword-list)))

(define (start-at-recent-slide) #f)

(set-page-numbers-visible! #f)

(define (tslide t)
  (slide #:layout 'center
                (text t (current-main-font) 50)))

(define (mk-itemize texts pics 
                    #:para [para para]
                    #:block [block blockf]
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

(define (mod/lang lang
                  #:name [name #f] 
                  #:space [space #t]
                  #:sizeof [sz (blank 1)]
                  . body)
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

(slide #:layout 'center 
       (scale (titlet "To Type or Not To Type") 1.5)
       (blank)
       (t "Sam Tobin-Hochstadt"))

#;
(define outline (make-outline 'motivation "Why?" #f 
                              'background "Background" #f
                              'overview "What is it like?" #f
                              'how "How does it work" #f
                              'conclusion "Research" #f))

(tslide "Why")

(slide #:title "The Rise of Dynamic Languages"
       (vc-append
        (hc-append 
         #;#;
         (bitmap "../popl08/javascript-logo.png")
         (blank 20)
         (scale (bitmap "../popl08/perl_logo.jpg") .5)
         (blank 20)
         (scale (bitmap "../popl08/python-logo.png") .8))
        (blank 20)
        (hc-append (scale (bitmap "../popl08/ruby-400.png") .5)
                   (blank 20)
                   (scale (bitmap "../popl08/tcl.gif") .4)
                   (blank 20)
                   (bitmap "../popl08/php.gif")
                   (blank 20)
                   (bitmap "../popl08/lua.png"))))

(slide #:title "The Advantages of Dynamic Languages"
       (item "Rapid Development")
       (item "Easy Prototyping")
       (item "Interactivity")
       (item "Simplicity"))

(slide #:title "The Disadvantages of Dynamic Languages"
       (para "Always code as if the guy who ends up maintaining your code will be a violent psychopath who knows where you live.")
       (para "      - John F. Woods"))

(slide #:title "Promoting Maintainability"
       
       (para "Documentation helps with maintenence")
       'next
       (para "Unless it is")
       (subitem "Incomplete")
       (subitem "Incorrect")
       (subitem "Outdated")
       (subitem "Missing"))

(start-at-recent-slide)

(slide #:title "Types Can Help"
       (para "Types provide checked documentation")
       (blank)
       'alts
       (list
        (list
         (tt "f(x, y)")
         (blank)
         (para "What do we know about " (tt "f") "?"))
        (list
         (tt "int f(int x, int y)")
         (blank)
         (para "What more do we know about " (tt "f") "?"))))

(slide #:title "Type Refactoring"
       (para "Refactoring by adding types makes your program more:")
       (subitem "Maintainable")
       (subitem "Understandable")
       (subitem "Evolvable"))
        
(tslide "Where")

(slide #:title "Scheme"
       #:layout 'center
       'alts
       (list
        (list
         (para "The language of 211")
         (scale (bitmap "htdp-cover.gif") .8))
        (list
         (para "A language for PL research")
         (scale (bitmap "lambda.png") .5))
        (list
         (para "A language for real programming")
         (scale (bitmap "plt-green.jpg") 1))))

(slide #:title "PLT Scheme"
       (para "a programming language – a descendant of Scheme")
       (para "a family of programming languages – variants of Scheme")
       (para "a set of tools for using a family of programming languages"))

(slide #:title "PLT Scheme"
       (t "A mature programming language for writing real software"))

(slide #:title "Typed Scheme"
       (para "A type system for Scheme")
       (para "A typechecker for PLT Scheme code")
       (para "A language integrated with the rest of PLT Scheme"))

(start-at-recent-slide)

(tslide "What")

(define t-sq (code (: sq (Number -> Number))
                   (define (sq x)
                     (* x x))))

(define t-sq/prov (code #,t-sq
                        (provide sq)))

(define s-sq (lb-superimpose (ghost t-sq) (code 
                                           (code:contract Number -> Number)
                                           (define (sq x) 
                                             (* x x)))))
(define s-sq/prov (code #,s-sq
                        (provide sq)))

(define hello (lt-superimpose (ghost t-sq) (code (printf "Hello World"))))

(define sz (code (sq "eleven") (code:comment "=> contract violation")))


(slide #:title "Simple Type Refactoring" #:layout 'center
       'alts
       (list 
        (list (para "Hello World in Scheme")
              (smod #:name "print"  #:sizeof sz
               hello))
        (list (para "Hello World in Typed Scheme")
              (tmod #:name "print" #:sizeof sz
               hello))
        (list (para "Simple Arithmetic in Scheme")
              (smod s-sq #:name "arith" #:sizeof sz))
        (list (para "Simple Arithmetic in Typed Scheme")
              (tmod t-sq #:name "arith" #:sizeof sz))))

(define big-code
  (code (require shapes)
        ||
        (: size : (shape -> Number))
        (define (size s)
          (cond [(Rectange? s)
                 (* (Rectangle-w s)
                    (Rectangle-h s))]
                [(Circle? s)
                 (* (sqr (Circle-r s)) pi)]
                [(Square? s)
                 (sqr (Square-s s))]
                [else (error "unknown shape")]))))

(define big-code2
  (code (require shapes)
        ||
        (: size : (Shape -> Number))
        (define (size s)
          (cond [(Rectange? s)
                 (* (Rectangle-w s)
                    (Rectangle-h s))]
                [(Circle? s)
                 (* (sqr (Circle-r s)) pi)]
                [(Square? s)
                 (sqr (Square-s s))]))))

(start-at-recent-slide)

(slide #:title "A Bigger Example"
       #:layout 'center
       (blank)
       (tmod #:name "shapes" #:sizeof big-code
             (lt-superimpose
              (ghost big-code)
              (code (define-struct: shape ())
                    ||
                    (define-struct: (Rectangle shape) 
                      ([w : Number] [h : Number]))
                    (define-struct: (Circle shape)
                      ([r : Number]))
                    (define-struct: (Square shape)
                      ([s : Number]))))))


(slide #:title "A Bigger Example"
       #:layout 'center
       (blank)
       (tmod #:name "size" #:sizeof big-code
             big-code))

(slide #:title "A Bigger Example (2)"
       #:layout 'center
       (blank)
       (tmod #:name "shapes" #:sizeof big-code
             (lt-superimpose
              (ghost big-code)
              (code (define-struct: shape ())
                    ||
                    (define-struct: (Rectangle shape) 
                      ([w : Number] [h : Number]))
                    (define-struct: (Circle shape)
                      ([r : Number]))
                    (define-struct: (Square shape)
                      ([s : Number]))
                    (define-type-alias Shape
                      (U Rectangle Circle Square))))))


(slide #:title "A Bigger Example (2)"
       #:layout 'center
       (blank)
       (tmod #:name "size" #:sizeof big-code
             (lt-superimpose (ghost big-code) 
                             big-code2)))

(define-syntax-rule (red-code . e)
  (parameterize ([code-colorize-enabled #f])
    (colorize (code . e) "red")))

(define t-addx (code (define: (addx [x : Num]) : (Num -> Num)
                       (lambda: ([y : Num]) (+ x y)))))
(define s-addx (code (define (addx x)
                       (lambda (y) (+ x y)))))

(let* (
       [sz-long (code (require/typed addx (Num -> (Num -> Num)) addx))]
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
  
  (define-syntax-rule (red-block . _) 'nothing)
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
          #;#;
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

(tslide "How")

(slide #:title "Scheme Features"
       (para "Unions")
       (para "Recursive Types")
       (para "Modules")
       (para "Macros")
       (para "Structures")
       (para "Classes")
       (para "Parameters")
       (para "Variable-arity functions")
       (para "..."))

(slide #:title "Unions"
       'alts
       (list
        (list
         (para "Scheme programmers think in unions:")
         (blank)
         (code (code:comment "A Color is one of:")
               (code:comment "  - 'red")
               (code:comment "  - 'blue")
               (code:comment "  - 'green")))
        (list
         (para "Existing typed languages have unions:")
         (blank)
         (vl-append
          (tt "data Color =")
          (tt "     Red")
          (tt "   | Green")
          (tt "   | Blue"))
         'next
         (code (symbol? 'red) (code:comment "=> #t"))
         (ht-append (tt "Red :: Symbol") (t "  ?")))))

(slide #:title "Unions"
       (para "Solution:")
       (subitem "True, non-disjoint, unions"))

(start-at-recent-slide)

(slide #:title "Macros"
       'alts
       (list
        (list
         (code (or x y z)))
        (list
         (code (or x y z)
               (code:comment "=>")
               (let ([tmp1 x])
                 (if tmp1 tmp1
                     (let ([tmp2 x])
                       (if tmp2 tmp2
                           z))))))
        (list
         (code
          (match x
            [(list a b c) 1]
            [(vector x y z) 2]
            [else 'no-match])))))
(slide #:title "Macros"
       'alts
       (list
        
        (list
         (scale
          (code (let ((x1 x)) 
                  (let ((fail2 (lambda () (match:error x1))))
                    (let* ((f3 (lambda () 
                                 (syntax-parameterize 
                                  ((fail (make-rename-transformer (quote-syntax fail2)))) 
                                  (let ((else x1)) (begin (quote no-match)))))))
                      (cond 
                        ((pair? x1) 
                         (let ((car6 (car x1)) (cdr7 (cdr x1)))
                           (cond
                             ((pair? cdr7)
                              (let ((car10 (car cdr7)) (cdr11 (cdr cdr7)))
                                (cond ((pair? cdr11)
                                       (let ((car14 (car cdr11)) (cdr15 (cdr cdr11)))
                                         (cond ((null? cdr15)
                                                (syntax-parameterize
                                                 ((fail (make-rename-transformer (quote-syntax f3))))
                                                 (let ((c car14)) (let ((b car10)) (let ((a car6)) (begin 1))))))
                                               (else (f3)))))
                                      (else (f3)))))
                             (else (f3)))))
                        ((vector? x1) 
                         (case (vector-length x1)
                           ((3) (let ((temp20 (vector-ref x1 0))
                                      (temp21 (vector-ref x1 1))
                                      (temp22 (vector-ref x1 2)))
                                  (syntax-parameterize 
                                   ((fail (make-rename-transformer (quote-syntax f3))))
                                   (let ((z temp22)) 
                                     (let ((y temp21))
                                       (let ((x temp20)) 
                                         (begin 2)))))))))
                        (else (f3)))))))
          .5)
        )))

(slide #:title "Macros"
       (para "Solution:")
       (subitem "Typecheck after macro expansion"))

(tslide "When")

(slide #:title "A History of Typed Scheme"
       (para "An Idea")
       (para "An Investigation")
       (para "A Prototype")
       (para "A Paper")
       (para "A System"))

(slide (scale (titlet "Thank You") 1.5))

