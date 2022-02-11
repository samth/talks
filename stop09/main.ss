#lang slideshow

(require slideshow/code scheme/list)

(slide #:layout 'center (titlet "A Gradual Typing Poem")
       (t "Sam Tobin-Hochstadt & Robby Finder"))

(define-syntax-rule (red-code . e)
  (parameterize ([code-colorize-enabled #f])
    (colorize (code . e) "red")))

(current-keyword-list (list* "define:" "let:" "require/typed" "require/contract" ":" "->"  "U" "struct" "Listof" "Pair"
                             "provide/contract" "..." "define-type-alias" "define-struct:" "List" "Rec"
                             (current-keyword-list)))

(define (ttcode . args)
  (apply vl-append (map tt args)))

(define (ttcode-file fname)
  (apply ttcode (for/list ([i (in-lines (open-input-file fname))]) i)))



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

(define (mk-itemize texts pics #:para [para para]
                    #:overlay [overlay lt-superimpose])
  (define g-pics (map ghost (apply append (map (lambda (e) (if (list? e) e (list e))) pics))))
  (let loop ([seen-ts '()] [ts (map para texts)] [ps pics])
    (define (rest v)
      (append
        (map (lambda (p)
               (append (reverse seen-ts)
                       (list (car ts)) 
                       (map ghost (cdr ts))
                       (list (apply overlay p g-pics))))
             v)
        (loop (cons (car ts) seen-ts)
              (cdr ts)
              (cdr ps))))
    (cond
      [(null? ts) null]
      [(list? (car ps)) (rest (car ps))]
      [else (rest (list (car ps)))])))

;; OUTLINE
;; - problem statement
;;    - picture

(define out (make-outline
             'prob "Problem Statement" #f
             'thesis "The Typed Scheme Advantage" #f
             'intro "An Intro to Typed Scheme" #f
             'sol1 "The First Solution" #f
             'sol2 "The Second Solution" #f
             'moral "The Moral" #f))

(slide #:title "The Problem"
       (para "Write a function that accepts the specification of an infinite"
             "regular tree"
             "and turn it into a representation of the tree")
       'next (blank)
       (para "Write a function that accepts a tree and finds its period"))

(define g (scale (bitmap "graph2.png") .5))

(slide #:title "An Example"
       'alts
       (parameterize ([current-title-color "orange"])
         (list (list g)
               (list (cc-superimpose g (colorize (titlet "Implementation") "blue")))))
       'next (blank)
       'alts
       (parameterize ([current-title-color "orange"])         
         (list
          (list
           (code (a (b b c)
                    (c (d d d)
                       (e e c)))))
          (list
           (cc-superimpose
            (code (a (b b c)
                     (c (d d d)
                        (e e c))))
            (colorize (titlet "Spec") "blue"))))))

(slide #:title "How to Solve It"
       g
       #;'next (blank)
       (code (a (b b c)
                (c (d d d)
                   (e e c)))))
#;
(slide #:title "The Standard Solution"
       (para "Pointers")
       (para "Placeholders")
       (para "and Mutation"))

(slide #:title "The Problem with the Solution"
       'alts
       (list
        (list
         (para "The Standard Solution")
         (item "exposes mutability")
         (item "exposes placeholders")
         (item "pushes the burden onto the client (the period function)"))
        (list
         (ht-append (scale (ttcode-file "../../papers/gradual-poem/itree.hs") .27)
                    (scale (ttcode-file "../../papers/gradual-poem/itree.sml") .27)
                    (scale (ttcode-file "../../papers/gradual-poem/itree2.sml") .27)))))

(out 'thesis)

(slide #:title "Where We're Going"
       (para "Typed Scheme allows a simple implementation of the problem where")
       (subitem "the complexity of the implementation is hidden")
       (subitem "the client has all the advantages of the original code")
       'next
       (para "All because of gradual typing!"))

;; - thesis:
;;   TS allows a simple solution where 
;;   (a) complexity is hidden
;;   (b) some nice features are preserved

(out 'intro)

(define bigsz (ghost (lt-superimpose
                      (code
                       ""
                       ""
                       (define-struct: ImpTree 
                         ([name  : Symbol]
                          [left  : (U ImpTree Symbol)]
                          [right : (U ImpTree Symbol)])))
                      (code (require/typed "x.ss" [t ImpTree])))))
;(start-at-recent-slide)

(define-syntax TS
  (syntax-rules ()
    [(TS #:sizeof sz . stuff)
     (mod/lang "typed-scheme" #:sizeof sz
               (code . stuff))]
    [(TS . stuff)
     (mod/lang "typed-scheme"
               (code . stuff))]))

(slide #:title "Typed Scheme"
       (TS #:sizeof bigsz
           (: x Number)
           (define x 1)))

(slide #:title "Typed Structs"
       (TS #:sizeof bigsz
           (define-struct: ImpTree 
             ([name  : Symbol]
              [left  : (U ImpTree Symbol)]
              [right : (U ImpTree Symbol)]))))

(slide #:title "Occurrence Typing"
       (TS #:sizeof bigsz
           (if (ImpTree? t)
               (display (ImpTree-name t))
               (display "no name"))))

(slide #:title "Modules"
       (TS #:sizeof bigsz
        (: t ImpTree)
        (define t (make-ImpTree 'a 'x 'y))
        (provide t)))

(slide #:title "Typed/Untyped Integration"
       (vl-append
        (mod/lang "scheme" #:sizeof bigsz
                  (code (provide t))
                  (code (define t (make-ImpTree #,(cloud 50 30)))))
        (colorize (t "      contract boundary") "red")
        (blank 50)
        (TS #:sizeof bigsz
            (require/typed "x.ss" [t ImpTree])
            (ImpTree-left t))))
       
;; - intro to TS
;;   - purpose
;;   - general feel - PLT Scheme module language
;;   - what a TS program looks like
;;   - typed/untyped integration

(out 'sol1)

(define it (code
            (define-struct: ImpTree 
              ([name  : Symbol] 
               [left  : (U ImpTree Symbol)]
               [right : (U ImpTree Symbol)])
              #:mutable)))

(define it* (code
             (define-struct: ImpTree 
               ([name  : Symbol]
                [left  : ImpTree]
                [right : ImpTree]))))

(define st
  (code (define-type-alias SpecTree 
          (Rec ST (U Symbol (List Symbol ST ST))))))

(define (seq s1 . s)
  (for/list ([i (in-range (add1 (length s)))])
    (let-values ([(f r) (split-at s i)])
      (list (apply vl-append (add-between (append (list s1) f (map ghost r)) (blank 3)))))))

(slide #:title "Specification"  
       'alts
       (seq st it)
       #;
       (list (list
              (vl-append
               st
               (blank)
               (ghost it)))
             (list
              (vl-append
               st
               (blank)
               it))))

(define per-impl
  (code 
   (: period (ImpTree -> (U Number #f)))
   (define (period it)
     (: bfs #,(cloud 100 30))
     (define (bfs s v) #,(cloud 100 30))
     (let ([l (ImpTree-left it)]
           [r (ImpTree-right it)])
       (if (and (ImpTree? l) (ImpTree? r))
           (bfs (list (cons l 1) (cons r 1))
                '())
           (error 'fail))))))

(slide #:title "Client Code"
       'alts
       (list
        (list
         (lt-superimpose
          (ghost per-impl)
          (code
           (: period (ImpTree -> (U Number #f)))
           (define (period it) #,(cloud 100 30)))))
        (list
         per-impl)
        (list
         (code 
          (: period (ImpTree -> (U Number #f)))
          (define (period it) 
            (: bfs #,(cloud 100 30))
            (define (bfs s v) #,(cloud 100 30))
            (let ([l (ImpTree-left it)]
                  [r (ImpTree-right it)])
              (#,(red-code if) #,(red-code (and (ImpTree? l) (ImpTree? r)))
                  (bfs (list (cons l 1) (cons r 1))
                       '())
                  #,(red-code (error 'fail)))))))))

(slide #:title "Client Code"
       'alts
       (list
        (list (code (: bfs ((Listof (Pair ImpTree Number)) 
			    (Listof Symbol) 
                            -> (U Number #f)))
                    (define (bfs stack visited)
                      (match stack
                        ['() #f]
                        [(cons (cons (struct ImpTree (str2 tl tr)) i) 
			       rest)
                         (cond
                           [(eq? str2 (ImpTree-name it)) i]
                           [(memq str2 visited) (bfs rest visited)]
                           [#,(red-code (and (ImpTree? tl) (ImpTree? tr)))
                            (bfs (append rest (list (cons tl (add1 i))
						    (cons tr (add1 i))))
                                 (cons str2 visited))]
                           #,(red-code [else (error 'fail)]))]))))))

(slide #:title "Client Code"
       (para "Exactly the problem we thought we'd have"))




;; - solution 1 in TS
;;   - regular one w/ error checking

(out 'sol2)

(slide #:title "Better Specification"  
       'alts
       (list 
             (list
              (vl-append
               st
               (blank)
               it*))))

(slide #:title "Gradual Typing to the Rescue"
       (code
        (require/typed "itree.ss"
          [struct ImpTree ([name  : Symbol]
                           [left  : ImpTree]
                           [right : ImpTree])]
          [link (SpecTree -> ImpTree)])))

(define per-impl2
  (code 
   (: period (ImpTree -> (U Number #f)))
   (define (period it) 
     (let ([l (ImpTree-left it)]
           [r (ImpTree-right it)])
       (bfs (list (cons l 1) (cons r 1))
            '())))))

(slide #:title "Client Code"
       'alts
       (list
        (list
         (lt-superimpose
          (ghost per-impl2)
          (code
           (: period (ImpTree -> (U Number #f)))
           (define (period it) #,(cloud 100 30)))))
        
        (list
         per-impl2)))

(slide #:title "Client Code"
       'alts
       (list
        (list (code (: bfs ((Listof (Pair ImpTree Number))
			    (Listof Symbol) 
                            -> (U Number #f)))
                    (define (bfs stack visited)
                      (match stack
                        ['() #f]
                        [(cons (cons (struct ImpTree (str2 tl tr)) i)
			       rest)
                         (cond
                           [(eq? str2 (ImpTree-name it)) i]
                           [(memq str2 visited) (bfs rest visited)]
                           [else
                            (bfs (append rest 
                                         (list (cons tl (add1 i))
                                               (cons tr (add1 i))))
                                 (cons str2 visited))])]))))))

(slide #:title "What Happened?"
       (para "Typed Scheme automatically synthesized contracts")
       (para "Mutation is hidden"))

;; - clever idea
;;   - we can use require/typed to hide details
;; - new implementation
;;   - has the nice properties

(out 'moral)

(slide #:title "Moral"
       (para "Gradual Typing adds expressiveness to" (bit "typed") "languages"))

;; - what's in the paper
;; - other cool things about TS (since this is a workshop)

(slide #:layout 'center (titlet "Thank You"))
