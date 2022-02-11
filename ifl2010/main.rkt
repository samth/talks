#lang slideshow

(require slideshow/step slideshow/code slideshow/face (only-in slideshow/slide title-size)
         (except-in "beamer.ss" title) "lib.ss" racket/gui
         "tslide.ss" "config.ss" 
         racket/runtime-path mzlib/etc unstable/gui/slideshow)

(title '("Extensible Pattern Matching" "")
       '()
       '(("Sam Tobin-Hochstadt" "PLT @ Northeastern University"))
       "IFL, September 3, 2010")

(title '("Extensible Pattern Matching""in an Extensible Language")
       '()
       '(("Sam Tobin-Hochstadt" "PLT @ Northeastern University"))
       "IFL, September 3, 2010")
(set-page-numbers-visible! #f)
(do-start? #f)

(define-syntax stg
  (syntax-rules ()
    [(_ expr)
     (stg #:scale 1 expr)]
    [(_ #:scale val expr)
     (code 
      (: magnitude : Complex -> Real)
      (define (magnitude n)
        #,(scale (code expr) val)))]))

;; Pattern Matching in Racket - as in abstract

(slide/staged [one one* two three four five]
  (pict-case stage-name
    [(one)
     (stg 
      (cond 
        [(eq? (first n) 'cart)
         (sqrt (+ (sqr (second n))
                  (sqr (third n))))]
        [(eq? (first n) 'polar)
         (second n)]))]
    [(one*)
     (stg #:scale 0.5
      (if (not (pair? n))
          (error 'bad-input)
          (let ([t1 (first n)]
                [t1* (rest n)])
            (if (not (pair? t1*))
                (error 'bad-input)
                (let ([t2 (first t1*)]
                      [t2* (rest t1*)])
                  (if (not (pair? t3))
                      (error 'bad-input)
                      (let ([t3 (first t2*)]
                            [t3* (rest t2*)])
                        (if (not (null? t3))
                            (error 'bad-input))
                        (cond [(eq? t1 'cart)
                               (sqrt (+ (sqr t2) (sqr t3)))]
                              [(eq? t1 'polar)
                               t2]
                              [else (error 'bad-input)]))))))))]
    
    [(two)
     (stg 
      (match n
        [(list 'cart x y)
         (sqrt (+ (sqr x) (sqr y)))]
        [(list 'polar r theta)
         r]))]
    [(three)
     (stg
      (match n
        [(list 'cart xs ...)
         (sqrt (apply + (map sqr xs)))]
        [(list 'polar r theta ...)
         r]))]
    [(four)
     (stg
      (match n
        [(cart xs ...)
         (sqrt (apply + (map sqr xs)))]
        [(polar r theta ...)
         r]))]
    [(five)
     (stg
      (match n             
        [(polar r theta ...) r]))]
    [else (blank 0 0)]))

;; Implementing Pattern Matching

(tslide "Pattern Matching in Racket")

(slide/staged
 [a b c d1 d2]
 #:title 
 (list (code match) 
       (case stage-name
         [(a) " works for arbitrary data"]
         [(b) " provides expressive patterns"]
         [(c) " is an optimizer"]
         [(d1 d2) " supports recursive patterns"]))
 (pict-case 
  stage-name
  [(a)
   (code 
    (match e
      [(list a b) (+ a b)]
      [(? string? a) (string-length a)]
      [(? number? a) a]))]
  [(b)
   (code
    (match e
      [(app add1 n) n]))]
  [(c)
   (code
    (match e          
      [(list (? B?)) do-something-else]))]
  [(d1)
   (code (match (list 2 4 6 8 10)
           [(list (? even? y) ...) 
            (foldr + 0 y)]))]
  [(d2)
   (code (match '(3 2 1 3)
           [(list-no-order 1 2 3 ...) 'yes]
           [_ 'no]))]
  [else (blank)])
 (blank 50)
 (show (t "[Le Fessant & Maranget]") (eq? stage-name 'c)))

(tslide "Extensible Languages")

(slide/staged 
 [one two three]
 #:title "Simple Language Extension"
 (code 
  (define-syntax
    (let ([x e] ...) body)
    #,(red-code ((lambda (x ...) body) e ...))))
 (blank 50)
 (code (let ([x 1] [y 2]) (+ x y)))
 (pict-case stage-name
   [(two three) (code ((lambda (x y) (+ x y)) 1 2))]
   [else (blank)])
 (show
  (t "[Kohlbecker et al, 1980s]")
  (eq? 'three stage-name)))

(slide/staged 
 [one two three]
 #:title "Adding Computation"
 (code
  (define-syntax (numbers start end)
    #,(red-code (list #,(plain-code (in-range #,(code start) #,(code end)))))))
 (blank 50)
 (code (numbers 1 10)) 
 (pict-case stage-name #:combine cc-superimpose
   [(two three) (code (list 1 2 3 4 5 6 7 8 9 10))]
   [else (blank)])
 (show
  (t "[Dybvig et al, 1990s]")
  (eq? 'three stage-name)))

(slide/staged 
 [one two]
 #:title "Racket"
 (para "Modular Language Extension")
 (para "Compiler API")
 (para "Arbitrary Language Rewriting")
 (code ...)
 (show
  (t "[Flatt et al, 2000s]")
  (eq? 'two stage-name)))


(slide/staged 
 [one two]
 (code (define-syntax x 1)
       code:blank
       (define-syntax (get-x)
         (syntax-value x))
       code:blank
       code:blank
       (get-x)
       code:blank
       #,(show (hbl-append (code 1)) (eq? stage-name 'two) )))


;; Implementing Extensible Pattern Matching

(tslide "Extensible Pattern Matching")

(slide/staged 
 [mac mat]
 ;#:title (case stage-name [(mac) "An Extensible Language"] [(mat) "An Extensible Matcher"])
 #;(pict-case stage-name
   [(mac) (para "Macros are Rewrite Rules")]
   [(mat) (para "Match Expanders are Rewrite Rules")]
   [else (blank)])
 (vl-append
  (code
   (define-syntax (let ([x e] ...) b)
     #,(red-code ((lambda (x ...) b) e ...))))
  (blank 40)
  (show
   (code 
    (define-matcher (not-false p)
      #,(red-code (? (compose not false?) p))))
   (eq? stage-name 'mat)))
 )

(define c (code (app transformer pattern)))


(start)

(define (c2 stage)
  (define sv (code (syntax-value id)))
  (define trans (code (match-expander-fn #,sv)))  
  
  (define body
    (code (let ([transformer 
                 #,trans])
            (parse-pattern
             (transformer #,(red-code (id pats ...)))))))
  (define just-sv (pin-over (ghost body) (list trans sv ) lt-find sv))
  (define just-trans (pin-over (ghost body) trans lt-find trans))
  (define t-red (code (transformer #,(red-code (id pats ...)))))
  (define let-body (code (parse-pattern
                          #,t-red)))
  (define just-let (code (let ([transformer 
                                #,trans])
                           #,(ghost let-body))))
  (define just-let2 (code (let ([transformer 
                                 #,trans])
                            #,(pin-over (ghost let-body) t-red lt-find t-red))))
  (code
   [(id pats ...)
    #,(show (code (code:line #:when (bound-to-match-expander? id))) (> stage 1))
    #,(case stage
        [(0 1 2) (ghost body)]
        [(3) just-sv]
        [(4) just-trans]
        [(5) just-let]
        [(6) just-let2]
        [else body])]))

(define big-blank (blank (pict-width (c2 +inf.0)) 0))

(slide/staged [one two three four five six seven eight]
              #:title (if (= stage 1) (list "The core of " (code match)) "The extended core")
              (pict-if
               (not (eq? stage-name 'one))
               (code
                (define (parse-pattern pat)
                  (syntax-case pat
                    #,(lt-superimpose
                       (c2 (sub1 stage))
                       big-blank)
                    #,big-blank
                    [(cons pat1 pat2) ...]
                    [(? pred pat) ...]
                    ...)))
               (code
                (define (parse-pattern pat)
                  (syntax-case pat
                    #,big-blank
                    [(cons pat1 pat2) ...]
                    [(? pred pat) ...]
                    ...)))))

(slide/staged  
 [one two three four five]
 #:title "An Example"
 (pict-case stage
   [(1)
    (code 
     (define-matcher (not-false p) ...)
     code:blank
     (match (list 7 #f)
       [(list (not-false x) ... y) x]))]
   [(2)
    (code 
     #,(blue-code (define-syntax not-false 
                    (match-expander ...)))
     (match (list 7 #f)
       [(list (not-false x) ... y) x]))]
   [(3)
    (code 
     (define-syntax not-false 
       (match-expander ...))
     (match (list 7 #f)
       [(list (not-false z) ... y) z])
     code:blank
     #,(transparent-block (code (let ([transformer 
                                       (match-expander-fn (syntax-value not-false))])
                                  (parse-pattern (transformer #,(red-code (not-false z))))))))]
   [(4)
    (code 
     (define-syntax not-false 
       (match-expander ...))
     (match (list 7 #f)
       [(list (not-false z) ... y) z])
     code:blank
     #,(transparent-block (ct-superimpose
                           (blue-code (? (compose not false?) z))
                           (ghost (code (let ([transformer 
                                               (match-expander-fn (syntax-value not-false))])
                                          (transformer #'(not-false z))))))))]
   [(5)
    (code 
     (define-syntax not-false 
       (match-expander ...))
     (match (list 7 #f)
       [(list #,(blue-code (? (compose not false?) z)) ... y) z]))]
   [else (blank)]))
 

;; Other Applications

(tslide "Applications")

(slide #:title "Views [Wadler 87] as a library"
       (code (require (planet cobbe/views/views))
             (define-view Zero zero? ())
             (define-view Succ 
               exact-positive-integer? (sub1))
             (define (even? n)
               (match n
                 [(Zero) true]
                 [(Succ (Zero)) false]
                 [(Succ (Succ n)) (even? n)]))))

(slide #:title "Web Server Dispatching"
       (code (dispatch-rules
              [("") list-posts]
              [("posts" (string-arg)) review-post]
              [("archive" (integer-arg) (integer-arg))
               review-archive]
              [else list-posts])))



;; Related Work?

(parameterize ([current-keyword-list null]
               [current-line-sep 0])
  (slide 
   #:title "Other Extensible Systems"
   (para "View Patterns [Peyton-Jones et al]: " (code app) "patterns")
   (para "Views [Wadler]: " (code define-matcher) "and" (code app))
   (para "Active Patterns [Syme et al]: " "Multiple uses of" (code define-matcher) "," (code app) ", and" (code ?))))

(slide 
 (para "Pattern matching is great")
 (para "Extensible pattern matching is even better")
 (para "An expressive and extensible language can give us both"))

(tslide "Thanks!" '("Available at racket-lang.org"))