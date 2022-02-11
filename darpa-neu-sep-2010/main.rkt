#lang slideshow

(require "config.rkt" "lib.rkt" slideshow/code "tslide.rkt"
         unstable/gui/slideshow)

#;
(title '("Racket")
       '("")
       '(("" ""))
       "Northeastern, September 16, 2010")

(tslide "From Racket to GnoSys")

(slide #:title "Racket is already"
       (item "expressive")
       (item "extensible")
       (item "performant")
       (item "reliable")
       (item "cross-platform"))

(slide/staged 
 [plain typed typed* lazy lazy* web web* datalog datalog*]
 #:title "Languages"
 (mod/lang (case stage-name
             [(plain) "racket"]
             [(typed typed*) "typed/racket"]
             [(lazy lazy*) "lazy"]
             [(web web*) "web-server"]
             [(datalog datalog*) "datalog"])
           (pict-case stage-name
             [(plain lazy lazy* web web*)
              (code 
               code:blank
               (define (twice f x)
                 (f (f x))))]
             [(typed typed*)
              (code 
               (: twice : (All (A) (A -> A) A -> A))
               (define (twice f x)
                 (f (f x))))]             
             [(datalog datalog*)
              (let ([unquote #f])
              (code 
               |parent(john, douglas)|
               |ancestor(A, B) :-|
               |  parent(A, B)|           
               |ancestor(A, B) :-|               
               |  parent(A, C),|               
               |  ancestor(C, B)|))]))
 (pict-case 
  stage-name
  [(plain typed web lazy datalog) (blank 5)]
  [(datalog*) 
   (t "Phaeton: Restricted Languages for Improved Analysis")]
  [(lazy*)
   (t "Rocket: Optimized Extensible Semantics")]
  [(typed*)
   (t "Rocket: Integrating Static Semantics with Optimization")]
  [(web*)
   (t "Phaeton: Static Semantics for Code Transformation")]))

(slide/staged [plain plain* contract contract* type type* acl2]
 #:title (case stage-name
           [(plain plain*) "Racket"]
           [(contract contract*) "Contracts"]
           [(type type*) "Types"]
	   [(acl2) "Verification"])
 
 (pict-case stage-name
   [(plain plain*)
    (mod/lang "racket"
              (code
               (require net/url net/uri-codec)
               code:blank
               (code:comment " let-me-google-that-for-you : string  -> [listof bytes]")
               (define (let-me-google-that-for-you query)
                 (define base "http://www.google.com/search?q=")
                 (define url (string->url 
                              (string-append base (uri-encode query))))
                 (define rx #,(red-code #rx"(?<=<h3 class=\"r\">).*?(?=</h3>)"))
                 (regexp-match* rx (get-pure-port url)))))]
   [(contract contract*)
    (mod/lang "racket"
              (code
               (require net/url net/uri-codec)               
               (provide/contract 
                 [let-me-google-that-for-you  #,(red-code (string? -> [listof bytes?]))])
               (define (let-me-google-that-for-you query)
                 (define base "http://www.google.com/search?q=")
                 (define url (string->url 
                              (string-append base (uri-encode query))))
                 (define rx #,(red-code #rx"(?<=<h3 class=\"r\">).*?(?=</h3>)"))
                 (regexp-match* rx (get-pure-port url)))))]
   [(type type*)
    (mod/lang "typed"
              (code
               (require typed/net/url typed/net/uri-codec)
               code:blank               
               (: let-me-google-that-for-you : #,(red-code String  -> (Listof Bytes)))
               (define (let-me-google-that-for-you query)
                 (define base "http://www.google.com/search?q=")
                 (define url (string->url 
                              (string-append base (uri-encode query))))
                 (define rx #,(red-code #rx"(?<=<h3 class=\"r\">).*?(?=</h3>)"))
                 (regexp-match* rx (get-pure-port url)))))]
   [(acl2)
    (mod/lang "verified"
              (code
               (require net/url net/uri-codec)
               code:blank               
               (: let-me-google-that-for-you : #,(red-code String  -> (Listof Bytes)))
               (define (let-me-google-that-for-you query)
                 ...)
	       (defthm query-cleanup ...)))]
    )
 (pict-case stage-name
            [(plain*)
             (t "Performance optimization of Embedded Languages")]
            [(contract*)
             (t "Static contract validation")]
            [(type*)
             (t "Types and Little Languages")]
	    [(acl2)
	     (t "Verification of Program Properties")]))

(slide #:title "Hermes: DSLs for Language Specification"
       
       (code 
        (define-syntax (let stx)
          (syntax-parse stx
            [(let bs:distinct-bindings body:expr)
             #'((λ (bs.var ...) body) bs.rhs ...)])))
       'next
       (t "Automated Semantic Tools"))

(slide #:title "HALOS: High-level OS"
       (code
        (define (run-bounded thunk timeout)
          (define user-cust (make-custodian))
          (parameterize ([current-custodian user-cust])
            (thread thunk))
          (sleep timeout)
          (custodian-shutdown-all user-cust)))
       'next
       (t "Semantics-based resource control"))
