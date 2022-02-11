#lang at-exp slideshow

(require slideshow/step slideshow/code slideshow/face 
         (only-in slideshow/slide title-size)
         (except-in "beamer.ss" title) "lib.ss" racket/gui  "thanks.ss" 
         "tslide.ss" "config.ss" "langs/main.rkt"
         "ts-intro.rkt" "stages.rkt" unstable/gui/slideshow
         "vulns.rkt" "gnosys.rkt" "cert.rkt" "why-dsl.rkt")

(title '("Domain Specific Languages for GnoSys")
       '()
       '(("Sam Tobin-Hochstadt" "Northeastern University"))
       "CRASH November 2011 PI Meeting")
(set-page-numbers-visible! #f)
(do-start? #t)

(cert)

(gnosys-intro)

(why-dsls)

(tslide "DSLs in Racket")

(langs)

(define (example-langs)
  (define-exec-code (sld run-sld str)
    (slide 
     (t "Hello CRASH")))  
  (slide/staged 
   [#;racket slide #;web #;lazy scribble datalog typed tslideshow]
   (pict-case stage-name
     [(racket) (code 
                |#lang racket| (code:comment "An echo server")
                (define listener (tcp-listen 12345))
                (define (run)
                  (define-values (in out) (tcp-accept listener))
                  (thread (λ () (copy-port in out)
                            (close-output-port out)))
                  (run))
                (run))]
     [(slide)
      (mini-slide
       (code |#lang slideshow|
             #,sld))
      ]
     [(web) 
      (code |#lang web-server/insta|
            (code:comment "A simple web server")
            (define (start request)
              (response/xexpr
               '(html
                 (body "Hello DARPA")))))]
     [(lazy) 
      (code |#lang lazy|
            (code:comment "An infinite list:")
            (define fibs
              (list* 1 1 (map + fibs (cdr fibs))))
            
            (code:comment "Print the 1000th Fibonacci number:")
            (print (list-ref fibs 1000)))]
     [(scribble) (code |#lang scribble/base|
                       #,(as-comment "@; Generate a PDF or HTML document")
                       #,atsign title #,neg{#,(as-string "Bottles ---") #,atsign italic #,neg{#,(as-string "Abridged")}}
                       #,atsign(apply itemlist
                                      (for/list ([n (in-range 100 0 -1)])
                                        #,atsign item #,neg{#,atsign(format "~a" n) #,(as-string "bottles.")})))]
     [(datalog)
      (code |#lang datalog|
            #,(as-datalog "ancestor(A, B) :- parent(A, B).")
            #,(as-datalog "ancestor(A, B) :-")
            #,(as-datalog "  parent(A, C), D = C, ancestor(D, B).")
            #,(as-datalog "parent(john, douglas).")
            #,(as-datalog "parent(bob, john).")
            #,(as-datalog "ancestor(A, B)?"))]
     [(typed)
      (code |#lang typed/racket|
            (struct: person ([first : String]
                             [|last | : String]))
            (: greeting (person -> String))
            (define (greeting n)
              (format "~a ~a"
                      (person-first n) (person-last n)))
            (greeting (make-person "Bob" "Smith")))]
     [(tslideshow)
      (parameterize ([current-keyword-list '(":" "*" "->" "Listof" "define" "..." "U" "#lang typed/racket")])
      (code |#lang typed/racket|
            (: tslide : (U Pict String)
                        (Listof (U Pict String)) * -> Void)
            (define (tslide t . subs) ...)))])
   (pict-case stage-name #:combine cc-superimpose
     [(typed) (t "Racket with static types and full interoperation")]
     [(scribble) (t "A domain-specific language (and syntax) for documentation")]
     [(datalog) (t "Integrated logic programming")]
     [(racket) (t "A modern programming language")]
     [(web) (t "A language for writing web servers")]
     [(lazy) (t "Lazy evaluation")]
     [(slide) (clickback (colorize (t "Run me") "blue") (λ () 
                                                          (define ns (make-gui-namespace))
                                                          (parameterize ([current-namespace ns])
                                                            (namespace-require 'slideshow)
                                                            (namespace-require "config.rkt"))
                                                          (eval run-sld ns)))]
     [(tslideshow) (t "Integrating multiple languages")])))


(example-langs)

(tslide "Using DSLs to construct robust systems")

(slide (t "We needed a test platform for GnoSys ..."))

(slide/staged [intro spec]
              #:title "Racket on a Router"
              (two-columns (scale (bitmap "router.jpg") .4)
                           (pict-case stage-name
                             [(intro)
                              (vl-append
                               (t "Netgear WNDR3700v2")
                               (t "")
                               (t "Widely-used hardware")
                               (t "Standard interfaces")
                               (blank 20)
                               (para "Interesting to industrial partners"))]
                             [(spec)
                              (vl-append
                               (t "Netgear WNDR3700v2")
                               (t "")                               
                               (t "Racket v5.2")
                               (t "$80")
                               (t ""))])))

(slide #:title "Services"
       (ht-append 
        30
        (column 
         400          
         (vl-append
          (item "DNS")
          (subitem "Request/Reply")
          (subitem "Cache Management")
          (blank 20)
          (item "SSH")
          (subitem "Encryption")
          (subitem "Long-lived connections")
          (subitem "Shell Access")))
        (column 
         400
         (vl-append
          (item "DHCP")
          (subitem "Broadcast/Multicast")
          (subitem "Lease Storage")))))

(slide 
 (titlet "Why these services?"))


(slide/staged [one two three] #:title "Vulnerability Note VU#794580" #:layout 'center
       (parameterize ()
         (cc-superimpose
          (block
           (para "A remote, unauthenticated attacker can gain complete control of a system, including the ability to install programs,"
                 " view, change or delete data, or create new accounts by:")
           (blank 40)          
           (pict-case stage-name
             [(one) (blank)]
             [else
              (item "Convincing the target host to make a DNS request to ... an attacking server.")]))
           (pict-case stage-name
             [(three) (rotate (white-box (scale (colorize (t "Fixed with a safe language") "red") 2)) (/ pi 6))]
             [else (blank)]))))

(slide/staged 
 [one two three] #:title "Vulnerability Note VU#794580" #:layout 'center
 (cc-superimpose
  (block
   (para "ISC BIND 8.2.2-P6 vulnerable to DoS when processing SRV records, aka the \"srv bug\"")
   (blank 40)          
   (pict-case stage-name
     [(one) (blank)]
     [else
      (para "Any network client capable of sending SRV records to vulnerable"
            " name server systems can exercise this vulnerability.")]))
  (pict-case stage-name
    [(three) (rotate (white-box (scale (colorize (t "Fixed with a DSL") "red") 2)) (/ pi 6))]
    [else (blank)])))

(vulns)

(tslide "Demo")


(slide #:title "Demo"
       (item "It's running now -- connect and try it")
       (vl-append
               (subitem "SSID"((current-code-tt) "RacketRouter")","((current-code-tt) "ssh roar.example"))
               (blank 10))
       (item "Sandboxes + DSLs")
       (blank 30)
       (item "Language technology at the core"))


(slide #:title "Wrap-up"
       
       (item "Raising the level of discourse with DSLs")
       (ghost (vl-append
               (subitem "SSID"((current-code-tt) "RacketRouter")","((current-code-tt) "ssh roar.example"))
               (blank 10)))
       (item "Secure-by-construction network services")
       (blank 30)
       (item (t "Built in Racket using GnoSys language technology")))

#;
(dynamic-require "bitsyntax.rkt" #f)