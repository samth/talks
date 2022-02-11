#lang racket
(require slideshow slideshow/code "lib.rkt" (except-in "beamer.rkt" title) "tslide.rkt" "config.rkt"
         unstable/gui/slideshow "ts-intro.rkt" "../icfp2010/peano.rkt" "../icfp2010/combine.rkt")

(title '("Contracts and Types in Racket") '() '(("" "")))

(define (boundary c)
  (define n (if (number? c) c (pict-width c)))
  (cc-superimpose (colorize (filled-rounded-rectangle  n 50) "black")
                  (colorize (t "Boundary") "red")))

(slide #:title "Goals"
       (para "Allow programmers to state and enforce invariants")
       (blank 40)
       (hc-append (blank 30) (t "Static, dynamic, and hybrid")))

(slide #:title "Contracts"
       #:layout 'center
       (para "Contracts allow components to enforce their interfaces")
       'next
       (item "Pioneered by Meyer in Eiffel")
       (item "Extended to higher-order languages by Findler & Felleisen")
       (item "Extensively used in Racket"))

(slide #:title "Contract Use in Racket"
       (para "Survey conducted by Michael Greenberg")
       (item "More than 4000 contracts in the Racket code base")
       (item "Includes both simple checks for integers and complex invariant specification"))

(slide #:title "Basic Contracts"
       (rmod #:name "nums"
        (code
         (define phi 1.61803399)
         ||
         (provide (contract-out phi number?))))
       (boundary (code (provide (contract-out phi number?))))
       (rmod #:name "use" #:sizeof (code (provide (contract-out phi number?)))
        (code
         (require nums)
         (add1 phi))))

(slide #:title "Basic Contracts"
       (rmod #:name "nums"
        (code
         (define phi "1.61803399")
         ||
         (provide (contract-out phi number?))))
       (boundary (code (provide (contract-out phi number?))))
       (cc-superimpose
        (colorize (it "nums broke the contract `number?' on `phi'") "red")
        (ghost
       (rmod #:name "use" #:sizeof (code (provide (contract-out phi number?)))
        (code
         (require nums)
         (add1 phi))))))

(slide #:title "Function Contracts"
       (rmod #:name "encrypt"
             (code
              (provide/contract
               [encrypter (string? prime? -> string?)])
              (define (encrypter str p)
                (rsa-encrypt str p))))
       (boundary (code (provide/contract
                        [encrypter (string? prime? -> string?)])))
       'next
       (rmod #:sizeof (code (provide/contract
                             [encrypter (string? prime? -> string?)]))
             #:name "client"
             (code 
              (require encrypt)
              (encrypter "Eat at Joe's" 42))))

(define big2
  (code (provide/contract
         [encrypter (string? (prime? -> key?) -> string?)])))

(slide #:title "Higher-order Contracts"
       #:layout 'center
       (rmod #:name "encrypt"
             (code
              #,big2
              (define (encrypter str keygen)
                (rsa-encrypt str (keygen 42)))))
       (boundary big2)
       'next
       (rmod #:sizeof big2
             #:name "client"
             (code 
              (require encrypt)
              (encrypter "Eat at Joe's" (λ (kp) ...))))
       'next
       (ct-superimpose
        (colorize (it "encrypt broke the contract ... on `encrypter'") 
                  "red")
        (ghost big2)))

(slide #:title "Dependent Contracts"
       (rmod #:name "root"
             (code
              (define (sqrt x) ...)
              ||
              (provide 
               (contract-out 
                [sqrt (([n positive?]) ->i [result positive?] 
                        #:post (~= (sqr result) n))])))))

(tslide "Contract Extensions")

(slide #:title "Contracts for Classes"
       (code
        (define prime-stack
          (class object%
            (define intlist null)
            (define/public (empty?) (null? intlist))
            (define/public (push e)
              (set! intlist (cons e intlist)))
            (define/public (pop)
              (begin0 (first intlist)
                      (set! intlist (rest intlist))))))
        ||
        (provide/contract
         [prime-stack
          (class/c
           [push (-> prime? void?)]
           [pop (-> #:pre (not (send this empty?)) prime?)])])))

(slide #:title "Contracts for ..."
       #:layout 'center
       (para "Support contracts for many Racket features")
       (item "Structures")
       (item "First-class modules")
       (item "Hash tables")
       (item "Promises")
       (item "Syntax objects")
       (item "Parametric Functions")
       (item "Lazily-checked data structures"))

(slide #:title "Contracts for mutable data"
       
       (rmod #:name "server" #:sizeof (code (define (break) (vector-set! v 5 "a string")))
             (code 
              (define v (make-vector 10000 0.0))
              ||
              (provide
               (contract-out [v (vector/c float?)]))))
       'next
       (rmod #:name "client"
             #:sizeof (code (define (break) (vector-set! v 5 "a string")))
             (code 
              (require server)
              (vector-set! v 1000 "not a number"))))

(slide #:title "Contracts for mutable data"
       
       (rmod #:name "server" #:sizeof (code (define (break) (vector-set! v 5 "a string")))
             (code 
              (define v (make-vector 10000 0.0))
              (define (break) (vector-set! v 5 "a string"))
              (provide
               (contract-out [v (vector/c float?)]))))
       (rmod #:name "client"
             #:sizeof (code (define (break) (vector-set! v 5 "a string")))
             (code 
              (require server)
              (vector-ref v 5))))

(tslide "Typed Racket")

(staged [one two]
        (slide #:title "Typed Racket Goals"
               (item "Type system adapted to Racket idioms")
               (show (subitem "Occurrence typing, union types, ...")
                     (= stage two))
               (item "Smooth & sound interoperation with untyped code")
               (show (subitem "Automatic contract generation")
                     (= stage two))))

(ts-intro)

(tslide "How Typed Racket Works")

(peano1) 
(combine1)


(define (ack-def2 #:typed [typed #t] #:define [def ack-define] #:cond [cnd ack-cond] #:colon [col (code :)])
  (code
   (define-type PInt (refine Integer positive?))
   #,(pict-if
      typed 
      (code (#,col ack : PInt PInt -> PInt))
      (code (code:comment " ack : Integer Integer -> Integer")))
   (#,def (ack m n)
     (#,cnd [(<= m 0) (+ n 1)]
            [(<= n 0) (ack (- m 1) 1)]
            [else (ack (- m 1) (ack m (- n 1)))]))))

(tslide "Future Plans")

(slide #:title "Future work"
       (item "Types for classes")
       (item "Integrating types and contracts")
       (item "Checking richer properties")
       (item "Integrating formal methods")
       (item "Automatically deriving types"))

;; server typed
(define (server-typed2 #:title [title "Modules"] #:msg [msg 'nothing] #:arg [arg (code 2 3)])
  (slide #:title title #:layout 'center
         (tmod #:name "ack"
               (ack-def2))
         #;(blank 10)
         (smod #:name "compute" #:sizeof (ack-def)
               (code (require ack)
                     ||
                     (ack #,arg)))
         msg))
(server-typed2 #:title "Integrating Types and Contracts")
      
