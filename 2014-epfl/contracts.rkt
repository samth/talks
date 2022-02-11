#lang slideshow

(require slideshow/code mzlib/etc "lib.ss" "tslide.ss" unstable/gui/slideshow
         "config.ss" "ts-intro.rkt")

(provide (all-defined-out))

(define error-message (error-t "client broke the contract (-> Integer Integer Integer) on ack"))

(define server
  (code (define (add5 x) (+ x 5))
        (provide/contract 
         [add5 (number? |.| -> |.| number?)])))
(define server2
  (code (define (add5 x) "x plus 5")
        (provide/contract 
         [add5 (number? |.| -> |.| number?)])))
(define (client2 #:comment [comment #f])
  (code (require server)
        #,(code (add5 "seven"))))
(define (client3-fun #:comment [comment #t])
  (code (require server)
        #,(code (add5 "seven")) #,(if comment (code (code:comment "=> client broke the contract   ")) (code))))
(define client3 (client3-fun))


(define ho-server1
  (code (define ((add-blaster x) y)
          (+ x y))
        (provide/contract
         [add-blaster 
          (number? |.| -> |.| (number? |.| -> |.| number?))])))

(define ho-server2
  (code (define ((add-blaster x) y)
          "Not a number")
        (provide/contract
         [add-blaster 
          (number? |.| -> |.| (number? |.| -> |.| number?))])))

(define ho-client1
  (code (require server)
        ((add-blaster 1) "two") (code:comment "=> client broke the contract")))

(define ho-client2
  (code (require server)
        ((add-blaster 1) 2) (code:comment "=> server broke the contract")))

(define (contracts/blame)
  (begin-with-definitions
    (tslide "Contracts and Blame")
    
    (slide #:title "Contracts"
           (para "A contract is dynamically checked property")
           ;'next
           'alts
           
           (list
            (list
             (smod #:name "server"
                   server)
             (smod #:name "client"
                   (code (require server)
                         #,(code (add5 7)))))
            (list
             (smod #:name "server"
                   server)
             (smod #:name "client"
                   (client2)))
            (list
             (smod #:name "server"
                   server2)
             (smod #:name "client"
                   (code (require server)
                         #,(code (add5 7)) (code:comment "=> contract error"))))))
    
    (slide #:title "Blame"
           (para "A contract is an agreement between two parties")
           ;'next
           'alts
           
           (list
            (list
             (smod #:name "server"
                   server)
             (smod #:name "client"
                   client3)
             'next
             (t "[Findler & Felleisen 02]"))
            (list
             (smod #:name "server"
                   server2)
             (smod #:name "client"
                   (code (require server)
                         #,(code (add5 7)) (code:comment "=> server broke the contract"))))))
    
    
    (slide #:title "Contracts"
           (para "A contract is an agreement between two parties")
           'alts
           (list
            (list
             (smod #:name "server" #:sizeof ho-client1
                   ho-server1)
             (smod #:name "client" #:sizeof ho-client1
                   ho-client1))        
            (list
             (smod #:name "server" #:sizeof ho-client1
                   ho-server2)
             (smod #:name "client" #:sizeof ho-client1
                   ho-client2))))
    
    (slide #:title "Contracts"
           (para "A contract is")
           (subpara "a dynamically enforced")
           (subpara "agreement between two parties")
           (subpara "with blame assignment."))
    ))

#;
(server-typed #:title "Sound Interoperation" 
              #:arg (code 'bad)
              #:msg error-message)


(define (multi-sound)
  (parameterize ([current-sizeof
                  (code (: addx (Number -> (Number -> Number)))
                        ||
                        (add5 7))])
      
      (define server1
        (tmod #:name "server"
              (code
               (: add5 (Number -> Number))
               (define (add5 x) (+ x 5)))))
      (define plus (code (+ x 5)))
      (define hl (cellophane (colorize (filled-rectangle (pict-width plus) (pict-height plus)) "pink") .5))
      (define server1-hl
        (tmod #:name "server"
              (code
               (: add5 (Number -> Number))
               (define (add5 x) #,(refocus (cc-superimpose hl plus) plus)))))
      (define server2
        (smod #:name "server"
              (code
               (define (add5 x) "x plus 5"))))
      (define server3
        (tmod #:name "server"
              (code
               (: addx (Number -> (Number -> Number)))
               (define (addx x) (lambda (y) (+ x y))))))
      
      (define client1
        (smod #:name "client"
              (code (require server)
                    (add5 7)
                    ||)))
      
      (slide/staged
       [good bad bad2 client-err1 #;client-err2 server-err1 #;server-err2 ho]
       #:title (title-t "Typed & Untyped") #:layout 'center
       (pict-case 
        stage-name
        [(good) (para "")]
        [(bad bad2) (para "Untyped code can make mistakes")]
        [else (para "Catch errors dynamically at the boundary")])
       (case stage-name
         [(good bad client-err1 client-err2) server1]
         [(bad2) server1-hl]
         [(server-err1 server-err2) server2]
         [(ho) server3])
       (case stage-name
         [(good) client1]
         [(bad bad2) (smod #:name "client"
                      (client2 #:comment (code ||)))]
         [(client-err1) 
          (smod #:name "client"
                (client2 #:comment (code ||)))]
         [(server-err1)
          (tmod #:name "client"
                (code (require server 
                               [add5 (Number -> Number)])
                      (add5 7) ))]
         [(ho)
          (smod #:name "client"
                (code (require server)
                      ((addx 7) 'bad) ))])
       (pict-case
        stage-name #:combine cc-superimpose
        [(good bad) (blank 1)]
        [(client-err1) (error-t "client broke the contract on add5")]
        [(server-err1) (error-t "server interface broke the contract on add5")]
        [(ho) (error-t "client broke the contract on add5")]
        [(bad2) (error-t "+: expects type <number> as 1st argument")]
        [(client-err2 server-err2)
         (t "The typed module is not at fault")]))
    
))
