#lang slideshow

(require "lib.ss" slideshow/code unstable/gui/slideshow "tslide.ss" "config.ss")
(provide (all-defined-out))

(define-values (ack-define ack-cond)
  (values (code define) (code cond)))

(define (ack-def #:typed [typed #t])
    (code
     #,(pict-if
        typed 
        (code (: ack : Integer Integer -> Integer))
        (code (code:comment " ack : Integer Integer -> Integer")))
     (#,ack-define (ack m n)
       (#,ack-cond [(<= m 0) (+ n 1)]
             [(<= n 0) (ack (- m 1) 1)]
             [else (ack (- m 1) (ack m (- n 1)))]))))

;; server typed
(define (server-typed #:title [title "Modules"] #:msg [msg 'nothing] #:arg [arg (code 2 3)])
  (slide #:title title #:layout 'center
         (tmod #:name "ack"
               (ack-def))
         #;(blank 10)
         (smod #:name "compute" #:sizeof (ack-def)
               (code (require ack)
                     ||
                     (ack #,arg)))
         msg))

(define (ts-intro #:skip [skip? #f] #:title? [title? #t])    
  (unless (and skip? (memq 1 skip?))
    (tslide "Typed Racket in 3 Slides"))
  (unless (and skip? (memq 2 skip?))
  (staged 
   (untyped typed)
   (slide #:title (and title? "Hello World")
          (pict-case 
           stage-name
           [(untyped) (smod #:name "hello" #:width (pict-width (ack-def))
                            (code (printf "Hello World\n")))]
           [(typed) (tmod #:name "hello" #:width (pict-width (ack-def))
                          (code (printf "Hello World\n")))]))))
  
  
  
  (unless (and skip? (memq 3 skip?))
  (staged 
   (untyped typed)
   (slide #:title (and title? "Functions") #:layout 'center
          (pict-case 
           stage-name #:combine ltl-superimpose
           [(untyped) (smod #:name "ack"
                            (code
                             #,(ack-def #:typed #f)
                             ||
                             (ack 2 3)))]
           [(typed) (tmod #:name "ack"
                          (code
                           #,(ack-def)              
                           ||
                           (ack 2 3)))]
           [else (mini-slide (smod #:name "ack"
                                   (ack-def #:typed #f))
                             #;(blank 10)
                             (smod #:name "compute" #:sizeof (ack-def)
                                   (code (require ack)
                                         ||                                        
                                         (ack 2 3))))]))))
  (unless (and skip? (memq 4 skip?))

  ;; neither typed
  (slide #:title (if title? "Modules" #f) #:layout 'center
         (smod #:name "ack"
               (ack-def #:typed #f))
         #;(blank 10)
         (smod #:name "compute" #:sizeof (ack-def)
               (code (require ack)
                     ||
                     (ack 2 3))))
  
  (server-typed #:title (and title? "Modules"))
  
  ;; client typed
  (slide #:title (and title? "Modules") #:layout 'center
         (smod #:name "ack"
               (ack-def #:typed #f))
         #;(blank 10)
         (tmod #:name "compute" #:sizeof (ack-def)
               (code (require [ack 
                               (Integer Integer -> Integer)])
                     (ack 2 3))))
  ;; both typed
  (slide #:title (and title? "Modules") #:layout 'center
         (tmod #:name "ack"
               (ack-def))
         #;(blank 10)
         (tmod #:name "compute" #:sizeof (ack-def)
               (code (require ack)
                     ||
                     (ack 2 3))))))
