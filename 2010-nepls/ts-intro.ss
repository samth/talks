#lang slideshow

(require "lib.ss" slideshow/code (planet cce/scheme:6/slideshow) "tslide.ss" "config.ss"
         scheme/gui/base)
(provide (all-defined-out))
#|
(define (ack-def #:typed [typed #t])
    (code
     #,(if typed 
           (code (: ack (Integer Integer -> Integer))) 
           (code (code:comment "ack : Integer Integer -> Integer")))
     (define (ack m n)
       (cond [(<= m 0) (+ n 1)]
             [(<= n 0) (ack (- m 1) 1)]
             [else (ack (- m 1) (ack m (- n 1)))]))))

;; server typed
(define (server-typed #:title [title "Modules"] #:msg [msg 'nothing] #:arg [arg (code 2 3)])
  (slide #:title title #:layout 'center
         (tmod #:name "ack"
               (ack-def))
         (blank 10)
         (smod #:name "compute" #:sizeof (ack-def)
               (code (require ack)
                     ||
                     (ack #,arg)))
         msg))

(define (ts-intro)    
  (tslide "Typed Scheme in 3 Slides")
  (start)
  (staged 
   (untyped typed)
   (slide #:title "Hello World"
          (pict-case 
           stage-name
           [(untyped) (smod #:name "hello" #:width (pict-width (ack-def))
                            (code (printf "Hello World\n")))]
           [(typed) (tmod #:name "hello" #:width (pict-width (ack-def))
                          (code (printf "Hello World\n")))])))
  
  
  
  (staged 
   (untyped typed)
   (slide #:title "Functions" #:layout 'center
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
                             (blank 10)
                             (smod #:name "compute" #:sizeof (ack-def)
                                   (code (require ack)
                                         ||                                        
                                         (ack 2 3))))])))
  ;; neither typed
  (slide #:title "Modules" #:layout 'center
         (smod #:name "ack"
               (ack-def #:typed #f))
         (blank 10)
         (smod #:name "compute" #:sizeof (ack-def)
               (code (require ack)
                     ||
                     (ack 2 3))))
  
  (server-typed)
  
  ;; client typed
  (slide #:title "Modules" #:layout 'center
         (smod #:name "ack"
               (ack-def #:typed #f))
         (blank 10)
         (tmod #:name "compute" #:sizeof (ack-def)
               (code (require [ack 
                               (Integer Integer -> Integer)])
                     (ack 2 3))))
  ;; both typed
  (slide #:title "Modules" #:layout 'center
         (tmod #:name "ack"
               (ack-def))
         (blank 10)
         (tmod #:name "compute" #:sizeof (ack-def)
               (code (require ack)
                     ||
                     (ack 2 3)))))
|#