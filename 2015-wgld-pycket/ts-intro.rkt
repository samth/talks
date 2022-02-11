#lang slideshow

(require "lib.ss" slideshow/code unstable/gui/slideshow "tslide.ss" "config.ss"
         "helper.rkt")
(provide (all-defined-out))

(define (title-t s)
  (t/quat s size2))


(define-values (ack-define ack-cond)
  (values (code/red define) (code/red cond)))

(define (ack-def #:typed [typed #t] #:define [def ack-define] #:cond [cnd ack-cond] #:colon [col (code :)])
    (code
     #,(pict-if
        typed 
        (code (#,col ack : Integer Integer -> Integer))
        (code (code:comment " ack : Integer Integer -> Integer")))
     (#,def (ack m n)
       (#,cnd [(<= m 0) (+ n 1)]
             [(<= n 0) (ack (- m 1) 1)]
             [else (ack (- m 1) (ack m (- n 1)))]))))

;; server typed
(define (server-typed #:title [title "Modules"] #:msg [msg 'nothing] #:arg [arg (code 2 3)])
  (slide #:title (title-t title) #:layout 'center
         (tmod #:name "ack"
               (ack-def))
         #;(blank 10)
         (smod #:name "compute" #:sizeof (ack-def)
               (code (require ack)
                     ||
                     (ack #,arg)))
         msg))

(define (ts-intro)    
  ;(tslide "Typed Racket in 3 Slides")
  (staged 
   (untyped typed)
   (slide #:title (title-t "Hello World")
          (pict-case 
           stage-name
           [(untyped) (smod #:name "hello" #:width (pict-width (ack-def))
                            (code (printf "Hello World\n")))]
           [(typed) (tmod #:name "hello" #:width (pict-width (ack-def))
                          (code (printf "Hello World\n")))])))
  
  
  
  (staged 
   (untyped typed)
   (slide #:title (title-t "Functions") #:layout 'center
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
                                         (ack 2 3))))])))
  ;; neither typed
  (slide #:title (title-t "Modules") #:layout 'center
         (smod #:name "ack"
               (ack-def #:typed #f))
         #;(blank 10)
         (smod #:name "compute" #:sizeof (ack-def)
               (code (require ack)
                     ||
                     (ack 2 3))))
  
  (server-typed)
  
  ;; client typed
  (slide #:title (title-t "Modules") #:layout 'center
         (smod #:name "ack"
               (ack-def #:typed #f))
         #;(blank 10)
         (tmod #:name "compute" #:sizeof (ack-def)
               (code (require/typed
                      [ack (Integer Integer -> Integer)])
                     (ack 2 3))))
  ;; both typed
  (slide #:title (title-t "Modules") #:layout 'center
         (tmod #:name "ack"
               (ack-def))
         #;(blank 10)
         (tmod #:name "compute" #:sizeof (ack-def)
               (code (require ack)
                     ||
                     (ack 2 3)))))
