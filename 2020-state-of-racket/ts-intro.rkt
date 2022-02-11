#lang slideshow

(require "lib.ss" slideshow/code unstable/gui/slideshow "tslide.ss" "config.ss"
         "helper.rkt" ppict/2)
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


(define (grad-def #:typed [typed #t] #:define [def ack-define] #:cond [cnd ack-cond] #:colon [col (code :)])
    (code
     #,(pict-if
        typed 
        (code (#,col grad : Real Real (-> Real Real) -> Real))
        (code (code:comment " grad : Real Real (-> Real Real) -> Real")))
     (#,def (grad step x df)
            (#,cnd [(< step 0.0001) x]
                   [else
                    (define new-x (+ x (* -0.01 (df x))))
                    (grad (abs (- x new-x)) new-x df)]))))





;; server typed


(require "overview.rkt")
(provide tr-slide)
(define-syntax-rule (tr-slide s more ...)
  (bottom-bar-slide "Typed Racket"
                    #:go (coord 1 1 'rb)
                    (t/cant s size2 #:color "white")
                    #:go (coord .5 .5 'cc)
                    more ...))

(define grad-call
  (code (grad 10000 6 (λ (x) (- (* 4 x x) (* 9 x))))))
(define tgrad-call
  (code (grad 10000 6 (λ ([x : Real]) (- (* 4 x x) (* 9 x))))))
(define (server-typed #:title [title "Modules"] #:msg msg #:arg [arg (code 2 3)])
  (tr-slide
   title #:go (coord .5 0 'ct) 30
         (tmod #:name "grad" #:width tgrad-call
               (grad-def))
         
         (smod #:name "compute" #:width tgrad-call
               (code (require grad)
                     ||
                     (grad #,arg)))
         #:next #:go (coord .5 .5 'cc) msg
         ))
(define (ts-intro)    
  ;(tslide "Typed Racket in 3 Slides")

  (tr-slide ""
                    #:go (coord .5 .5 'cc)
                    #:alt ((smod #:name "hello" #:width tgrad-call
                                 (code (printf "Hello World\n"))))
                    (tmod #:name "hello" #:width tgrad-call
                          (code (printf "Hello World\n"))))

  (tr-slide "Functions"
                    #:go (coord .5 .5 'cc)                    
                    #:alt ((smod #:name "grad" #:width tgrad-call
                                  (code
                                   #,(grad-def #:typed #f)
                                   ||
                                   #,grad-call #;(grad 10000 6 (λ (x) (- (* 4 x x) (* 9 x)))))))
                    (tmod #:name "grad"
                          (code
                           #,(grad-def)              
                           ||
                           #,tgrad-call)))
  

  (tr-slide "Modules"
                    #:go (coord .5 0 'ct)
                    30
                    #:alt
                    ((tmod #:name "grad" #:width tgrad-call
                          (grad-def))
                     #;(blank 10)
                     (smod #:name "compute" #:width tgrad-call
                           (code (require grad)
                                 ||
                                 #,grad-call)))
                    #:alt
                    ((smod #:name "grad" #:width tgrad-call
                           (grad-def #:typed #f))
                     (tmod #:name "compute" #:width tgrad-call
                           (code (require/typed
                                     [grad (Real Real (-> Real Real) -> Real)])
                                 #,tgrad-call)))
                    ;; both typed
                    (tmod #:name "grad" #:width tgrad-call
                          (grad-def))
                    #;(blank 10)
                    (tmod #:name "compute" #:width tgrad-call
                          (code (require grad)
                                ||
                                #,tgrad-call))))


(module+ main (ts-intro))
