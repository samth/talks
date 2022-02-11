#lang slideshow

(require "lib.ss" slideshow/code (planet cce/scheme:6/slideshow))
(provide ts-intro)

(define (ts-intro)
  (define (collatz-def #:typed [typed #t])
    (code
     #,(if typed 
           (code (: collatz (Number -> Number))) 
           (code (code:comment "collatz : Number -> Number")))
     (define (collatz n)
       (cond [(= n 1) 1]
	     [(even? n) (collatz (/ n 2))]
	     [else (collatz (add1 (* 3 n)))]))))
  
  (tslide "Typed Scheme in 3 Slides")
  ;(start)
  (staged 
   (untyped typed)
   (slide #:title "Hello World"
          (pict-case 
           stage-name
           [(untyped) (smod #:name "hello"
                            (code (printf "Hello World\n")))]
           [(typed) (tmod #:name "hello"
                          (code (printf "Hello World\n")))])))
  
  
  
  (staged 
   (untyped typed)
   (slide #:title "Functions"
          (pict-case 
           stage-name
           [(untyped) (smod #:name "collatz"
                            (code
                             #,(collatz-def #:typed #f)
                             ||
                             (collatz 17)))]
           [(typed) (tmod #:name "collatz"
                          (code
                           #,(collatz-def)              
                           ||
                           (collatz 17)))])))    
  ;; neither typed
  (slide #:title "Modules" #:layout 'center
         (smod #:name "collatz"
               (collatz-def #:typed #f))
         (blank 10)
         (smod #:name "compute" #:sizeof (collatz-def)
               (code (require collatz)
                     ||
                     (collatz 17))))
  ;; server typed
  (slide #:title "Modules" #:layout 'center
         (tmod #:name "collatz"
               (collatz-def))
         (blank 10)
         (smod #:name "compute" #:sizeof (collatz-def)
               (code (require collatz)
                     ||
                     (collatz 17))))
  ;; client typed
  (slide #:title "Modules" #:layout 'center
         (smod #:name "collatz"
               (collatz-def #:typed #f))
         (blank 10)
         (tmod #:name "compute" #:sizeof (collatz-def)
               (code (require [collatz 
                               (Number -> Number)])
                     (collatz 17))))
  ;; both typed
  (slide #:title "Modules" #:layout 'center
         (tmod #:name "collatz"
               (collatz-def))
         (blank 10)
         (tmod #:name "compute" #:sizeof (collatz-def)
               (code (require collatz)
                     ||
                     (collatz 17)))))
