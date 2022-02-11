#lang racket

(require slideshow "lib.ss" unstable/gui/slideshow slideshow/code "config.rkt"
         "ts-intro.rkt")

(provide combine1 combine2)

(def-repeated (s0 s1 s2 s3 s4 s5) (code s))
(def-repeated (s*0 s*1 s*2 s*3 s*4 s*5) (code s*))

(define test1 (code (string? s)))
(define test2 (code (string? s*)))
(define test3 (launder test1))
(define the-and (code (and #,test1 #,test2)))
(define append1 (code (string-append #,s1 #,s*1)))
(define append2 (code (string-append #,s2 (symbol->string #,s*2))))
(define bigT (code (U String Symbol)))
(define pic
  (code
   (: combine : 
      (U String Symbol) (U String Symbol) -> String)
   ||
   (define (combine #,s0 #,s*0)
     (cond [#,the-and
            #,append1]
           [#,test3
            #,append2]
           [(string? s*)
            (string-append (symbol->string #,s3) #,s*3)]
           [else
            (string-append (symbol->string #,s4)
                           (symbol->string #,s*4))]))))
(define tStr (code String))
(define tSym (code Symbol))

(define s@str (code String @ s))
(define s*@str (code String @ s*))
(define s*@sym (code Symbol @ s*))

(define imp1 (launder (code #,s@str ⊃ #,(overbar s*@str))))
(define imp2 (launder (code #,s*@str ⊃ #,(overbar s@str))))

(define (combine2)  
  (staged 
   [one two three four five]
   (slide #:title (title-t "Occurrence Typing, Formally")
          #:layout 'tall
    (pict-case stage-name
      [(one) pic]
      [(two) 
       (let ([concl (code ⊢ #,tStr @ s | | ⊢ #,tStr @ s*)])
         (connect
          (mini-slide (shaded pic)
                      (vc-append 
                        (ghost (code ⊢ #,s@str ∧ #,s*@str))
                        (ghost (hline (pict-width concl) 5))
                        concl))
          append1 concl))]
      [(three) 
       (let ([concl (code ⊢ #,tStr @ s | | ⊢ #,tStr @ s*)])
         (connect
          (connect
           (mini-slide (shaded pic)
                       (vc-append 
                        (code ⊢ #,s@str ∧ #,s*@str)
                        (hline (pict-width concl) 5)
                        concl))
           test1 s@str #f)
          test2 s*@str #f))]
      [(four)
       (connect (mini-slide (shaded pic)
                            (vc-append
                             (ghost (code ||))
                             (ghost (hline 1 5))
                             (code ⊢ #,s*@sym)))
                append2 s*@sym)]
      [(five)
       (connect
        (connect (mini-slide (shaded pic)
                             (vc-append
                              (code ⊢ #,s@str | | ⊢ #,imp1)
                              (hline (pict-width (code ⊢ #,s@str | | ⊢ #,imp1)) 5)
                              (code ⊢ #,s*@sym)))
                 the-and imp1 #f)
        test3 s@str #f)]))))

(define (combine1)  
  (staged
   [one one* two three four five]
   (define (build s s* ty ty*)
     (define ty1 (code #,(launder s) : #,(launder ty)))
     (define ty1* (launder (ghost (code #,(launder s) : #,(launder bigT)))))
     (define ty2* (launder (ghost (code #,(launder s*) : #,(launder bigT)))))
     (define ty2 (code #,(launder s*) : #,(launder ty*)))
     (define t (hc-append 50 (lt-superimpose ty1 ty1*) (lt-superimpose ty2 ty2*)))
     (connect
      (connect (mini-slide (shaded pic) (blank 50) t)
               s ty1)
      s* ty2))
   (slide #:title (title-t "Occurrence Typing, Informally")
          #:layout 'tall
          (pict-case stage-name
            [(one) pic]
            [(one*) (build s0 s*0 bigT bigT)]
            [(two) (build s1 s*1 tStr tStr)]
            [(three) (build s2 s*2 tStr tSym)]
            [(four) (build s3 s*3 tSym tStr)]
            [(five) (build s4 s*4 tSym tSym)]))))
