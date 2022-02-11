#lang slideshow

(require unstable/gui/slideshow slideshow/code "lib.rkt" "config.rkt")
(provide language)
(define (language)
  (staged
   [one two three]
   (define mb1 (code #%module-begin))
   (define mb2 (code #%module-begin))
   (define def
     (code
      (define-syntax (#,mb1 stx)
        ....)))
   (define tr-mod
     (transparent-block
      (mod/lang "racket         " #:name "typed/racket" #:name-frame (name-back "red")
                def)))
   
   (define main
     (transparent-block; #:size-of def
      (pict-case stage-name
        [(one)
         (mod/lang "typed/racket   " #:name "name" #:name-frame (name-back "blue")
                   (ltl-superimpose
                    (ghost def)
                    (code ....)))]
        [(two three)
          (code
           (module name typed/racket
             #,(ltl-superimpose
                (ghost def)
                (code (#,mb2
                       ....)))))]))) 
   (slide
    #:title "Languages as Libraries"
    (pict-case stage-name
      [(one two) (mini-slide (ghost tr-mod) main)]      
      [(three) (connect (mini-slide tr-mod main) mb1 mb2)])))
  
  (define (big-mod1 hl?)
    (define f (code f))
    (define hf (highlight f))
    
    (define c
      (code
       (define-syntax (#%module-begin stx)
         (syntax-parse stx
           [(_ forms ...)
            (let ()
              (for ([f (syntax->list #'(forms ...))])
                (typecheck #,f))
              ||
              #'(#%plain-module-begin forms ...))]))))
    (if hl? (pin-over c f lt-find (refocus hf f)) c))
  
  (define (big-mod2)
    (define unsyntax #f)
    (define c
      (code
       (define-syntax (#%module-begin stx)
         (syntax-parse stx
           [(_ forms ...)
            (let ([forms* (local-expand #'(forms ...))])
              (for ([f (rest (syntax->list forms*))])
                (typecheck f))
              ||
              #'(#%plain-module-begin #,forms*))]))))
    c)
  
  (define (big-mod3)
    (define unsyntax #f)
    (define c
      (code
       (define-syntax (#%module-begin stx)
         (syntax-parse stx
           [(_ forms ...)
            (let ([forms* (local-expand #'(forms ...))])
              (for ([f (rest (syntax->list forms*))])
                (typecheck f))
              (let ([forms** (optimize forms*)])
                #'(#%plain-module-begin #,forms**)))]))))
    c)
  
  (slide/staged 
   [mac mac* three four]
   #:title "Defining A Language"
   (pict-case stage-name
     [(mac) (big-mod1 #f)]
     [(mac*) (big-mod1 #t)]
     [(three) (big-mod2)]
     [(four) (big-mod3)]))
  
  (staged [one two three]
          (slide #:title "Typechecking"
                 (code 
                  (define (typecheck f)
                    (syntax-parse f
                      (code:comment "variables")
                      [v:identifier
                       (#,(if (= stage 2) (red-code lookup-type) (code lookup-type)) #'v)]
                      (code:comment "abstractions")
                      [(lambda (x) e)
                       (define t #,(if (= stage 3) (red-code (syntax-property #'x 'type-label)) (code (syntax-property #'x 'type-label))))
                       (#,(if (= stage 2) (red-code set-type!) (code set-type!)) #'x t)
                       (typecheck #'e)]
                      (code:comment "about 10 more cases")
                      ....)))))
  
  (slide/staged 
   [one two]
   #:title "Optimization"
   (tmod
    (pict-case 
        stage-name
      [(one) (code (: norm : Float Float -> Float)
                   (define (norm x y)
                     (flsqrt (+ (* x x) (* y y)))))]
      [(two) (code (: norm : Float Float -> Float)
                   (define (norm x y)
                     (unsafe-flsqrt 
                      (unsafe-fl+ (unsafe-fl* x x) (unsafe-fl* y y)))))])))
  
  )