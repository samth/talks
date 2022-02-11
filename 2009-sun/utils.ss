(module utils mzscheme
  (require slideshow/slide
           mzlib/etc)
  
  (require (lib "kw.ss") (lib "symbol.ss" "texpict")
           (lib "face.ss" "texpict") (lib "etc.ss")
           (lib "code.ss" "slideshow") (lib "step.ss" "slideshow"))
  (define (scale* p s)
    ;; scale/improve-new-text is problematic for items
    (let ([p (if (pict? p) p (t p))]) (scale/improve-new-text p s)))
  
  (define/kw (big   p #:optional [scale 3/2]) (scale* p scale))
  (define/kw (small p #:optional [scale 3/2]) (scale* p (/ scale)))
  
  
  (define (square n) (rectangle n n))
  (define sp (launder (ghost (square 110))))
  (define (filled-square n) (filled-rectangle n n))

  (define-values 
    (untyped typed)
    (let* ([size 60])
      (define ((mk f))
        (let ([p (f size)])
          (values (cc-superimpose p sp) p)))
      (values (mk circle) (mk filled-square))))
  
  
  (define-values (m1 fm1) (untyped))
  (define-values (m2 fm2) (untyped))
  (define-values (m3 fm3) (untyped))
  (define-values (m4 fm4) (untyped))
  (define-values (m5 fm5) (untyped))
  
  (define-values (tm1 tfm1)  (typed))
  (define-values (tm3 tfm3)  (typed))
  
  (define (mk-row l)
    (apply hc-append (append l (list sp))))
  
  
  (define row-top (mk-row (list sp sp m5 sp)))
  (define (row-mid f) (mk-row (list sp f sp m4)))
  (define (row-bot f) (mk-row (list f sp m2 sp)))
  
  (define (mk-no-arrow m1 m3) (vl-append 10 row-top (row-mid m3) (row-bot m1)))
  
  ;; connect : pict pict pict -> pict
  
  (define arrow-size 10)
  
  #;(define (connect/r main from to red?)
    (pin-arrow-line arrow-size main to lb-find from rt-find))
  
  
  (define (connect bot-find top-find)
    (opt-lambda (main from to [red? #f])
      (let ([color (if red? "red" #f)])
        (pin-arrow-line (if red? (* 2 arrow-size) arrow-size) main to bot-find from top-find #f color))))
  
  (define connect/l (connect rb-find lt-find))
  (define connect/r (connect lb-find rt-find))
  
  (define (add-arrows p fm1 fm3 red1 red2)
    (let* ([r-arrows (connect/r (connect/r (connect/r p fm1 fm3)
                                           fm3 fm5 red1)
                                fm2 fm4)]
           [l-arrows (connect/l (connect/l r-arrows fm2 fm3 red2)
                                fm4 fm5)])
      l-arrows))
    
  (define (mk-diag f g red1 red2)
    (let*-values ([(m1 fm1) (f)]
                  [(m3 fm3) (g)]
                  [(p) (mk-no-arrow m1 m3)])
      (big 
       (colorize
        (add-arrows p fm1 fm3 red1 red2)
        "blue"))))
  
  (provide (all-defined)))