(module talk (lib "slideshow.ss" "slideshow")
    
  (require (lib "kw.ss") (lib "symbol.ss" "texpict")
           (lib "face.ss" "texpict") (lib "etc.ss")
           (lib "code.ss" "slideshow") (lib "step.ss" "slideshow"))

  ;; setup
  
  (define (scale* p s)
    ;; scale/improve-new-text is problematic for items
    (let ([p (if (pict? p) p (t p))]) (scale/improve-new-text p s)))
  
  (define/kw (big   p #:optional [scale 3/2]) (scale* p scale))
  (define/kw (small p #:optional [scale 3/2]) (scale* p (/ scale)))
  
  (define-syntax smalltext
    (syntax-rules ()
      [(_ e) (parameterize ((current-font-size (* 3/4 
                                                  (current-font-size)))) e)]))
  (define-syntax tiny
    (syntax-rules ()
      [(_ e) (parameterize ((current-font-size (/ (current-font-size) 
                                                  2))) e)]))
  (current-keyword-list (list* "define:" "provide/contract" "/contract" ":" "<=" "->" "require/typed" "Listof" "let:"
                               (current-keyword-list)))

  (define (slide/c . x) (slide/center (colorize (apply page-para* x) "blue")))
  

  (define (header s) (slide/center (titlet s)))
  
  ;; the talk
  
  ;; title slide
  (slide/center
   (big (titlet "Types for Untyped Languages"))
   (t "Sam Tobin-Hochstadt")
   'next
   (t "and Matthias Felleisen"))
  
  (header "Interlanguage Migration")
  
  (slide/c "10 line Script")
  (slide/c "1000 line Tool")
  (slide/c "100,000 line Mission Critical Application")
  (slide/c (bitmap "cover.jpg"))
  (slide/c "Wanted: Smooth Upgrade Path")
      
  (header "Typed Scheme")
  
  (slide/c 
   (code (define: (factorial [n : Number]) : Number
           (if (zero? n)
               0
               (* n (- n 1))))))
  
  (slide/c "Integrated with PLT Scheme")
  (slide/c (code (require/typed read-xml
                                (Port -> (Opaque document?)) 
                                (lib "xml.ss" "xml"))))
  
  (slide/c "Supports Scheme Programming Idioms")
 (slide/c (code
            (let ([l (list 1 2 3)]
                  [x 4])
              (cond [(member x l) => car]
                    [else #f]))))
  (slide/c (code
            (let: ([l : (Listof Number) (list 1 2 3)]
                   [x : Number 4])
              (cond [(member x l) => car]
                    [else #f]))))
  
  (header "Implementation")
  
  (slide/c (size-in-pixels (bitmap "screenshot.png")))
  
  (slide/c "Macros")
  
  (slide/c (code (module m "typed-scheme.ss"
                   (+ 3 4))))
  
  (slide/c (code 
            (#%module-begin
             (#%app +
                    (#%datum . 3)
                    (#%datum . 4)))))
  
  (header "Theory")
  
  (slide/c "Type Soundness ... " (ghost (t "is hard")))
  (slide/c "Type Soundness ... " (t "is hard"))
 

  (slide/c "Isabelle/HOL")
  
  (slide/c (size-in-pixels (bitmap "shot-isa.png")))
  
  ;(require "../../typed-scheme/formalism/simple-redex-model.ss")
  
  ;(slide/c (clickback (t "PLT Redex") (lambda () (apply tr* terms))))
  
  (slide/c "PLT Redex")
  
  (slide/c (size-in-pixels (bitmap "shot-redex.png")))
  
  (header "The Future")
  
  (slide/c "Porting and Testing")
  
  (header "Thank You")

  (slide/c "Questions?")
  )