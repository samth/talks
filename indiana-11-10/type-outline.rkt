#lang slideshow
(require slideshow/code unstable/gui/slideshow "config.rkt" "lib.rkt" "peano.rkt")

(define forall (lift-above-baseline (scale (code ∀) .95) -3))
(define lam (let ([x (scale (code λ) .95)])
              (lift-above-baseline x -3)))

(provide type-outline)
(define (type-outline)
(slide/staged 
 [occur refine union varar #;local]
 #:title "Types for Existing Programs"
 (tmod #:name (symbol->string stage-name)
  (pict-case stage-name
   [(occur)
    pic]
   [(varar) 
    (code
     (: wrap (#,forall (B A ...)
               (A ... -> B) -> (A ... -> B)))
     (define (wrap f)
       (#,lam args
         (printf "args are: ~a\n" args)
         (apply f args))))]
   [(refine)
    (code
     (: check : String -> (Refinement sql-safe?))
     (define (check s)
       (if (sql-safe? s)
           s
           (error "unsafe string!"))))]
   [(local) 
    (code
     (define x 1)
     (define y 2)
     (map - (list x y)))]
   [(union)
    (code
     (define-type BT (U Number (Pair BT BT)))
     (: sizeof : BT -> Number)
     (define (sizeof b)
       (if (number? b)
           1
           (+ 1 (sizeof (car b)) (sizeof (cdr b))))))]))))