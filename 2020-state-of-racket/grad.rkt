#lang typed/racket/base

(: grad : Real Real (-> Real Real) -> Real)
(define (grad step x df)
  (cond [(< step 0.0001) x]
        [else
         (define new-x (+ x (* -0.01 (df x))))
         (grad (abs (- x new-x)) new-x df)]))
(provide grad)
(module* m racket/base
  (require (submod ".."))
  (time (for ([_ (in-range 10000)]) (grad 10000 6 (λ (x) (- (* 4 x x) (* 9 x)))))))
