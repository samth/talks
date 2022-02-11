#lang typed/racket

(: ackerman : Integer Integer -> Integer)
(define (ackerman m n)
  (cond [(<= m 0) (+ n 1)]
        [(<= n 0) (ackerman (- m 1) 1)]
        [else (ackerman (- m 1) (ackerman m (- n 1)))]))

(ackerman 2 3)

