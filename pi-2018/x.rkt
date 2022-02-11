#lang racket/base
(require racket/contract)
(define (vec-sum v)
  (let loop ([i 0] [sum 0])
    (if (= i (vector-length v)) sum
        (loop (add1 i) (+ sum (vector-ref v i))))))
(define/contract (c/vec-sum v)
  (-> (vectorof number?) number?)
  (let loop ([i 0] [sum 0])
    (if (= i (vector-length v)) sum
        (loop (add1 i) (+ sum (vector-ref v i))))))

(module sub typed/racket/base
  (provide t:vec-sum)
  (: t:vec-sum : (Vectorof Float) -> Float)
  (define (t:vec-sum v)
    (let loop ([i 0] [sum 0.0])
      (if (= i (vector-length v)) sum
          (loop (add1 i) (+ sum (vector-ref v i))))))
  )
(define N 10000000)
(require 'sub)
(for ([i 5])
  (collect-garbage) (collect-garbage)
  (time (vec-sum (make-vector N 1.1))))
(newline)
(for ([i 5])
  (collect-garbage) (collect-garbage)
  (time (c/vec-sum (make-vector N 1.1))))
(newline)
(for ([i 5])
  (collect-garbage) (collect-garbage)
  (time (t:vec-sum (make-vector N 1.1))))
