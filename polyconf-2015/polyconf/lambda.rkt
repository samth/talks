#lang racket


(provide #%module-begin #%top-interaction) ;; from `racket`

(provide lambda #%app #%datum)

(define-syntax-rule (lambda (x) e) (λ (x) e))

(define-syntax-rule (#%app operator operand)
  (#%plain-app operator operand))


(module reader syntax/module-reader
  polyconf/lambda)