#lang racket

(provide #%module-begin #%top-interaction)

(define-syntax-rule (#%module-begin . _)
  (#%plain-module-begin (displayln "hello polyconf")))

(define-syntax-rule (#%top-interaction . _)
  (displayln "hello polyconf"))

(module reader syntax/module-reader
  polyconf)