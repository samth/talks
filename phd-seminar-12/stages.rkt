#lang slideshow

(require "config.rkt" "lib.rkt")

(define ((mk-stage . words) hi?)
  (define name-pic (apply vc-append -5 (map (λ (p) (if (string? p) (t p) p)) words)))
  (define box (parameterize ([current-outline-color (if hi? "red" "black")]) 
                (colorize (transparent-block name-pic) (if hi? "red" "black"))))
  box)

(define il (mk-stage "Intermediate" "Language"))
(define static (mk-stage "Semantic" "Analysis"))
(define codegen (mk-stage "Code" "Generation"))
(define parser (mk-stage "Lexing &" "Parsing"))
(define linking (mk-stage (cc-superimpose (t "Linking") (vc-append -5 (t " ") (t " ")))))

(define (connect-stages a b)
  (pin-arrow-line #:hide-arrowhead? #f #:line-width 4 8 (hc-append 40 a b) a rc-find b lc-find))

(define order (list parser static il codegen linking))

(define (stages [highlight (map (λ _ #f) order)])
  (match* (order highlight)
    [((list rst ... lst) (list rst* ... lst*))
     (foldr connect-stages (lst lst*) (map (λ (f a) (f a)) rst rst*))]))

(provide (all-defined-out))