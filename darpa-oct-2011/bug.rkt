#lang racket
(let ()  
  (define (f #:x [x #f]) #f)  
  (define (g #:y [y #f]) 
    (begin (f) #f))
  #f)