#lang racket/base

(require slideshow "helper.rkt")

(define papers*
  (hash "ICFP 10" '(tr)
        "PLDI 11" '(tr lang compiler)
        "PLDI 16" '(tr verify)
        "ESOP 13" '(tr gradual)
        "ESOP 16" '(tr)
        "ESOP 15" '(gradual)
        "ESOP 14" '(other)
        "CACM 18" '(lang)
        "JFP 18" '(contract)
        "POPL 18" '(contract verify)
        "JFP 17" '(contract verify)
        "OOPSLA 17" '(contract gradual compiler)
        "ECOOP 17" '(compiler)
        "SNAPL 17" '(tr gradual)
        "Wadler 16" '(gradual)
        "PPOPP 16" '(tr other)
        "ECOOP 15" '(tr gradual)
        "ICFP 15a" '(compiler contract)
        "ICFP 15b" '(contract)
        "ICFP 14" '(contract verify)
        "PLDI 14" '(other)
        "ESOP 12" '(gradual)
        "OOPSLA 12a" '(contract verify)
        "OOPSLA 12b" '(compiler)
        "OOPSLA 12c" '(contract gradual)
        "OOPSLA 12d" '(gradual tr)
        "PADL 12" '(tr)
        "POPL 12" '(other)
        "SNAPL 15" '(lang)
        ))

(define colors
  (hash 'tr "green"
        'lang "lightblue"
        'verify "purple"
        'other "gray"
        'gradual "yellow"
        'contract "orange"
        'compiler "pink"))

(define tag-names
  (hash 'tr "Typed Racket"
        'lang "Languages"
        'verify "Verification"
        'other "Other"
        'gradual "Gradual Types"
        'contract "Contracts"
        'compiler "Compilers"))

(define w (pict-width (t/cant "E" size3)))

(define all-tags (apply append (hash-values papers*)))

(define papers (sort (for/list ([(p t) papers*]) (cons p t)) string<=? #:key car))
(define num (length papers))

(define (legend tags)
  (define d (for/list ([(tag name) tag-names])
              (define f (if (memq tag tags) values ghost))
              (define n (t/cant name (- size3 5)))
              (cons (f n)
                    (f (filled-rectangle w (pict-height n) #:color (hash-ref colors tag)
                                         #:border-color (hash-ref colors tag))))))
  (hc-append 20
   (apply vl-append (map car d))
   (apply vl-append (map cdr d))))

(define paper-procs
  (for/list ([(paper tags) (in-dict papers)])
    (define p (t/cant paper size3))
    (λ (keep-tags)
      (define tag-colors (apply ht-append
                                (for/list ([t tags])
                                  ((if (memq t keep-tags) values ghost)
                                   (filled-rectangle w (pict-height p)
                                                     #:border-color (hash-ref colors t)
                                                     #:color (hash-ref colors t))))))
      ((if (ormap (λ (t) (memq t keep-tags)) tags) values (λ (p) (cellophane p .25)))
       (lc-superimpose tag-colors p)))))

(define (paper-slide tags)
  (slide
   (hc-append 50
              (legend tags)
              (ht-append 20
                         (apply vl-append 20 (for/list ([p (take (drop paper-procs 0) (floor (/ num 3)))])  (p tags)))
                         (apply vl-append 20 (for/list ([p (take (drop paper-procs (floor (/ num 3))) (floor (/ num 3)))])  (p tags)))
                         (apply vl-append 20 (for/list ([p (take (drop paper-procs (floor (* 2 (/ num 3)))) (floor (/ num 3)))]) (p tags)))))))

(paper-slide all-tags)

(paper-slide '(compiler tr gradual lang))
