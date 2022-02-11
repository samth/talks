#lang slideshow

(require slideshow/code (planet cce/scheme:6/slideshow))
(provide class-slide class-code)
(define (class-code #:super [super #f] #:comment? [com? #f] #:comment2 [com2 #f] #:comment3 [com3 #f] #:type [type #f])
  (define c
    (code 
     #,(pict-cond [com? (code (code:comment "Start here:"))]
                  [com2 (code (code:contract main : stx bool stx |       id | id stxs stxs -> stx))]
                  [com3 (code (code:contract main : stx bool stx (or #f id) id stxs stxs -> stx))]
                  [type (code (: main (Stx Bool Stx (U  #f Id) Id Stxs Stxs -> Stx)))]
                  [else (code ||)])
     ;; main : syntax boolean syntax identifier (or #f) identifier  (listof identifier) (listof syntax)
     (define (main stx trace-flag super-expr 
                   deserialize-id-expr name-id
                   interface-exprs defn-and-exprs)
       #,(parameterize ([current-code-tt
                         (lambda (s) (text s (current-code-font) 6))])
           (hc-append (blank 100)
           (code
            (let-values ([(this-id) #'this-id]
                         [(the-obj) (datum->syntax (quote-syntax here) (gensym 'self))]
                         [(the-finder) (datum->syntax (quote-syntax here) (gensym 'find-self))])
              
              (let* ([def-ctx (syntax-local-make-definition-context)]
                     [localized-map (make-bound-identifier-mapping)]
                     [any-localized? #f]
                     [localize/set-flag (lambda (id)
                                          (let ([id2 (localize id)])
                                            (unless (eq? id id2)
                                              (set! any-localized? #t))
                                            id2))]
                     [bind-local-id (lambda (id)
                                      (let ([l (localize/set-flag id)])
                                        (syntax-local-bind-syntaxes (list id) #f def-ctx)
                                        (bound-identifier-mapping-put!
                                         localized-map
                                         id
                                         l)))]
                     [lookup-localize (lambda (id)
                                        (bound-identifier-mapping-get
                                         localized-map
                                         id
                                         (lambda ()
                                           (code:comment "If internal & external names are distinguished,")
                                           (code:comment "we need to fall back to localize:")
                                           (localize id))))])
                
                (code:comment "----- Expand definitions -----")
                (let ([defn-and-exprs (expand-all-forms stx defn-and-exprs def-ctx bind-local-id)]
                      [bad (lambda (msg expr)
                             (raise-syntax-error #f msg stx expr))]
                      [class-name (if name-id
                                      (syntax-e name-id)
                                      (let ([s (syntax-local-infer-name stx)])
                                        (if (syntax? s)
                                            (syntax-e s)
                                            s)))])
                  
                  
                  (code:comment "------ Basic syntax checks -----")
                  (for-each (lambda (stx)
                              (syntax-case stx (-init init-rest -field -init-field inherit-field
                                                      private public override augride
                                                      public-final override-final augment-final
                                                      pubment overment augment
                                                      rename-super inherit inherit/super inherit/inner rename-inner
                                                      inspect)
                                [(form orig idp ...)
                                 (and (identifier? (syntax form))
                                      (or (free-identifier=? (syntax form) (quote-syntax -init))
                                          (free-identifier=? (syntax form) (quote-syntax -init-field))))]))))))))))))
    (if super (cc-superimpose (titlet "+ 900 lines") c) c))

(define (class-slide #:title [title "What's not so good"])
  (slide #:title title
       #:layout 'center
       (blank 10)
       'alts
       (list
        ;(list (class-code))
        (list (class-code #:super #t))
        (list (class-code #:super #t #:comment? #t))
        (list (class-code #:super #t #:comment2 #t))
        (list (class-code #:super #t #:comment3 #t))
        (list (class-code #:super #t #:type #t)))
       ))
