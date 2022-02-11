
#lang at-exp slideshow

(require slideshow/step slideshow/code slideshow/face 
         (only-in slideshow/slide title-size)
         (except-in "beamer.ss" title) "lib.ss" racket/gui  "thanks.ss" 
         "tslide.ss" "config.ss" "langs/main.rkt"
         "ts-intro.rkt" "stages.rkt" unstable/gui/slideshow
         "vulns.rkt")

(provide why-dsls as-comment atsign neg as-string as-datalog)



(define (hl p) (cc-superimpose (inset (cellophane (colorize (filled-rectangle (pict-width p) (pict-height p)) "pink") .7) -20)
                               p))

(define (as-string s)
  (colorize ((current-code-tt) s) literal-color))
(define (as-comment s)
  (colorize ((current-code-tt) s) comment-color))

(define (as-paren s)
  (colorize ((current-code-tt) s) keyword-color))

(define atsign
  (inset (as-paren "@") 0 0 (- (pict-width (code | |))) 0))

(define neg
  (inset (as-paren "") 0 0 (* 2 (- (pict-width (code | |)))) 0))

(define (as-datalog s)
  (apply hbl-append
         (for/list ([c s])
           (case c
             [(#\( #\) #\: #\- #\, #\. #\= #\?) (colorize ((current-code-tt) (string c)) keyword-color)]
             [else (colorize ((current-code-tt) (string c)) id-color)]))))

(define (why-dsls)


(tslide "Why Domain-Specific Languages?")


(code-colorize-enabled #t)



(slide (mini-slide (vr-append 60 hudak-quote perlis-quote)))

(define t! (code T!))
(define sw-comp (code switch-compare))
(define def/mt (code define/match))

(define rb-code (scale (code (code:comment "Red-Black deletion")
                             (#,def/mt (del node)
                                       ||
                               [(#,t! c l k v r) (code:comment "=>")
                                ||
                                (#,sw-comp (cmp key k)
                                  [<   (bubble c (del l) k v r)]
                                  [=   (remove node)]
                                  [>   (bubble c l k v (del r))])]
                               [else   node]))
                       1))

(define rb-code2 (highlight-on #:inset 7 rb-code sw-comp def/mt t!))

(define arcnt (code arcount))
(define ar2ct (launder arcnt))

(define bit-code
  (code (qdcount :: bits 16)
        (ancount :: bits 16)
        (nscount :: bits 16)
        (#,arcnt :: bits 16)
        (q-section :: 
          (t:ntimes qdcount (t:question packet)))
        (a-section ::
          (t:ntimes ancount (t:rr packet)))
        (auth-section ::
          (t:ntimes nscount (t:rr packet)))
        (additional-section ::
          (t:ntimes #,ar2ct (t:rr packet)))))

(slide/staged [analysis optimize reason express express2 ensure ensure2]
              #:title (colorize (vl-append (titlet "Q: Why DSLs?")
                                           (pict-case stage-name
                                             [(analysis) (titlet "A: Tractable analysis and verification")]
                                             [(optimize) (titlet "A: Improved code generation")]
                                             [(reason)   (titlet "A: Seamless sublanguages")]
                                             [(express express2)  (titlet "A: Express the problem directly")]
                                             [(ensure ensure2)   (titlet "A: Eliminate entire classes of errors")]))
                                "blue")
              (pict-case stage-name #:combine cc-superimpose
                [(analysis) (scale (bitmap "cobasa.png") .75)]
                [(optimize) (scale (bitmap "benchmarks-00.png") 1)]
                [(reason)   (parameterize ([current-keyword-list '("*" "/" "|" "&" "")]
                                           [code-colorize-enabled #f])
                              (define prin (black-code printer))
                              (define rx-sub (black-code (rx-subst
                                                    #,(blue-code (\| "John" "Paul" "George" "Ringo"))
                                                    (run/string
                                                     #,(red-code (wget -O - http://reviews.com/letitbe)))
                                                    pre "Beatle" post)))
                              (scale (code 
                                      (& #,(red-code (\| (gunzip) (html2ps) (lpr -P ,#,prin)))
                                         #,(red-code (<< ,#,rx-sub))))
                                     1.1))]
                [(express)  (scale rb-code 1.4)]
                [(express2)  (scale rb-code2 1.4)]
                [(ensure)   (scale bit-code 1.2)]
                [(ensure2)   (scale (connect bit-code arcnt ar2ct 8
                                             #:find1 rc-find #:find2 ct-find) 1.2)])))