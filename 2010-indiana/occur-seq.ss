#lang slideshow

(require slideshow/code unstable/gui/slideshow "lib.ss"
         (for-syntax syntax/parse) slideshow/balloon)

(provide occur-seq)

(require "config.ss")

(define (balloon-on p txt #:inner [inner p]  #:dir [dir 'sw] #:find [find (case dir
                                                                            [(sw) rt-find]
                                                                            [(nw) rb-find]
                                                                            [(se) lt-find]
                                                                            [(ne) lb-find]
                                                                            [(w)  rc-find]
                                                                            [(e)  lc-find]
                                                                            [(n)  cb-find]
                                                                            [(s)  ct-find])])
  (define-values (s1 s2) (case dir
                           [(sw) (values -1 1)]
                           [(nw) (values -1 -1)]
                           [(ne) (values 1 -1)]
                           [(se) (values 1 1)]
                           [(w) (values -1 0)]
                           [(e) (values 1 0)]
                           [(s) (values 0 1)]
                           [(n) (values 0 -1)]))
  (refocus (pin-balloon (wrap-balloon (inset txt -5) dir (* s1 15) (* s2 15)) p inner find) p))


(define (mk-untyped name)
    (pict-case 
     name
     [(number?)
      (code 
       ||
       (define (f x)
         (if (number? x)
             (add1 x)
             0)))]
     [(else)
      (code
       (code:comment "s is a symbol, number or string")
       (define (->string s)
         (cond [(symbol? s) (symbol->string s)]
               [(number? s) (number->string s)]
               [else s])))]
     [(and)
      (code 
       (define (g x y)
         (cond [(and (number? x) (string? y)) 
                (+ x (string-length y))]
               [(number? x) (+ x y)]
               [else y])))]
     [else (code (: ->string ((U Symbol Number String) -> String)))]))

(define big-g (ghost (mk-untyped 'and)))

(define (occur-seq)    
  (staged
   [number? number?-typed else else-typed and and-typed]
   (if (odd? stage)       
       (slide 
        #:title "Scheme Idioms"
        (smod #:name (symbol->string stage-name) #:space (not (eq? stage-name 'and))
              (if (eq? stage-name 'and)
                  (vl-append (code (code:contract g : Any (U String Number) -> Number))
                             (mk-untyped stage-name))
                  (mk-untyped stage-name)))
        (ghost big-bot))
       (do-occur stage))))

(define (do-occur stage)
  (case stage
    [(2) (occur1)]
    [(4) (occur3)]
    [(6) (start)(occur4)]))

(define-syntax red-x
  (syntax-parser 
   [(red-x c:expr when:id ...)
    #'(case stage-name
        [(when ...) (red-code c)]
        [else (code c)])]
   [(red-x c:expr #:else other when:id ...)
    #'(case stage-name
        [(when ...) (red-code c)]
        [else other])]))

(code-scripts-enabled #t)

(define ecloud (cloud 30 20))

(define big-bot
  (vc-append (htl-append 50 (code N_x) (code N_x ⊃ #,(overbar (code Str_y))))
             (hline 250 5)
             (overbar (code Str_y))))

(define-syntax-rule (balloon-x c typ stages ...)
  (pict-case 
   stage-name
   [(stages ...) (balloon-on (red-code c) (code typ))]
   [else (code c)]))

(define-syntax-rule (balloon-x* c typ args ... (stages ...))
  (pict-case 
   stage-name
   [(stages ...) (balloon-on args ... (red-code c) (code typ))]
   [else (code c)]))

(define (occur1)
  (slide/staged
   [one two three four five six seven* seven  eight eight*]
   #:title "Filters & Objects"
   (let ()
     [define first-x (red-x x two)]
     [define third-x (red-x x four eight eight*)]
     [define env-code
       (pict-case
        stage-name
        [(eight*)
         (vl-append 
          (code env: #,(red-code |x:Number|))
          (code type: Number))]
        [else
         (vl-append 
          (code env: #,(red-code |x:Any| + Number_x))
          (code ||))])]
     [define num?-x
       (pict-case
        stage-name
        [(seven*)
         (vl-append (code type: Boolean)
                    (code filter: #,(red-code (apply-filter Number x))))]
        [else
         (vl-append (code type: Boolean)
                    (code filter: #,(red-code Number_x)))])]
     [define mod
       (tmod #:sizeof big-g #:name "number?"
             (code
              (: f (Any -> Number))
              (define (f #,first-x)
                (if #,(case stage-name
                        [(seven seven*)
                         (balloon-x* (number? x) #,num?-x #:dir 'nw (seven seven*))]
                        [else (code (#,(case stage-name [(five) (balloon-on (red-code number?) (code type: (Any -> Boolean : #,(red-code Number))) #:dir 'nw)] [else (code number?)]) #,(balloon-x* x #,(case stage-name [(three) (code type: Any)] [(six) (vl-append (code type: Any) (code object: #,(red-code x)))]) #:dir 'nw (three six))))]) 
                    (add1 #,third-x)
                    0))))]
     (case stage-name
       [(two) (balloon-on mod (code type: Any) #:inner first-x      #:dir 'w)]
       [(four) (balloon-on mod (code type: Number) #:inner third-x  #:dir 'nw)]
       [(eight) (balloon-on mod env-code #:inner third-x #:dir 'nw)]
       [(eight*) (balloon-on mod env-code #:inner third-x #:dir 'nw)]
       [else mod]))
   (ghost big-bot)))

(define (occur2)
  (slide/staged
   [one two three four five]
   #:title "Occurrence Typing 2" #:name "member"
   (tmod
    (pict-case
     stage-name
     [(one)
      (code     
       (cond [(member x l) => car]
             [else (error "not found")]))]
     [(two three four five)
      (code
       (let ([tmp (member x l)])
         (if #,(red-x tmp three four)
             (car #,(red-x tmp four five))
             (error "not found"))))]))
   (pict-case
    stage-name
    [(one two) (blank 0)]
    [(three) 
     (code tmp : (U #f (Pair A (Listof A))))]
    [(four) 
     (code tmp : (U #f (Pair A (Listof A))) |;| #,(lt-superimpose (hline (pict-width (code N)) 5) (code N_x)) |;| #,ecloud)]
    [(five) (code tmp : (Pair A (Listof A)))]
    [else (ghost big-bot)])))


(define (occur3)
  (slide/staged
   [one two three four five six pre-seven seven post-seven post-seven2 
        pre-eight3 pre-eight2 pre-eight eight post-eight post-eight2
        nine1 nine2]
   #:title "Then & Else"
   (let ()
     (define num?-s-balloon (vl-append (code type: Boolean)
                                       (code filter: Number_s #,(red-code \|) #,(colorize (overbar (red-code Number_s)) "red"))))
     (define sym?-s-balloon (vl-append (code type: Boolean)
                                       (code filter: Symbol_s #,(red-code \|) #,(colorize (overbar (red-code Symbol_s)) "red"))))
     (define usns (code type: (U Symbol Number String)))
     (define uns (code type: (U Number String)))
     (define s (code type: Symbol))
     (define n (code type: Number))
     (define sym? (red-x symbol? pre-seven seven))
     (define num? (red-x number? pre-eight eight pre-eight2 pre-eight3))
     (define sym->str (red-x (symbol->string #,(balloon-x* s #,s #:dir 'sw (three))) post-seven post-seven2))
     (define num->str (red-x (number->string #,(balloon-x* s #,n #:dir 'nw (five))) post-eight post-eight2 pre-eight2 pre-eight3))
     (define third-s (red-x s four eight pre-eight2 pre-eight3))
     (define fourth-s (red-x s six nine1 nine2 pre-eight2 pre-eight3))
     (define the-mod
       (tmod
        #:sizeof big-g #:name "else"
        (code        
         (: ->string ((U Symbol Number String) -> String))
         (define (->string s)
           (cond [#,(balloon-x* #,(code (#,sym? #,(balloon-x s #,usns two))) #,sym?-s-balloon (seven)) #,sym->str]
                 #,(red-x [#,(balloon-x* #,(code (#,num? #,(balloon-x* #,third-s #,uns #:dir 'nw (four)))) #,num?-s-balloon #:dir 'nw (eight)) #,num->str] pre-eight2 pre-eight3)
                 #,(red-x [else #,(balloon-x* #,fourth-s #,(code type: String) #:dir 'nw (six))] pre-eight2 pre-eight3 nine1 nine2))))))
     (case stage-name
       [(pre-seven) (balloon-on
                     the-mod
                     (code type: (Any -> Boolean : Symbol #,(red-code \|) #,(colorize (overbar (red-code Symbol)) "red"))) 
                     #:inner sym? #:find (move-find-left ct-find 30))]
       [(post-seven post-seven2)
        (balloon-on
         the-mod
         (case 
          stage-name
          [(post-seven)  (code env: |s:(U Symbol Number String)| + Symbol_s)]
          [(post-seven2) (code env: |s:Symbol|)]
          [else (blank 1)])
         #:inner sym->str #:dir 's #:find (move-find-left ct-find 0))]
       [(pre-eight) (balloon-on 
                     the-mod 
                     (code type: (Any -> Boolean : Number #,(red-code \|) #,(colorize (overbar (red-code Number)) "red"))) 
                     #:inner num? #:dir 'nw #:find (move-find-left cb-find 30))]       
       [(pre-eight3) (balloon-on 
                      the-mod 
                      (code env: |s:(U Symbol Number String)| #,(red-code +) #,(colorize (overbar (red-code Symbol_s)) "red"))
                      #:inner fourth-s #:dir 'n #:find (move-find-left cb-find -10))]
       [(pre-eight2) (balloon-on 
                      the-mod 
                      (code env: |s:(U Number String)|)
                      #:inner fourth-s #:dir 'n #:find (move-find-left cb-find -10))]
       [(post-eight post-eight2)
        (balloon-on
         the-mod
         (case 
          stage-name
          [(post-eight)  (code env: |s:(U Number String)| + Number_s)]
          [(post-eight2) (code env: |s:Number|)]
          [else (blank 1)])
         #:inner num->str #:dir 'n #:find (move-find-left cb-find 0))]
       [(nine1) (balloon-on 
                 the-mod 
                 (code env: |s:(U Number String)| #,(red-code +) #,(colorize (overbar (red-code Number_s)) "red"))
                 #:inner fourth-s #:dir 'n #:find (move-find-left cb-find -10))]
       [(nine2) (balloon-on 
                 the-mod 
                 (code env: |s:String|)
                 #:inner fourth-s #:dir 'n #:find (move-find-left cb-find -10))]
       [else the-mod]))
   (ghost big-bot)))


(define (occur4)
  (staged
   [one two three four five six seven eight nine]
   (define (both-filter p)
     (code filter: #,p \| #,(overbar p)))
   (define S_y (both-filter (code String_y)))
   (define N_x (both-filter (code Number_x)))
   (define and-expr
     (red-x (and #,(balloon-x* (number? x) #,N_x #:dir 's (two)) #,(balloon-x* (string? y) #,S_y #:dir 's (three))) four five))
   (define num? (red-x (number? x) six seven))
   (define plus (red-x (+ x y) eight nine))
   (define mod
     (tmod #:sizeof big-g  #:name "and" #:space #f
           (vl-append
            (code (: g (Any (U Number String) -> Number)))
            (code
             (define (g x y)
               (cond [#,and-expr 
                      (+ x (string-length y))]
                     [#,num? #,plus]
                     [else y]))))))
   (slide
    #:title "Logical Reasoning"
    (case stage-name
      [(four five) (balloon-on 
                    mod #:inner and-expr #:dir 's
                    (pict-case 
                     stage-name
                     [(four)
                      (code filter: Number_x String_y \|)]
                     [else 
                      (code filter: Number_x String_y \| #,(red-code Number_x) #,(red-code ⊃) #,(colorize (overbar (red-code String_y)) "red"))]))]
      [(six seven)
       (balloon-on 
        mod #:inner num? #:dir 'nw
        (pict-case 
         stage-name
         [(six)
          (code 
           env: #,(red-code Number_x) #,(red-code ⊃) #,(colorize (overbar (red-code String_y)) "red")
           filter: Number_x)]
         [(seven)
          (code 
           env: #,(code Number_x) #,(code ⊃) #,(overbar (code String_y))
           filter: Number_x #,(colorize (overbar (red-code String_y)) "red"))]))]
      [(eight nine)
       (balloon-on 
        mod #:inner plus #:dir 'n
        (pict-case 
         stage-name
         [(eight)
          (code 
           env: |x:Any| |   y:(U Number String)| + #,(red-code Number_x) #,(colorize (overbar (red-code String_y)) "red"))]
         [(nine)
          (code env: |x:Number| |y:Number|)]))]
      [else mod])
    (ghost big-bot))))


