(module eli-talk (lib "slideshow.ss" "slideshow")
  (require (lib "kw.ss") (lib "etc.ss") (lib "class.ss") (lib "mred.ss" "mred")
           (lib "setf.ss" "swindle") (lib "misc.ss" "swindle")
           (lib "symbol.ss" "texpict")
           (lib "code.ss" "slideshow") (lib "step.ss" "slideshow"))

  (define show-all? #t)
  (define (in-this-dir f)
    (build-path (this-expression-source-directory) f))
  (define icon-file "icon.png")
  (define logo-file "logo.png")
  (set-margin! 20)
  (set-page-numbers-visible! #f)

  (define (split l x)
    (let loop ([l l] [cur '()] [ls '()])
      (cond [(null? l) (reverse! (cons (reverse! cur) ls))]
            [(eq? x (car l)) (loop (cdr l) '() (cons (reverse! cur) ls))]
            [else (loop (cdr l) (cons (car l) cur) ls)])))

  (define fade-bg
    (let* ([margin*2 (* margin 2)]
           [margin/2 (/ margin 2)]
           [w (+ margin*2 client-w)]
           [h (+ margin*2 client-h)]
           [trans  (make-object brush% "white" 'transparent)]
           [inside (make-object brush% "white" 'solid)]
           [1st (make-object color% #x20 #x00 #x20)]
           [2nd (make-object color% "white")])
      (define (draw dc x y)
        (define (draw-one i)
          (send dc draw-rectangle (+ x i) (+ y i) (- w (* 2 i)) (- h (* 2 i))))
        (let ([b (send dc get-brush)]
              [p (send dc get-pen)])
          (send dc set-brush trans)
          (color-series dc margin/2 1 1st 2nd draw-one #t #t)
          (send dc set-brush inside)
          (draw-one margin/2)
          (send dc set-pen p)
          (send dc set-brush b)))
      (inset (dc draw w h 0 0) (- margin))))

  (define (scale* p s)
    ;; scale/improve-new-text is problematic for items
    (let ([p (if (pict? p) p (t p))]) (scale/improve-new-text p s)))
  (define/kw (big   p #:optional [scale 3/2]) (scale* p scale))
  (define/kw (small p #:optional [scale 3/2]) (scale* p (/ scale)))

  (define icon (bitmap (in-this-dir icon-file)))
  (define logo (bitmap (in-this-dir logo-file)))
  (define blue-arrow (colorize (arrow gap-size 0) "blue"))
  (define small-blue-arrow (small blue-arrow))
  (define --- sym:emdash)
  (define *up* (blank 0 -48))

  (define (res s) (colorize (tt (format "~a" s)) "blue"))

  (define (append: . body) (cons 'append body))
  (define (flash: . body) (cons 'flash body))

  (define-syntax (n-alts: stx)
    (syntax-case stx ()
      [(_ num body ...)
       (let ([id (lambda (s) (datum->syntax-object stx s stx))])
         (with-syntax ([before? (id 'before?)]
                       [before  (id 'before)]
                       [upto    (id 'upto)]
                       [upto?   (id 'upto?)]
                       [after?  (id 'after?)]
                       [after   (id 'after)]
                       [from?   (id 'from?)]
                       [from    (id 'from)]
                       [only?   (id 'only?)]
                       [only    (id 'only)])
           #'(append: 'alts
               (map (lambda (n)
                      (let* ([v (lambda (p?)
                                  (lambda (n p) ((if (p? n) values ghost) p)))]
                             [before? (lambda (m) (< n m))]
                             [upto?   (lambda (m) (<= n m))]
                             [after?  (lambda (m) (> n m))]
                             [from?   (lambda (m) (>= n m))]
                             [only?   (lambda (m) (= n m))]
                             [before  (v before?)]
                             [upto    (v upto?)]
                             [after   (v after?)]
                             [from    (v from?)]
                             [only    (v only?)])
                        (list body ...)))
                    (list-of n (n <- 1 .. num))))))]))

  (define ((make-steps-proc proc) . body)
    (if (memq 'next body) (list 'steps proc body) (apply proc body)))
  (define p:    (make-steps-proc page-para))
  (define p*:   (make-steps-proc page-para*))
  (define p/c:  (make-steps-proc page-para/c))
  (define p*/c: (make-steps-proc page-para*/c))
  (define p/r:  (make-steps-proc page-para/r))
  (define p*/r: (make-steps-proc page-para*/r))
  (define item: (make-steps-proc page-item))
  (define item-: (make-steps-proc
                  (lambda body
                    (apply page-item/bullet (ghost bullet) body))))
  (define subitem: (make-steps-proc page-subitem))

  (define ((make-cols: para) . ts)
    (let* ([w (/ client-w (length ts))])
      (apply htl-append
             (map (lambda (x) (para w (if (string? x) (t x) x))) ts))))
  (define cols:   (make-cols: para))
  (define cols/c: (make-cols: para/c))

  (define current-icon-pos (make-parameter #f))
  (define ((make-assembler) title v-sep contents)
    (ct-superimpose
     (cond [(current-icon-pos) =>
            (lambda (p) (lb-superimpose fade-bg (hb-append (blank p 0) icon)))]
           [else fade-bg])
     (if title (vc-append v-sep (titlet title) contents) contents)))

  (define (process-slide body)
    (let loop ([body body])
      (if (null? body)
        body
        (let ([head (car body)] [rest (loop (cdr body))])
          (case (and (pair? head) (car head))
            [(flash)
             (if (pair? rest)
               `(alts (,(loop (cdr head)) ,rest))
               (cdar body))]
            [(steps)
             `(,@(let ([l (split (caddr head) 'next)]
                       [f (cadr head)])
                   (if (null? l)
                     (loop (list (apply f (caddr head))))
                     (let loop1 ([l (reverse l)]
                                 [r '()])
                       (if (null? l)
                         (loop r)
                         (loop1 (cdr l)
                                `((flash ,(apply f (apply append (reverse l))))
                                  ,@r))))))
               ,@rest)]
            [(append) `(,@(loop (cdr head)) ,@rest)]
            [else `(,head ,@rest)])))))

  (define slides '())
  (define outlines '())
  (define (slide*: . body)
    (push! `(slide ,@(process-slide body)) slides))
  (define outline*:
    (let ([counter 0] [last-title #f])
      (lambda (title . comment)
        (if (equal? title last-title)
          (push! comment (cddar outlines))
          (begin (set! counter (add1 counter))
                 (push! `(,counter ,title ,comment) outlines)
                 (set! last-title title)))
        (push! `(outline ,counter) slides))))
  (define (slide: . body)
    (when show-all? (apply slide*: body)))
  (define (outline: . body)
    (when show-all? (apply outline*: body)))
  (define (make-slides)
    (let* ([slides   (reverse slides)]
           [outlines (reverse outlines)]
           [icon-delta (/ (- (pict-width full-page) (pict-width icon))
                          (max 1 (- (length slides) 2)))])
      (define outline
        (apply
         make-outline
         (apply append
                (mapply
                 (lambda (tag title . comments)
                   (let ([comments (reverse! comments)])
                     (list tag title
                           (and (not (andmap null? comments))
                                (lambda (_)
                                  (let ([comment (pop! comments)])
                                    (if (pair? comment)
                                      (apply para (* 3/4 client-w) comment)
                                      (blank 0 0))))))))
                 outlines))))
      (current-slide-assembler (make-assembler))
      ;(start-making-slides)
      (dolist (args slides)
        (case (car args)
          [(outline) (outline (cadr args))]
          [(slide)   (if (cadr args)
                       (apply slide/title/center (cdr args))
                       (apply slide/center (cddr args)))])
        (current-icon-pos
         (cond [(current-icon-pos) => (lambda (p) (+ icon-delta p))]
               [else 0])))
      ;(done-making-slides)
      ))

  (slide: #f
    (big (bt "A Self-Hosting Evaluator using HOAS"))
    (t "Eli Barzilay")
    (t "Northeastern University"))

  ;; (outline: "Introduction")

  (slide: "Basic Idea"
    (item: "Richer representations are better:")
    (n-alts: 2
      (table 3
        (let ([row (lambda (n a b)
                     (list (from n (t a))
                           (from n (t "vs."))
                           (from n (t b))))])
          `(,@(row 1 "Gödel Numbers" "Character Strings")
            ,@(row 1 "Flat Strings"  "S-Expressions")
            ,@(row 2 "S-Expressions" "Functions")))
        (list rc-superimpose cc-superimpose lc-superimpose)
        cc-superimpose
        gap-size gap-size)))

  (slide: "HOAS Encoding of Syntax in Scheme"
    (p: "Say that we want to represent the following syntax:")
    (code (lambda (x) (+ x 1)))
    'next
    (p: "Plain s-expression:")
    (code `(lambda (x) (+ x 1)))
    'next
    (p: "HOAS representation:")
    (code (lambda (x) `(+ ,x 1)))
    (p: "or")
    (code `(lambda ,(lambda (x) `(+ ,x 1)))))

  (slide: "Higher Order Abstract Syntax"
    (item: "HOAS is a representation for syntax with binders")
    (item: "Has been gaining popularity fairly recently")
    'next
    (item: (bt "Simple"))
    (item: (bt "Robust") (small " (including subtleties)"))
    (item: (bt "Efficient")
           (small " (no bookkeeping, and we're now jitting...)"))
    (item: (bt "Good Integration")
           (small " (errors, subtleties, DrScheme tools, macros)")))

  (slide: "HOAS & Scheme"
    (item: "HOAS is popular in strict languages")
    (item: "Not as much in Scheme")
    (subitem: (small "(Maybe due to superior macro support?)"))
    'next
    (p: "This is unfortunate, since it is even more natural to use in Scheme:")
    (item: "Representation validity is easy to do dynamically")
    (subitem: (small "(vs. hairy & exotic types)"))
    (item: "Need to transform concrete syntax to HOAS:"
           "in Scheme this is easily done with macros"))

  ;; (outline: "Evaluator Implementation")

  (slide: "Simple Evaluator"
    (p: "We construct a tiny evaluator, enhancing it in steps until it can"
        "bootstrap itself."))

  (define (ctt x) (colorize (tt x) (current-comment-color)))

  (slide: "Creating HOAS Encodings"
    (p: "Need a way to create an encoding from concrete syntax:")
    (htl-append (code (lambda (x) (+ x 1)))
                (t "  to  ")
                (code (lambda (x) `(+ ,x 1))))
    'next
    (p: "In Scheme, this is easy with a macro:")
    (n-alts: 2
      (small
       (code
        (code:comment "Translates simple terms into a HOAS representation")
        (code:comment "values, which are:")
        (code:comment #,(ctt "Term = atom                      ") "; literals")
        (code:comment #,(ctt "     | (list 'if Term Term Term) ") "; conditionals")
        (code:comment #,(ctt "     | (Term ... -> Term)        ") "; abstractions")
        (code:comment #,(ctt "     | (list Term ...)           ") "; applications")
        code:blank
        (define-syntax Q
          (syntax-rules (lambda if)
            [(Q (lambda args b)) (lambda args (Q b))]
            [(Q (if x y z))      (list 'if (Q x) (Q y) (Q z))]
            [(Q (f x ...))       (list (Q f) (Q x) ...)]
            [(Q x)               #,(colorize
                                    (if (only? 1)
                                      (htl-append
                                       (colorize (tt "'") (current-literal-color))
                                       (tt "x"))
                                      (pin-over (tt "x") 0 0
                                                (ctt "     ; share Scheme bindings")))
                                    (current-id-color))])))
       1.2)))

  (slide: "Implementing an Evaluator"
    (n-alts: 2
      (small
       (code
        (code:contract ev : Term -> Val)
        (code:comment "evaluates an input term into a Scheme value")
        (define (ev expr)
          (cond [(not (pair? expr)) expr]
                [(eq? 'if (car expr))
                 (ev (if (ev (cadr expr))
                       (caddr expr)
                       (cadddr expr)))]
                [else (ev (#,(if (from? 2) (colorize (tt "apply") "red") (code apply)) (ev (car expr))
                                 (map ev (cdr expr))))])))
       1.1))
    (p: "The" (tt "apply") "is either calling a Scheme procedure,"
        "or a substitution"))

  (slide: "Making the Evaluator Lazy"
    (small
     (code
      (code:contract ev* : Term -> Val)
      (code:comment "evaluates an input term into a Scheme value,")
      (code:comment "lazy version")
      (define (ev* expr)
        (cond [(not (pair? expr)) expr]
              [(eq? 'if (car expr))
               (ev* (if (ev* (cadr expr))
                      (caddr expr)
                      (cadddr expr)))]
              [else (ev* (let ([f (ev* (car expr))])
                           (apply f (if (primitive? f)
                                      (map ev* (cdr expr))
                                      (cdr expr)))))])))
     1.1))

  (slide: "Making the Evaluator Lazy"
    (small
     (code
      > (ev (Q ((lambda (x y z) (if x y z))
                #t (display "true\n") (display "false\n"))))
      true
      false
      > (ev* (Q ((lambda (x y z) (if x y z))
                 #t (display "true\n") (display "false\n"))))
      true
      > (ev* (Q (((lambda (f)
                    ((lambda (x) (f (x x)))
                     (lambda (x) (f (x x)))))
                  (lambda (fact)
                    (lambda (n)
                      (if (zero? n) 1 (* n (fact (- n 1)))))))
                 5)))
      120)
     1.2))

  (slide: "Self-Hosting Evaluator"
    (p: "Use a new struct type:")
    (small
     (code
      (code:comment "A type for syntax representation values")
      (code:comment #,(ctt "Term = atom                           ") "; literals")
      (code:comment #,(ctt "     | (term 'if Term Term Term)      ") "; conditionals")
      (code:comment #,(ctt "     | (term 'lam (Term ... -> Term)) ") "; abstractions")
      (code:comment #,(ctt "     | (term 'app Term ...)           ") "; applications")
      (define-struct term (tag exprs) #f)
      (define (term tag . args) (make-term tag args)))
     1.1))

  (slide: "Self-Hosting Evaluator"
    (small
     (code
      (code:comment "Translates terms into a HOAS representation")
      (define-syntax (Q s)
        (let transform ([s s])
          (syntax-case s (Q quote lambda if let and or cond case else delay)
            [(Q (Q x)) (code:comment "transform once, then reprocess:")
             (with-syntax ([1st-pass (transform (syntax (Q x)))])
               (syntax (Q 1st-pass)))]
            [(Q (quote x))           (syntax 'x)]
            [(Q (lambda args b))     (syntax (term 'lam (lambda args (Q b))))]
            [(Q (if x y z))          (syntax (term 'if  (Q x) (Q y) (Q z)))]
            [(Q (let ([x v] ...) b)) (syntax (Q ((lambda (x ...) b) v ...)))]
            [(Q (and))               (syntax #t)]
            [(Q (and x))             (syntax x)]
            [(Q (and x y ...))       (syntax (Q (if x (and y ...) #f)))]
            [(Q (or))                (syntax #f)]
            [(Q (or x))              (syntax x)]
            [(Q (or x y ...))        (syntax (Q (let ([x* x]) (if x* x* (or y ...)))))]
            [(Q (cond))              (syntax 'unspecified)]
            [(Q (cond [else b]))     (syntax (Q b))]
            [(Q (cond [test b] clause ...))
                                     (syntax (Q (if test b (cond clause ...))))]
            [(Q (case v))            (syntax 'unspecified)]
            [(Q (case v [else b]))   (syntax (Q b))]
            [(Q (case v [(tag) b] clause ...)) (code:comment "(naive translation)")
                                     (syntax (Q (if (eqv? v 'tag) b (case v clause ...))))]
            [(Q (delay x))           (syntax (delay (Q x)))]
            [(Q (f x ...))           (syntax (term 'app (Q f) (Q x) ...))]
            [(Q x)                   (syntax x)]))))
     5/3))

  (slide: "Self-Hosting Evaluator"
    (flash:
     (small
      (code
       (code:contract ev : Term -> Val)
       (code:comment "evaluates an input term into a Scheme value")
       (define (ev expr)
         (if (term? expr)
           (let ([subs (term-exprs expr)])
             (case (term-tag expr)
               [(lam) expr]
               [(if)  (ev (if (ev (car subs))
                            (cadr subs)
                            (caddr subs)))]
               [(app) (let ([f (ev (car subs))]
                            [args (map ev (cdr subs))])
                        (cond [(and (term? f) (eq? 'lam (term-tag f)))
                               (ev (apply (car (term-exprs f)) args))]
                              [(procedure? f)
                               (apply f args)]
                              [else (error 'ev "bad procedure")]))]
               [else (error 'ev "bad tag")]))
           expr)))
      4/3))
    (small
     (code
      (code:contract ev* : Term -> Val)
      (code:comment "evaluates an input term into a Scheme value,")
      (code:comment "uses call-by-need")
      (define (ev* expr)
        (cond
          [(term? expr)
           (let ([subs (term-exprs expr)])
             (case (term-tag expr)
               [(lam) (lambda args
                        (ev* (apply (car subs)
                                    (map (lambda (a)
                                           (delay (ev* a)))
                                         args))))]
               [(if)  (ev* (if (ev* (car subs))
                             (cadr subs)
                             (caddr subs)))]
               [(app) (apply (ev* (car subs)) (cdr subs))]
               [else (error 'ev* "bad tag")]))]
          [(promise? expr) (ev* (force expr))]
          [(primitive*? expr)
           (lambda args (apply expr (map ev* args)))]
          [else expr])))
     4/3))

  (slide: "It can run itself"
    (flash:
     (small
      (code
       (define ev1
         (ev (Q (Y (lambda (ev)
                     (lambda (expr)
                       (cond
                         [(term? expr)
                          (let ([subs (term-exprs expr)])
                            (case (term-tag expr)
                              [(lam) (lambda args
                                       (ev (apply (car subs) args)))]
                              [(if)  (ev (if (ev (car subs))
                                           (cadr subs)
                                           (caddr subs)))]
                              [(app) (apply (ev (car subs))
                                            (map ev (cdr subs)))]
                              [else (error 'ev1 "bad tag")]))]
                         [else expr])))))))
       #,(t " ")
       > (ev (Q (ev1 (Q (+ 1 2)))))
       3
       > (ev (Q (ev1 (Q ((lambda (x) (+ x 2)) 1)))))
       3)
      4/3))
    (flash:
     (small
      (code
       (define ev2
         (ev (Q (ev1 (Q (lambda (expr)
                          (code:comment
                           "Same code as ev1, substituting ev2 for ev1"))
                          ...)))))
       #,(t " ")
       > (ev (Q (ev1 (Q (ev2 (Q (+ 1 2)))))))
       3
       > (ev (Q (ev1 (Q (ev2 (Q ((lambda (x) (+ x 2)) 1)))))))
       3)
      4/3)))

  (slide: "More games"
    (small
     (code
      (define ev*1
        (ev* (Q (Y (lambda (ev)
                     (lambda (expr)
                       (cond
                         [(term? expr)
                          (let ([subs (term-exprs expr)])
                            (case (term-tag expr)
                              [(lam) (lambda args
                                       (ev (apply (car subs) args)))]
                              [(if)  (ev (if (ev (car subs))
                                            (cadr subs)
                                            (caddr subs)))]
                              [(app) (apply (ev (car subs))
                                            (map ev (cdr subs)))]
                              [else (error 'ev*1 "bad tag")]))]
                         [else expr])))))))
      #,(t " ")
      > (ev* (Q (ev*1 (Q (+ 1 2)))))
      3
      > (ev* (Q (ev*1 (Q ((lambda (x y) y) (+ 1 "2") 333)))))
      333)
     4/3))

  ;; (outline: "HOAS Problems")

  (slide: "HOAS Problems"
    (p: "HOAS comes with a price, two big problems:")
    (item: "Exotic terms")
    (subitem: "Some terms are invalid as representations")
    (p: "          " (code (term 'lam (lambda (x) (if (equal? x 1) 0 x)))))
    (p: "        "
        (small (htl-append (t "(") (code Term ... -> Term)
                           (t "  is not restricted enough)"))))
    'next
    (item: "Induction")
    (subitem: "How do you take")
    (p: "          " (code (term 'lam (lambda (x) (term 'app + x 1)))))
    (p: "        apart?"))

  (slide: "HOAS Problems"
    (p: "Solutions exist, easy in Scheme:")
    (item: "Exotic terms")
    (p: blue-arrow
        "Restrict construction to syntactically verifiable representations")
    'next
    (item: "Induction")
    (p: blue-arrow "These functions are very well behaved")
    (p: (ghost blue-arrow) "You can still take them apart:")
    (p: (ghost blue-arrow) (code (term 'lam (lambda (x) (term 'app + x 1)))))
    (p: (ghost blue-arrow)
        (code -> (term 'app
                       (lambda (x) +)
                       (lambda (x) x)
                       (lambda (x) 1)))))

  ;; (outline: "Conclusion")

  (slide: "Conclusion"
    (item: "HOAS is just as useful in Scheme, maybe more")
    (p: blue-arrow "Make your binders bind"))

  (slide: #f (t "סוף") (t "(The End)"))

  (make-slides))
