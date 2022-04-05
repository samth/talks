
(module util mzscheme
  (require (lib "class.ss")
           (lib "list.ss")
           (lib "math.ss")
           (lib "slideshow.ss" "slideshow")
           (lib "code.ss" "slideshow"))
  
  (provide (all-defined))

  ;; font stuff
  
  (define-syntax with-font-factor
    (syntax-rules ()
      [(with-font-factor factor . body)
       (parameterize ((current-font-size (floor (* factor (current-font-size)))))
         . body)]))
  
  (define-syntax big
    (syntax-rules ()
      [(big e)
       (with-font-factor 3/2 e)]))
  
  (define-syntax small
    (syntax-rules ()
      [(_ e) (with-font-factor 3/4 e)]))
  
  (define-syntax tiny
    (syntax-rules ()
      [(_ e) (with-font-factor 1/2 e)]))

  ;; code stuff
  
  (define (code-pict? p)
    (and (code-pict-bottom-line-pict p) #t))
  
  (define-syntax codep
    (syntax-rules ()
      [(codep x props)
       (let ([c (code* x)])
         (small (codep* c props)))]))
  
  (define (codep* c props)
    (if (code-pict? c)
        (let* ([p (proplist/simple props)]
               [last0 (code-pict-bottom-line-pict c)]
               [last1 (htl-append (ghost last0) (lift-above-baseline p (* 3/3 (pict-height (t "X")))))]
               [last2 (let-values ([(x y) (lt-find c last0)])
                        (inset last1 x y 0 0))])
          (pict->code-pict (lt-superimpose c last2)
                           last1))
        (let ([p (proplist/simple props)])
          (htl-append c (lift-above-baseline p (* 4/3 (pict-height p)))))))
  
  (define-syntax code/prop
    (syntax-rules ()
      [(code/prop x . propspec)
       (let ([c (code x)])
         (pict->code-pict (stxprop c . propspec)
                          (code-pict-bottom-line-pict c)))]))
  
  (define-syntax stxprop
    (syntax-rules ()
      [(stxprop pict props)
       (stxprop pict props 40 -40)]
      [(stxprop pict props dx dy)
       (stxprop pict props dx dy lb-find)]
      [(stxprop pict props dx dy finder)
       (stxprop* pict (small props) dx dy finder)]))
  
  (define (stxprop* pict props dx dy finder)
    (let* ([p (proplist props)]
           [anchor (colorize (disk 8) "red")]
           [anchor-w-p (pin-over anchor dx dy p)])
      (let-values ([(dx1 dy1) (cc-find anchor-w-p anchor)]
                   [(dx2 dy2) (finder anchor-w-p p)])
        (let* ([arrow (colorize (pip-arrow-line (- dx2 dx1) (- dy2 dy1) 5) "red")]
               [anchor-w-arrow (pin-over anchor-w-p dx1 dy1 arrow)])
          (pin-over pict -4 0 anchor-w-arrow)))))

  (define (proplist props)
    (if (pair? props)
        (color-frame (highlight (inset (proplist/base props) 5) "white") "red")
        (blank 0)))

  (define (proplist/simple props)
    (if (pair? props)
        (color-frame (highlight (inset (proplist/base props) 2) "white") "red")
        (blank 0)))
  
  (define (proplist/base props)
    (define keys0
      (map (lambda (p) (small (tt (format "~a" (car p))))) props))
    (define key-width (apply max 0 (map pict-width keys0)))
    (define keys
      (map (lambda (p) (lc-superimpose p (blank key-width 1))) keys0))
    (apply vl-append 0
           (map (lambda (k p) (hbl-append 2 k (small (t " = ")) (cadr p)))
                keys props)))
  
  (define-syntax code/step
    (syntax-rules ()
      [(_ c) (vr-append (code* c) #; (stepper #'c))]))

  (define-syntax code/step/intro-hide
    (syntax-rules ()
      [(_ c) (vr-append (code* c) #;(stepper/intro-hide #'c))]))

  (define-syntax code/step/hide
    (syntax-rules ()
      [(_ c) (vr-append (code* c) #;(stepper/hide #'c))]))


  ;; Pseudocode
  
  (define (pcode arg)
    (small (if (string? arg) (tt arg) arg)))

  (define (pseudocode . args)
    (apply vl-append (map pcode (filter values args))))

  ;; Parse tree
  
  (define (node tag . items)
    (when (null? items)
      (error 'node "can't have zero items"))
    (lambda (show?)
      (let* ([items (map (lambda (p) (p show?)) items)]
             [start (car items)]
             [end (car (last-pair items))]
             [left (hc-append (apply vl-append items) (blank 5))]
             [wrap (if show? values ghost)])
        (define left-w (pict-width left))
        (define left-h (pict-height left))
        (define-values (start-dx start-dy) (rc-find left start))
        (define-values (end-dx end-dy) (rc-find left end))
;        (for-each (lambda (p)
;		    (define-values (dx dy) (rc-find left p))
;		    (set! left (pin-over left dx dy
;					 (wrap (pip-line (- left-w start-dx) 0 0)))))
;		  items)
        (let* ([left (pin-over left start-dx start-dy
                               (wrap (pip-line (- left-w start-dx) 0 0)))]
               [left (pin-over left end-dx end-dy
                               (wrap (pip-line (- left-w end-dx) 0 0)))]
               [left (pin-over left left-w start-dy
                               (wrap (pip-line 0 (- end-dy start-dy) 0)))])
          (hc-append left
                     (wrap (hline 10 10))
                     (wrap (colorize (small (t tag)) "blue"))
                     (blank 10))))))
  
  (define (ev tag)
    (lambda (show?)
      (lc-superimpose (small (t tag))
                      (ghost (small (t "xxxx-xxxxxxxxxxx"))))))
  
  ;; Grammar
  
  (define (gram s)
    (small (tt s)))
  
  ;; Labels
  
  (define (plabel pict from to dx label)
    (define-values (from-x from-y) (rc-find pict from))
    (define-values (to-x to-y) (rc-find pict to))
    (define the-x (+ (pict-width pict) dx))
    (define eps 5)
    (define bracket 10)
    (let* ([pict
            (pin-over pict 
                      (+ the-x 30)
                      (- (/ (+ from-y to-y) 2)
                         (/ (pict-height label) 2))
                      label)]
           [pict
            (pin-over pict
                      the-x
                      (+ from-y eps)
                      (pip-arrows-line 0 (- to-y from-y eps eps) 0))]
           [pict
            (pin-over pict
                      (- the-x bracket)
                      (+ from-y eps)
                      (pip-line (* 2 bracket) 0 0))]
           [pict
            (pin-over pict
                      (- the-x bracket)
                      (- to-y eps)
                      (pip-line (* 2 bracket) 0 0))])
      pict))
  
  ;; Code
  (define-syntax code*
    (syntax-rules ()
      [(code* e)
       (parameterize ((code-colorize-enabled #f))
         (code e))]))

  (define-syntax code/c
    (syntax-rules ()
      [(code/c c e)
       (colorize (code* e) c)]))

  (define-syntax code/m
    (syntax-rules ()
      [(code/m n e)
       (code/c (mark-color n) e)]))

  (define (mark-color n)
    (list-ref '("black" "red" "blue") n))
  
  (define (highlight pict color)
    (pin-under pict 0 0 
               (colorize
                (filled-rectangle (pict-width pict) (pict-height pict))
                color)))
  
  ;; macro hiding illustration
  
  (define (derivbox kind e1 e2)
    (ht-append 
     (if (pict? kind) kind (tiny (bt kind)))
     (frame (inset (vc-append e1 
                              (if e2
                                  (hc-append (arrow 20 (/ pi -2))
                                             (blank 30)
                                             e2)
                                  (blank)))
                   5))))
  (define (stepbox e1 e2)
    (frame (inset (vl-append (hc-append (blank 20 1) (blank 30 1) e1)
                             (hc-append (arrow 20 0)
                                        (blank 30 1) 
                                        e2))
                  5)))

  (define (hiding-example/old selection mode show-synth? lines)
    (define (sel sym pict)
      (if (eq? sym selection)
          (pin-under pict
                     -20
                     -20
                     (colorize (rectangle (+ 40 (pict-width pict))
                                          (+ 40 (pict-height pict)))
                               (case mode
                                 ((hide) "blue")
                                 ((seek) "red"))))
          pict))
    (define xA (derivbox "Macro"
                        (small (code* (when in-Rome? (do-as-Romans))))
                        (small (code* (if ---)))))
    (define xB (stepbox (small (code* (when in-Rome?
                                        (do-as-Romans))))
                       (small (code* (if in-Rome?
                                         (begin (do-as-Romans)))))))
    (define xC (derivbox "If"
                        (small (code* (if in-Rome? (begin (do-as-Romans)))))
                        (small (code* (if in-Rome? (begin ---))))))
    (define xD (derivbox "Variable"
                        (small (code* in-Rome?))
                        (small (code* in-Rome?))))
    (define CE (t "..."))
    (define xE (derivbox "Macro"
                         (small (code* (do-as-Romans)))
                         (small (code* ---))))
    (define S (colorize (derivbox "Synth"
                                  (small (code* (when in-Rome? (do-as-Romans))))
                                  (small (code* ???)))
                        "purple"))
    (define A (sel 'a xA))
    (define B (sel 'b xB))
    (define C (sel 'c xC))
    (define D (sel 'd xD))
    (define E (sel 'e xE))
    (define layerS (hc-append (blank 300 1) S))
    (define layer0 (hc-append (blank 200 1) A))
    (define layer1a (hc-append B (blank 360 1)))
    (define layer1b (hc-append (blank 360 1) C))
    (define layer2a (hc-append (blank 360 1) CE))
    (define layer2 (hc-append (blank 200 1) D (blank 40 1) E))
    (define pict0
     (vc-append (blank 1 40)
                layer2
                (blank 1 20)
                layer2a
                (blank 1 20)
                layer1b 
                (blank 1 30)
                layer1a
                (blank 1 60)
                layer0
                (blank 1 50)
                (if show-synth? layerS (ghost layerS))))
    (define pict1
      (foldl (lambda (pair pict)
               (pin-arrow-line 10 pict
                               (car pair) ct-find
                               (cdr pair) cb-find))
             pict0
             (list (cons A B)
                   (cons A C)
                   (cons C D)
                   (cons C CE)
                   (cons CE E))))
    (define pict2
      (foldl (lambda (pair pict)
               (pin-arrow-line 10 pict
                               (car pair) ct-find
                               (cdr pair) cb-find
                               2 "purple"))
             pict1
             (case lines
               ((d) (list (cons S D)))
               ((de) (list (cons S D)
                           (cons S E)))
               (else null))))
    (list pict2))

  
  
  
  (define (dbox label e1 e2)
    (values #;vl-append 
            #;(tiny (t label))
     (frame (inset (vl-append (ht-append (blank 20 1) (blank 10 1) e1)
                              (ht-append (arrow 20 (/ pi -2))
                                         (blank 10 1)
                                         e2))
                   5))))
  (define (sbox label e1 e2)
    (values #;vl-append
            #;(tiny (t label))
     (dash-frame (inset (vl-append (ht-append (blank 20 1) (blank 10 1) e1)
                                   (ht-append (cc-superimpose
                                               (blank 20)
                                               (scale (arrow 10 0) 2 1))
                                              (blank 10 1)
                                              e2))
                        5))))

  
  
  (define (hiding-example selection mode show-synth? lines)
    (define (sel sym pict)
      (if (eq? sym selection)
          (pin-under pict
                     -15
                     -15
                     (colorize (rectangle (+ 30 (pict-width pict))
                                          (+ 30 (pict-height pict)))
                               (case mode
                                 ((hide) "blue")
                                 ((seek) "red"))))
          pict))
    
    (define xA (dbox "macro"
                     (small (code* (define (check n)
                                     (my-or A B))))
                     (small (code* (define-values (check)
                                     (lambda (n) (let-values ---)))))))
    (define xB (sbox "transform"
                     (small (code* (define (check n)
                                     (my-or A B))))
                     (small (code* (define-values (check)
                                     (lambda (n) (my-or A B)))))))
    (define xC (dbox "define-values"
                     (small (code* (define-values (check)
                                     (lambda (n) (my-or A B)))))
                     (small (code* (define-values (check)
                                     (lambda (n)
                                       (let-values ---)))))))
    (define xD (dbox "lambda"
                     (small (code* (lambda (n) (my-or A B))))
                     (small (code* (lambda (n) (let-values ---))))))
    (define xE (dbox "macro"
                     (small (code* (my-or A B)))
                     (small (code* (let-values ---)))))
    (define xF (sbox "transform"
                     (small (code* (my-or A B)))
                     (small (code* (let ([t A]) (my-or B))))))
    (define xG (dbox "macro"
                     (small (code* (let ([t A])
                                     (if t t (my-or B)))))
                     (small (code* (let-values ---)))))
    (define S (colorize (dbox "synth"
                              (small (code* (define (check n)
                                              (my-or A B))))
                              (small (code* ???)))
                        "purple"))
    (define A (sel 'a xA))
    (define B (sel 'b xB))
    (define C (sel 'c xC))
    (define D (sel 'd xD))
    (define E (sel 'e xE))
    (define F (sel 'f xF))
    (define G (sel 'g xG))
    (define H (t "..."))

    (define layer0 (hc-append A (blank 50) (if show-synth? S (ghost S))))
    (define layer1 (hc-append B (blank 50 1) C))
    (define layer2 (hc-append (blank 360 1) D))
    (define layer3 (hc-append (blank 360 1) E))
    (define layer4 (hc-append F (blank 100 1) G))
    (define layer5 (hc-append (blank 440 1) H))
    (define pict0
      (vc-append layer5
                 (blank 1 20)
                 layer4
                 (blank 1 30)
                 layer3
                 (blank 1 30)
                 layer2
                 (blank 1 30)
                 layer1
                 (blank 1 60)
                 layer0))
    (define pict1
      (foldl (lambda (pair pict)
               (pin-arrow-line 10 pict
                               (car pair) ct-find
                               (cdr pair) cb-find))
             pict0
             (list (cons A B)
                   (cons A C)
                   (cons C D)
                   (cons D E)
                   (cons E F)
                   (cons E G))))
    (define pict2
      (foldl (lambda (pair pict)
               (define p1
                 (pin-arrow-line 10 pict
                                 (car pair) ct-find
                                 (cadr pair) cb-find
                                 2 "purple"))
               (define p2
                 (pin-over p1
                           (cadr pair)
                           cb-find
                           (caddr pair)))
               p2)
             pict1
             (case lines
               ((e) (list (list S E (colorize (small (bt "   (- - [])")) "purple"))))
               (else null))))
    (list pict2))

  ;; MACRO EXPANDER PSEUDOCODE
  
  (define (pseudo-expander/old instr?)
    (pseudocode "expand-term(term, env, phase) ="
                (and instr?
                     (colorize (pcode "  emit-event(\"visit\", term)") "red"))
                "  case term of "
                "    (kw . _)"
                "      where lookup(resolve(kw), env, phase) = (Primitive, expander)"
                (and (not instr?) "      => let term2 = expander(term, env, phase)")
                (and instr?
                     (colorize (pcode "      => emit-event(\"enter-primitive\", term)") "red"))
                (and instr?
                     (pcode "         let term2 = expander(term, env, phase)"))
                (and instr?
                     (colorize (pcode "         emit-event(\"exit-primitive\", term2)") "red"))
                (and instr?
                     (colorize (pcode "         emit-event(\"return\", term2)") "red"))
                "         return term2"
                "    ..."
                ""
                "expand-prim-lambda(term, env, phase) ="
                (and instr?
                     (colorize (pcode "  emit-event(\"primitive-lambda\")") "red"))
                "  case term of "
                "    (kw formals body)"
                "      where formals is a list of identifiers"
                "      => let formals2 = freshnames(formals)"
                "         let env2 = extend-env(env, formals2, Variable, phase)"
                "         let body2 = rename(body, formals, formals2)"
                (and instr?
                     (colorize (pcode "         emit-event(\"renames\", formals2, body2)") "red"))
                "         let body3 = expand-term(body2, env2, phase)"
                "         return (kw formals2 body3)"
                "    else => raise syntax error"))
  #;  
  (define (code:expand-term instr?)
    (code* 
     (define (expand-term term env phase)
       #,@(if instr? (list (colorize (code* (emit-event "visit" term)) "red")) null)
       (define kw (get-keyword term))
       (define meaning (lookup kw env phase))
       (cond [(primitive? meaning)
              #,@(if instr? (list (colorize (code* (emit-event "enter-primitive" term)) "red")) null)
              (define term2
                ((primitive-expander meaning) term env phase))
              #,@(if instr? (list (colorize (code* (emit-event "exit-primitive" term2)) "red")) null)
              #,@(if instr? (list (colorize (code* (emit-event "return" term2)) "red")) null)
              term2]
             [(macro? meaning)
              #,@(if instr? (list (colorize (code* (emit-event "enter-macro" term)) "red")) null)
              (define M (new-mark))
              (define term2 (mark M term))
              #,@(if instr? (list (colorize (code* (emit-event "macro-pre" term2)) "red")) null)
              (define term3
                ((macro-transformer meaning) term2))
              #,@(if instr? (list (colorize (code* (emit-event "macro-post" term3)) "red")) null)
              (define term4 (mark M term3))
              #,@(if instr? (list (colorize (code* (emit-event "exit-macro" term4)) "red")) null)
              (expand-term term4 env phase)]
             ...))))
  (define (code:expand-term0)
    (code* 
     (define (expand-term term env phase)
       (define kw (get-keyword term))
       (define meaning (lookup kw env phase))
       (cond [(primitive? meaning)
              (define term2
                ((primitive-expander meaning) term env phase))
              term2]
             [(macro? meaning)
              (define M (new-mark))
              (define term2 (mark M term))
              (define term3
                ((macro-transformer meaning) term2))
              (define term4 (mark M term3))
              (expand-term term4 env phase)]
             ...))))
  (define (code:expand-term1)
    (code* 
     (define (expand-term term env phase)
       #,(colorize (code* (emit-event "visit" term)) "red")
       (define kw (get-keyword term))
       (define meaning (lookup kw env phase))
       (cond [(primitive? meaning)
              #,(colorize (code* (emit-event "enter-prim" term)) "red")
              (define term2
                ((primitive-expander meaning) term env phase))
              #,(colorize (code* (emit-event "exit-prim" term2)) "red")
              #,(colorize (code* (emit-event "return" term2)) "red")
              term2]
             [(macro? meaning)
              #,(colorize (code* (emit-event "enter-macro" term)) "red")
              (define M (new-mark))
              (define term2 (mark M term))
              #,(colorize (code* (emit-event "macro-pre" term2)) "red")
              (define term3
                ((macro-transformer meaning) term2))
              #,(colorize (code* (emit-event "macro-post" term3)) "red")
              (define term4 (mark M term3))
              #,(colorize (code* (emit-event "exit-macro" term4)) "red")
              (expand-term term4 env phase)]
             ...))))

  #;
  (define (code:expand-prim-lambda instr?)
    (code*
     (define (expand-prim-lambda term env phase)
       #,@(if instr? (list (colorize (code* (emit-event "prim-lambda")) "red")) null)
       (syntax-case term ()
         [(kw formals body)
          (begin
            (define formals2 (new-formals formals))
            (define body2 (subst #'body formals formals2))
            #,@(if instr? (list (colorize (code* (emit-event "rename" formals2 body2)) "red")) null)
            (define env2 (extend env formals2 'variable))
            (define term3 (expand-term term2 env2 phase))
            `(kw ,formals2 ,body3))]
         [else
          (syntax-error term)]))))
  (define (code:expand-prim-lambda0)
    (code*
     (define (expand-prim-lambda term env phase)
       (syntax-case term ()
         [(kw formals body)
          (check-formals #'formals)
          (begin
            (define formals2 (new-formals #'formals))
            (define body2 (subst #'body #'formals formals2))
            (define env2 (extend env formals2 'variable))
            (define term3 (expand-term term2 env2 phase))
            `(kw ,formals2 ,body3))]
         [else
          (syntax-error term)]))))
  (define (code:expand-prim-lambda1)
    (code*
     (define (expand-prim-lambda term env phase)
       #,(colorize (code* (emit-event "prim-lambda")) "red")
       (syntax-case term ()
         [(kw formals body)
          (check-formals #'formals)
          (begin
            (define formals2 (new-formals #'formals))
            (define body2 (subst #'body #'formals formals2))
            #,(colorize (code* (emit-event "rename" formals2 body2)) "red")
            (define env2 (extend env formals2 'variable))
            (define term3 (expand-term term2 env2 phase))
            `(kw ,formals2 ,body3))]
         [else
          (syntax-error term)]))))

  (define (downexpand)
    (arrow 20 (* 3/2 pi)))
  
  (define (downexpand*)
    (vc-append 5
               (filled-rectangle 10 20) 
               (filled-rectangle 10 20)
               (downexpand)))

  (define (titleshot name file)
    (cc-superimpose (color-frame (if (file-exists? file)
                                     (bitmap file)
                                     (highlight (blank 400)))
                                 "black")
                    (highlight (bt name) "white")))
  
  (define (stack picts delta)
    (define (stack* picts delta)
      (let loop ([base (blank)] [picts picts] [offset 0])
        (if (null? picts)
            null
            (let ([next (lt-superimpose base (inset (car picts) offset offset 0 0))])
              (cons next (loop next (cdr picts) (+ offset delta)))))))
    (let* ([ps (stack* picts delta)]
           [g (ghost (apply lt-superimpose ps))])
      (map (lambda (p) (lt-superimpose p g)) ps)))
  
  (define-syntax labl
    (syntax-rules ()
      [(labl c str)
       (ht-append (frame (ltl-superimpose (blank 450 0) (code c)))
                  (colorize (t (string-append "  " str)) "red"))]))
  
  
  
  
  )
