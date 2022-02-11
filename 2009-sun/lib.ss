#lang slideshow

;; from Jay McCarthy

(require mred
         slideshow/code
         scheme/runtime-path
         (only-in "beamer.ss" current-title-font plt-title-background-path current-background-pict
                  blockf current-block-background-color block current-block-maker))
(provide title
         icon-assembler title-text-size large-text-size)


(define purple-color (make-object color% 150 0 150))
(define title-text-size 60)
(define large-text-size 48)
(define normal-text-size 36)
(define small-text-size 24)
(define section-point-size 48)
(define big-bullet-size normal-text-size)
(define big-bullet-color "darkblue") 
(define small-bullet-size 30)
(define small-bullet-color purple-color)
(define other-small-bullet-color "blue")
(define bullet-inset 50)

(define section-title-color "black")
(define institution-color "black")
(define author-color "black")
(define title-color section-title-color)



(define (title title-strs subtitle-strs authors/institutions)
  (parameterize ([current-slide-assembler
                  (lambda (title sep content)
                    (inset 
                     content
                     (- margin)
                     (- margin)
                     0
                     0))])
    (slide
     (cc-superimpose
      (bitmap plt-title-background-path)
      (vr-append
       (vl-append
        (apply vl-append (map (lambda (x) (text x (current-title-font) title-text-size))
                              title-strs))
        (apply vl-append (map (lambda (x) (text x `(italic . ,(current-title-font)) large-text-size))
                              subtitle-strs)))
       (blank 0 50)
       
       (apply vr-append 
              (map (lambda (x)
                     (vr-append (colorize (text (car x) 'decorative small-text-size) author-color)
                                (colorize (text (cadr x) 'decorative small-text-size) institution-color)
                                (blank 0 10)))
                   authors/institutions)))))))

(define-runtime-path logo-path "PLTnolarval-small.jpg")
(define top-right-logo (bitmap logo-path))

(define (icon-assembler
         #:logo-scale [logo-scale 1])
  (let ([orig (current-slide-assembler)])
    (current-slide-assembler
     (lambda (title sep content)
       (rt-superimpose 
        (size-in-pixels (scale top-right-logo logo-scale))
        (cc-superimpose (orig title sep content) 
                        full-page))))))

;; lib code by me
(provide tslide mk-itemize smod tmod red-block current-block-background-color nomod transparent-block
         (all-defined-out))

(define (tslide t)
  (parameterize ([current-background-pict (bitmap plt-title-background-path)])
    (slide #:layout 'center
           (text t (current-title-font) title-text-size))))
  
(define (mk-itemize texts pics #:para [para para] #:block [block blockf]
                    #:overlay [overlay lt-superimpose])
  (define g-pics (map ghost (apply append (map (lambda (e) (if (list? e) e (list e))) pics))))
  (let loop ([seen-ts '()] [ts (map para texts)] [ps pics])
    (define (rest v)
      (append
       (map (lambda (p)
              (append (reverse seen-ts)
                      (list (car ts)) 
                      (map ghost (cdr ts))
                      (list (block (apply overlay p g-pics)))))
            v)
       (loop (cons (car ts) seen-ts)
             (cdr ts)
             (cdr ps))))
    (cond
      [(null? ts) null]
      [(list? (car ps)) (rest (car ps))]
      [else (rest (list (car ps)))])))

(define (mod/lang lang #:name [name #f] #:space [space #t]
                  #:height [height 10]
                  #:sizeof [sz (code #,(string-append "#lang " lang (or name "")))] . body)
  (define box
    (lt-superimpose
     (ghost (blank (pict-width sz) height))
     (apply
      vl-append 
      ((current-code-tt) (string-append "#lang " lang))
      (if space (cons (tt "") body) body))))
  (if name
      (rt-superimpose (frame ((current-code-tt) (string-append " " name " "))) box)
      box))

(define-syntax-rule (nomod . args)
  (transparent-block (code . args)))

(define-syntax-rule (tmod . args)
  (parameterize ([current-block-background-color "Bisque"]
                 [current-outline-color "blue"])
    (transparent-block (mod/lang "typed-scheme   " . args))))

(define-syntax-rule (red-block . args)
  (parameterize ([current-block-background-color "Tomato"])
    (block . args)))

(define current-outline-color (make-parameter "black"))

(define-syntax-rule (transparent-block . e)
  (parameterize ([current-block-maker
                  (lambda (header picts)
                    (define body (inset (apply vl-append picts) 15))
                    (define protoback
                      (colorize (linewidth 30 (rounded-rectangle (pict-width body) (pict-height body) 25))
                                (current-outline-color)))
                    (define shadow
                      (inset (colorize protoback "lightgray") 5 5 0 0))
                    (when header
                      (error 'block-maker "header not supported yet"))
                    (lt-superimpose #;shadow protoback body))])
    (block (vc-append . e))))

(define-syntax-rule (smod . args)
  (parameterize ([current-block-background-color "Azure"]
                 [current-outline-color "red"])    
    (transparent-block (mod/lang "scheme         " . args))))

(define (codett txt)
  (text txt (current-code-font) (current-font-size)))

(define (subpara . args)
  (hc-append (blank (* 2 gap-size))
             (apply para #:width (- (current-para-width) (* 2 gap-size))
                    args)))
(define start start-at-recent-slide)
(provide red-code)
(define-syntax-rule (red-code . e)
  (parameterize ([code-colorize-enabled #f])
    (colorize (code . e) "red")))
