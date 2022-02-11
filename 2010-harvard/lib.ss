#lang slideshow

;; from Jay McCarthy

(require mred
         slideshow/code
         scheme/runtime-path
         (only-in "beamer.ss" current-title-font plt-title-background-path current-background-pict
                  blockf current-block-background-color block current-block-maker))
(provide title
         icon-assembler title-text-size large-text-size)
(provide (all-defined-out))

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
(define title-color "blue")


(define (title title-strs subtitle-strs authors/institutions [location #f])
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
      (vc-append
      (vr-append
       (vl-append
        (apply vl-append (map (lambda (x) (text x (current-title-font) title-text-size))
                              title-strs))
        (apply vl-append (blank 0)
               (map (lambda (x) (text x `(italic . ,(current-title-font)) large-text-size))
                    subtitle-strs)))
       (blank 0 50)
       
       (apply vr-append 
              (map (lambda (x)
                     (vr-append (colorize (text (car x) 'decorative small-text-size) author-color)
                                (colorize (text (cadr x) 'decorative small-text-size) institution-color)
                                (blank 0 10)))
                   authors/institutions)))
       (blank 0 50)
       (if location
           (colorize (text location 'decorative small-text-size) author-color)
           (blank 0 10)))))))

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

(define current-sizeof (make-parameter #f))

(define (mod/lang lang 
                  #:name [name #f]
                  #:space [space #t]
                  #:name-frame [framer frame]
                  #:sizeof [sz (or (current-sizeof) 
                                   (code #,(string-append "#lang " lang (or name ""))))] 
                  #:width [width (pict-width sz)]
                  #:height [height (pict-height sz)]
                  . body)
  (define box
    (vl-append
     ((current-code-tt) (string-append "#lang " lang))
     (if space (code ||) (blank 0))
     (lt-superimpose
      (ghost sz)
      (ghost (blank width height))
      (apply
       vl-append 
       body))))
  (if name
      (rt-superimpose (framer name) box)
      box))

(define-syntax-rule (nomod . args)
  (transparent-block (code . args)))

(define ((name-back c) name)
  (define p (colorize ((current-code-tt) (string-append " " name " ")) (if (equal? c "blue") "white" "white")))
  (cc-superimpose (cellophane (colorize (filled-rectangle (pict-width p) (pict-height p)) c) 1) p))

(define-syntax-rule (tmod . args)
  (parameterize ([current-block-background-color "Bisque"]
                 [current-outline-color "blue"])
    (transparent-block (mod/lang "typed-scheme   " #:name-frame (name-back "blue") . args))))

(define-syntax-rule (smod . args)
  (parameterize ([current-block-background-color "Azure"]
                 [current-outline-color "red"])    
    (transparent-block (mod/lang "      scheme   " #:name-frame (name-back "red") . args))))

(define-syntax-rule (red-block . args)
  (parameterize ([current-block-background-color "Tomato"])
    (block . args)))

(define current-outline-color (make-parameter "black"))

(define-syntax-rule (transparent-block . e)
  (parameterize ([current-block-maker
                  (lambda (header picts)
                    (define body (inset (apply vl-append picts) 15))
                    (define protoback
                      (colorize (linewidth 3 (rounded-rectangle (pict-width body) (pict-height body) 25))
                                (current-outline-color)))
                    (define shadow
                      (inset (colorize protoback "lightgray") 5 5 0 0))
                    (when header
                      (error 'block-maker "header not supported yet"))
                    (lt-superimpose #;shadow protoback body))])
    (block (vc-append . e))))

(define (codett txt)
  (text txt (current-code-font) (current-font-size)))

(define (subpara . args)
  (hc-append (blank (* 2 gap-size))
             (apply para #:width (- (current-para-width) (* 2 gap-size))
                    args)))

(define do-start? (make-parameter #t))
(define (start) 
  (when (do-start?)
    (start-at-recent-slide)))
(provide red-code)
(define-syntax-rule (red-code . e)
  (parameterize ([code-colorize-enabled #f])
    (colorize (code . e) "red")))

(define (error-t txt)
  (colorize
   (text txt 
         (cons 'italic (current-code-font))
         (current-font-size))
   "red"))

(define-syntax-rule (stage-proc [names ...] . body)
  (lambda (name1) staged))

(define ((move-find-left finder diff) . args)
  (define-values (v1 v2) (apply finder args))
  (values (- v1 diff) v2))