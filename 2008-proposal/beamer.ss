
(module beamer mzscheme
  (require (lib "slide.ss" "slideshow")
           (lib "code.ss" "slideshow")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "etc.ss")
           (only (lib "main.ss" "scheme") define-syntax-rule keyword-apply))
  
  (provide (all-defined))
  
  (define ((mk-not-installed f) . _)
    (error f "no handler installed"))

  ;; Title page info

  (define title (make-parameter #f))
  (define short-title (make-parameter #f))
  (define subtitle (make-parameter #f))

  (define-struct authinfo (name inst email))
  (define authors (make-parameter null))

  (define current-title-slide-maker
    (make-parameter (mk-not-installed 'current-title-slide-maker)))
  (define basic-assembler (current-slide-assembler))

  ;; author : string string/#f string/#f -> void
  ;; Add an author to the authors list
  (define (author name inst email)
    (authors (append (authors)
                     (list (make-authinfo name inst email)))))

  (define (title-slide)
    ((current-title-slide-maker)))
#;
  ;; Block environments
  (define-syntax define-syntax-rule
    (syntax-rules ()
      [(define-syntax-rule (macro . pattern) template)
       (define-syntax macro
         (syntax-rules ()
           [(_ . pattern) template]))]))

  (define local-width (make-parameter client-w))

  (define-syntax-rule (narrow-by n . body)
    (parameterize ((local-width (- (local-width) n)))
      . body))
  (define-syntax-rule (narrow-by-factor n . body)
    (parameterize ((local-width (floor (* (local-width) n))))
      . body))
  (define-syntax-rule (narrow-to n . body)
    (parameterize ((local-width n))
      . body))

  (define-syntax-rule (deflocal local-x x extra ...)
    (define (local-x . ps) (apply x extra ... (local-width) ps)))
  (deflocal local-para para)
  (deflocal local-para* para*)
  (deflocal local-para/c para/c)
  (deflocal local-para*/c para*/c)
  (deflocal local-para/r para/r)
  (deflocal local-para*/r para*/r)
  (deflocal local-item item/bullet (current-bullet))
  (deflocal local-item* item*/bullet (current-bullet))
  
  ;; Need to get indenting right
  (define (local-subitem . ps) (keyword-apply subitem/kw (list '#:bullet '#:width) (list (current-subbullet) (local-width)) ps))
  ;;(deflocal local-subitem* item*/bullet (current-subbullet))

  (define current-bullet (make-parameter bullet))
  (define current-subbullet (make-parameter o-bullet))
  
  ;; Local environments

  (define-syntax-rule (block p ...)
    (narrow-by (current-block-margin)
               (block-pict #f p ...)))
  (define-syntax-rule (block/header h p ...)
    (narrow-by (current-block-margin)
               (block-pict h p ...)))
  (define current-block-margin (make-parameter 0))
  (define current-block-maker
    (make-parameter (mk-not-installed 'current-block-maker)))
  (define (block-pict header . picts)
    ((current-block-maker) header picts))
  
  (define current-block-background-color
    (make-parameter "lightblue"))

  
  (define (blockf . p)
    (narrow-by (current-block-margin)
               (apply block-pict #f p)))
  
  (define (block* . p)
    (narrow-by (current-block-margin)
               (inset (apply vl-append p) 15)))
  
  (current-block-maker
   (lambda (header picts)
     (define color (current-block-background-color))
     (define body (inset (apply vl-append picts) 15))
     (define protoback
       (filled-rounded-rectangle (pict-width body) (pict-height body) -20))
     (define back (colorize protoback color))
     (define shadow
       (inset (colorize protoback "lightgray") 5 5 0 0))
     (when header
       (error 'block-maker "header not supported yet"))
     (lt-superimpose shadow back body)))
  (current-block-margin 30)

  ;; theorem, lemma, proof, corollary, example, definition
  
  ;; Default

  (define current-background-pict
    (make-parameter (blank 1024 768)))
  
  (define current-title-background-pict
    (make-parameter (blank 1024 1)))

  (current-slide-assembler
   (let ([assembler (current-slide-assembler)])
     (lambda (a b c)
       (define title
         (if a (make-title-pict a) #f))
       (define main
         (if title
             (vl-append b title c)
             c))
       (refocus (lt-superimpose (current-background-pict)
                                (inset main (get-margin)))
                main))))

  (current-background-pict
   (scale
    (bitmap (build-path (this-expression-source-directory) "art" "vgrad2.png"))
    1024 1))

  (current-title-background-pict
   (bitmap
    (build-path (this-expression-source-directory)
                "art"
                "gradient-blue-black.png")))

  (define (make-title-pict s)
    (let* ([ucp (text s (current-main-font) title-size)]
           [cp (colorize ucp "white")]
           [p (inset cp (get-margin))]
           [vscale (/ (pict-height p) (pict-height (current-title-background-pict)))])
      (inset (lt-superimpose (scale (current-title-background-pict) 1 vscale) p)
             (- (get-margin)))))

  (current-main-font " Verdana")
  (current-code-font " Monospace")
  ;;(current-code-font " Andale Mono")
  ;;(current-code-font `(bold . " Andale Mono"))

  (current-line-sep (* 2 line-sep))
  (current-title-color (make-object color% 0 0 150))
  
  ;; ----------------
  
  (current-title-slide-maker
   (lambda ()
     (slide/center
      (parameterize ((current-block-background-color "blue"))
        (block 
         (inset (colorize (vc-append (text (string-append (title) ":") (current-main-font) title-size)
                                     (text (subtitle) (current-main-font) (- title-size 6))) "white")
                20 20)))
      (apply vc-append line-sep 
             (map t (map authinfo-name (authors)))))))
   )
