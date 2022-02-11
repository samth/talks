(module setup (lib "slideshow.ss" "slideshow")
  (require (lib "class.ss")
           (lib "etc.ss")
           (lib "mred.ss" "mred")
           (lib "code.ss" "slideshow"))
  
  (provide (all-defined))
  
  ;; commented out for printing 
  (define letterpress (inset (bitmap "letterpress.png")
                             (- (get-margin))))
  
  #;(define letterpress
    (and (not printing?)
	 (inset (bitmap (build-path 
			 (this-expression-source-directory)
			 "letterpress.png"))
		(- (get-margin)))))
  
  
  #;(when letterpress
    (let ([assembler (current-slide-assembler)])
      (current-slide-assembler
       (lambda (a b c)
         (let ([p (assembler a b c)])
           (lt-superimpose letterpress
                           p))))))
  
  (define (fancy-title-separator w h)
    (let ([p (make-object dc-path%)])
      (send p move-to 0 4.5)
      (send p curve-to 4.5 5 5 5 5 0)
      (send p curve-to 5 5 5.5 5 10 4.5)
      (send p line-to 10 5.5)
      (send p curve-to 5.5 5 5 5 5 10)
      (send p curve-to 5 5 4.5 5 0 5.5)
      (send p close)
      (send p scale (/ w 10) (/ h 10))
      (dc (lambda (dc x y)
            (send dc draw-path p x y))
          w h)))      
  
  (define hilite-new-color "forestgreen")
  (define hilite-copy-color "red")
  
  (define big-line-sep (* 3 line-sep))
  (define (big-blank) (blank 0 gap-size))
  
  (define (2/3-para . l)
    (apply para (* 2/3 client-w) l))
  
  (define (rm s) (text s 'roman (current-font-size)))
  
  (define (dt s) (bt s))
  
  (define (b-item . l)
    (apply item (* 4/5 client-w) l))
  
  (define orig-main-font (current-main-font))
  
  (current-main-font "Vera Sans")
  (current-line-sep (* 2 line-sep))
  #;(current-title-color (make-object color% 0 0 150))
  
  (current-keyword-list (list* "define-datatype"
                               "cases"
                               (current-keyword-list))))
