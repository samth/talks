#lang typed/racket/no-check

(require unstable/gui/slideshow scheme/class "t-slideshow.ss" slideshow 
         (only-in scheme/gui color%)
         unstable/gui/ppict unstable/gui/pslide
         "helper.rkt"
         (except-in "beamer.ss" title) "lib.ss")
(provide pslide/title tslide subtitle-pict tslide*)

(define-syntax-rule (pslide/title e . rest)
  (pslide #:go (coord 0.05 0.05 'lc)
        (t/quat e size2)
        . rest))

(: subtitle-pict : (String -> Pict))
(define (subtitle-pict s)
  (text s (current-title-font) large-text-size))

(: tslide : ((U String Pict) (Listof (U Pict String)) * -> Void))
(define (tslide t . subs)
  (: row : ((Listof (U Pict String)) -> Pict))
  (define (row sub)
    (let* ([picts 
            (map (lambda: ([e : (U Pict String)])
                   (if (pict? e) 
                       e
                       (colorize (subtitle-pict e) 
                                 (make-object color% 70 70 70))))
                 sub)]
           [gpicts (map ghost picts)]) 
      (apply hbl-append 50 
             (map (lambda: ([e : Pict]) (apply cc-superimpose e gpicts)) picts))))
  (parameterize ([current-background-pict (bitmap plt-background-path)])
    (slide #:layout 'center
           (if (null? subs)
               (if (string? t) (text t (current-title-font) title-text-size) t)
               (apply vc-append 0 
                      (append 
                       (map ghost (map row subs))
                       (list (text t (current-title-font) title-text-size))
                       (map row subs)))))))


(define (tslide* s [papers #f])
  (pslide 
   #:go (coord 0.5 0.5)
   (cond [(pict? s) s]
         [else (t/section s)])
   #:go (coord 0.0 0.95 'lc)
   (cond [(pict? papers) papers]
         [(list? papers) (apply vl-append (for/list ([p papers]) (t/cant p 24)))]
         [papers (t/cant papers 24)]
         [else (blank)])))
