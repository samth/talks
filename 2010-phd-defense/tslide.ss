#lang typed/racket/base/no-check

(require (planet cce/scheme:6/slideshow) racket/class "t-slideshow.ss" slideshow racket/gui "beamer.ss" (only-in "lib.ss" title-text-size large-text-size))
(provide tslide subtitle-pict color%)

(: subtitle-pict : (String -> Pict))
(define (subtitle-pict s)
  (text s (current-title-font) large-text-size))

(: tslide (String (Listof (U Pict String)) * -> Void))
(define (tslide t . subs)
  (: row ((Listof (U Pict String)) -> Pict))
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
  (parameterize ([current-background-pict (bitmap plt-title-background-path)])
    (slide #:layout 'center
           (if (null? subs)
               (text t (current-title-font) title-text-size)
               (apply vc-append 0 
                      (append 
                       (map ghost (map row subs))
                       (list (text t (current-title-font) title-text-size))
                       (map row subs)))))))

