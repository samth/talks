#lang slideshow

(require scheme/class scheme/gui)


(define (scale2 p x-factor y-factor)
  (let ([new
         (dc
          (lambda (dc x y)
            (let-values ([(xs ys) (send dc get-scale)])
              (send dc set-scale (* xs x-factor) (* ys y-factor))
              (draw-pict p dc 0 0)))
          (* (pict-width p) x-factor)
          (* (pict-height p) y-factor)
          (* (pict-ascent p) y-factor)
          (* (pict-descent p) y-factor))])    
    (make-pict (pict-draw new)
               (pict-width new)
               (pict-height new)
               (pict-ascent new)
               (pict-descent new)
               null
               #f #f)))

(define png (make-object bitmap% "rhino50.jpg"))
(define png/mask (make-object bitmap% "rhino-alpha.png" 'png/mask))

(scale2 (bitmap png) .5 .5)
(scale2 (bitmap png/mask) .5 .5)

(send png set-loaded-mask (send png/mask get-loaded-mask))

(scale2 (bitmap png) .5 .5)
(scale2 (bitmap png/mask) .5 .5)
(define w 100)
(define h 100)

(define b (make-bytes (* w h 4)))
(send (send png/mask get-loaded-mask) get-argb-pixels 0 0 w h b #t)
(bytes->list b)

