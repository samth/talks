#lang slideshow

(require slideshow/step slideshow/code slideshow/face 
         unstable/gui/ppict unstable/gui/pslide
         (only-in slideshow/slide title-size)
          "config.ss"
         (except-in "beamer.ss" title) "lib.ss" racket/gui  "thanks.ss" 
         "tslide.ss" lang-slide "contracts.rkt"
         "ts-intro.rkt" "stages.rkt"
         "helper.rkt"
         racket/runtime-path (except-in mzlib/etc identity) unstable/gui/slideshow)

(require ppict-slide-grid "table.rkt")
;(set-grid-base-pict!)

(pslide
 #:go (coord .5 .5 'cc) 
 (bitmap plt-background-path)
   #:go (coord 0.05 .85 'lc)
   (t/cant "Sam Tobin-Hochstadt" size2)
   #:go (coord 0.05 .95 'lc)
   (t/quat "Indiana University" size2)
   #:go (coord 0.95 .95 'rc)
   (t/quat "PEPM 2016" size2)
 #:go (coord 0.05 .25 'lt)
 (t/cant "Pycket" (+ 10 size1))
 #:go (coord 0.95 .30 'rt)
 (t/quat "A tracing JIT" size2)
 (t/quat "For a functional language" size2))

;(set-page-numbers-visible! #t)
(do-start? #f)

(chart)

(code-colorize-enabled #t)
;(slide (langs-pict #t))

(pslide/staged/title
 (zero one two  four three)
 "A simple program"
 #:go (coord .5 .4 'cc)
 (pict-case stage-name #:combine cc-superimpose
            [(zero one)(scale (code (define (dot u v)
                                      (for/sum ([x u]
                                                [y v])
                                        (* x y))))
                       1.2)]
            [(two) (scale (code (define (dot u v)
                                  (for/sum ([x (in-vector u)]
                                            [y (in-vector v)])
                                    (fl* x y))))
                          1.2)]
            [(three) (scale (code (define/contract (dot u v)
                                    ((vectorof flonum?) (vectorof flonum?)
                                     . -> . flonum?)
                                    (for/sum ([x (in-vector u)]
                                              [y (in-vector v)])
                                      (fl* x y))))
                            1.2)]
            [(four) 
             (scale (code (define (dot v1 v2)
                            (define len (flvector-length v1))
                            (unless (= len (flvector-length v2))
                              (error 'fail))
                            (let loop ([n 0] [sum 0.0])
                              (if (unsafe-fx= len n) sum
                                  (loop (unsafe-fx+ n 1)
                                        (unsafe-fl+
                                         sum (unsafe-fl*
                                              (unsafe-flvector-ref v1 n)
                                              (unsafe-flvector-ref v1 n))))))))
                    .8)])
 #:go (coord .5 .8 'cc)
 (pict-case stage-name #:combine rc-superimpose
            [(one) (t/cant "506 ms (size 10000000)" size2)]
            [(two) (t/cant "39 ms (size 10000000)" size2)]
            [(three) (t/cant "933 ms (size 10000000)" size2)]
            [(four) (t/cant "29 ms (size 10000000)" size2)]))

(pslide/title
 "Success?"
 #:go (coord .1 .3 'lc)             
 (t/quat "✓ Fast code" size2)
 #:go (coord .1 .5 'lc)             
 (t/quat "✓ Generic operations and contracts" size2)
 #:go (coord .1 .7 'lc)             
 (t/quat "✗ You can only pick one" size2))

;; (pslide/title "Why are contracts hard to optimize?"
;;               #:go (coord .5 .5)
;;               (code (contract (-> integer? integer?) (lambda (x) x))))

;; (pslide/title "Why are contracts hard to optimize?"
;;               #:go (coord .5 .5)
;;               (code (chaperone-procedure
;;                      (lambda (x) x)
;;                      (lambda (v) (unless (integer? v) (error 'blame)) v)
;;                      (lambda (v) (unless (integer? v) (error 'blame)) v))))

(pslide #:go (coord .5 .4 'cc)
        (colorize (vc-append
         (t/section "What do contracts and" #:color "black")
         (t/section "generic functions" #:color "black")
         (t/section "have in common?" #:color "black")) "black")
        200
        #:next
        (t/section "Indirection"))
         


(pslide #:go (coord .5 .6 'cc) (scale (bitmap "cake.jpg") 1.7))

;(tslide  "Enter Pycket")

(pslide #:go (coord .3 .3 'cc) (scale (bitmap "racket-logo.png") .6)
        #:go (coord .5 .3 'cc) (t/inc "+" size1)
        #:go (coord .7 .3 'cc) (scale (bitmap "pypy-logo.png") .8)
        #:go (coord .5 .62 'cc) (t/inc "=" size1)
        #:go (coord .5 .79 'cc) (scale (bitmap "pycket-logo.png") .25))

(pslide/staged/title
 (one two three four)
 "With added cake ..."
 #:go (coord .5 .4 'cc)
 (pict-case stage #:combine cc-superimpose
            [(3)(scale (code (define (dot u v)
                               (for/sum ([x u]
                                         [y v])
                                 (* x y))))
                       1.2)]
            [(2) (scale (code (define (dot u v)
                                (for/sum ([x (in-vector u)]
                                          [y (in-vector v)])
                                  (fl* x y))))
                        1.2)]
            [(4) (scale (code (define/contract (dot u v)
                                ((vectorof flonum?) (vectorof flonum?)
                                  . -> . flonum?)
                                (for/sum ([x (in-vector u)]
                                          [y (in-vector v)])
                                  (fl* x y))))
                        1.2)]
            [(1) (scale (code (define (dot v1 v2)
                                (define len (flvector-length v1))
                                (unless (= len (flvector-length v2))
                                  (error 'fail))
                                (let loop ([n 0] [sum 0.0])
                                  (if (unsafe-fx= len n) sum
                                      (loop (unsafe-fx+ n 1)
                                            (unsafe-fl+
                                             sum (unsafe-fl*
                                                  (unsafe-flvector-ref v1 n)
                                                  (unsafe-flvector-ref v1 n))))))))
                        .8)])
 #:go (coord .5 .8 'cc)
 (pict-case stage #:combine rc-superimpose
            [(3) (t/cant "12 ms (size 10000000)" size2)]
            [(2) (t/cant "11 ms (size 10000000)" size2)]
            [(4) (t/cant "17 ms (size 10000000)" size2)]
            [(1) (t/cant "8 ms (size 10000000)" size2)]))

(define (pic t b [scl 1])
  (pslide/title t #:go (coord .5 .5 'cc) (scale (bitmap (~a b ".png")) scl)))

(tslide "How does it work?")

(pslide/title "Tracing JIT"
              #:go (coord .1 .45 'lc)
              20
              (t/quat "1. Interpret Program" size2)
              (t/quat "2. Find hot loop" size2)
              (t/quat "3. Record operations for one iteration"  size2)
              (t/quat "4. Optimize" size2)
              (t/quat "5. Switch to new code" size2)
)

(define (go start end [clip? #t] #:title [title #f])
  (for ([i (in-range (sub1 start) end 1)])
    (if title
        (pslide #:go (coord .5 .5 'cc)
            (scale (page->pict (pdf-page "jsiek-slides.pdf" i)) 3)
            #:go (coord 0.5 0 'cc)
            ((if clip? values ghost) (filled-rectangle 1024 65 #:color "white" #:draw-border? #f))
            #:go (coord 0.05 0.05 'lc)
            (t/cant title size2))
        (pslide #:go (coord .5 .5 'cc)
                (scale (page->pict (pdf-page "jsiek-slides.pdf" i)) 3)
                #:go (coord .5 0 'cc)
                ((if clip? values ghost) (filled-rectangle 1024 65 #:color "white" #:draw-border? #f))))))

(go 8 9 #t #:title "Tracing JIT")


(pslide/title "Tracing JIT"
              #:go (coord .5 .52 'cc) (bitmap "jit.png")
              #:go (coord .9 .95 'rc) (t/quat "(Diagram from Antonio Cuni)" 16))

              

(pslide/title "Resulting Optimizations"
              #:go (coord .1 .45 'lc)
              20
              (t/quat "Inlining (happens for free)" size2)
              (t/quat "Constant propagation" size2)
              (t/quat "Allocation Removal" size2)
)

(pslide/title "Dot product Inner Loop"
              #:go (coord .5 .5 'cc)
              (apply vl-append 0
                     (map (lambda (s)
                            (if (list? s)
                                (t/quat (car s) 32)
                                (t/inc s 32)))
                   (list
                    "label(acc, idx1, idx2, len1, len2, arr1, arr2)"
                    (list "  check loop counters")
                    "guard(idx1 < len1)"
                    "guard(idx2 < len2)"
                    (list "  fetch elements")
                    "val1     = getarrayitem_gc(arr1, idx1)"
                    "val2     = getarrayitem_gc(arr2, idx2)"
                    (list "  computation")
                    "prod     = val1 * val2"
                    "acc_new  = acc + prod"
                    (list "  increment counters")
                    "idx1_new = idx1 + 1"
                    "idx2_new = idx2 + 1"
                    (list "  loop back")
                    "jump(acc_new, idx1_new, idx2_new, len1, len2, arr1, arr2)"))))


(define (t* l) (t/quat l size2))

(tslide "Meta-tracing: the magic part")

(pslide #:go (coord .5 .5 'cc #:compose vl-append)
        40
        (t/quat "We didn't write a JIT or an optimizer!" size2)
        #:next
        (t/quat "RPython creates a JIT from an interpreter" size2))

(go 11 11 #t #:title "Meta-tracing JIT")

(pic "CEK Machine" "cek")

(pslide/title "CEK Advantages"
              #:go (coord .1 .45 'lc)
              20
              (t* "Fast continuations")
              (t* "Tail recursion")
              (t* "Arbitrary size stack")
              #:next
              (colorize (t* "Allocation everywhere") "red")
              )

(pslide/title "From CEK to JIT"
              #:go (coord .1 .45 'lc)
              20
              (t* "1. Whole-program type inference")
              (t* "2. Translation to C")
              (t* "3. Adding JIT based on hints"))

(pic "Main Interpreter Loop" "main-loop")

(pslide/title "Other hints"
              #:go (coord .1 .35 'lc)
              20
              (t* "Immutable Data")
              (t* "Loop unrolling")
              (t* "Constant functions")
              (t* "Specialization"))

(tslide "A loop by any other name")

(define c (coord .5 .5 'cc))

(pslide/title "Detecting loops"
              #:go (coord .5 .5 'cc)
              (t/cant "Record back-edges in control flow graph" size2)
              ;#:go c
              (scale (bitmap "loop-counter.png") .55)
              50
              (t/cant "The default approach" size2)
              #:next
              #:go (coord .5 .5 'cc)
              (rotate (shadow-frame (t/cant "Function calls, not loops" 50)
                                    #:shadow-descent 5) (/ pi 6)))

(define-syntax-rule (mk-box c)
  (cc-superimpose
   (rectangle 120 40)
   (scale (code c) .5)))



(define loop-pict
  (let* ([one (mk-box (+ a b))]
         [two (mk-box (loop a b))]
         [three (mk-box (my-add a b))]
         [four (mk-box (+ a b))]
         [five (mk-box (my-add a b))]
         [base 
          (hc-append 80 one two three four five)]
         [base (pin-arrow-line 10 base one rc-find two lc-find)]
         [base (pin-arrow-line 10 base two rc-find three lc-find)]
         [base (pin-arrow-line 10 base three rc-find four lc-find)]
         [base (pin-arrow-line 10 base four rc-find five lc-find)]
         [base (pin-arrow-line 10 base five cb-find one cb-find
                               #:start-angle (- (/ pi 11))
                               #:end-angle (+ (/ pi 11)))]
         )
    base))

(define loop-pict2
  (let* ([one (mk-box (+ a b))]
         [two (mk-box (loop a b))]
         [three (mk-box (my-add a b))]
         [four (mk-box (+ a b))]
         [five (mk-box (my-add a b))]
         [base 
          (hc-append 80 one two three four five)]
         [base (pin-arrow-line 10 base one rc-find two lc-find)]
         [base (pin-arrow-line 10 base two rc-find three lc-find)]
         ;[base (pin-arrow-line 10 base three rc-find four lc-find)]
         ;[base (pin-arrow-line 10 base four rc-find five lc-find)]
         [base (pin-arrow-line 10 base three cb-find one cb-find
                               #:start-angle (- (/ pi 11))
                               #:end-angle (+ (/ pi 11)))]
         )
    base))

(pslide/title "Detecting loops"
              #:go (coord .5 .5 'cc)
              (t/cant "A loop is a repeated AST node" size2)
              #:next
              (code (define (my-add a b) (+ a b))
                    (define (loop a b)
                      (if (= a b) 1
                          (loop (my-add a b)
                                (my-add a b)))))
              20
              (t/cant "Trace from hot node back to itself" size2)
              20
              loop-pict)

(pslide/title "Detecting loops"
              #:go (coord .5 .5 'cc)
              (t/cant "A loop is a repeated AST node" size2)
              (code (define (my-add a b) (+ a b))
                    (define (loop a b)
                      (if (= a b) 1
                          (loop (my-add a b)
                                (my-add a b)))))
              20
              (t/cant "Trace from hot node back to itself" size2)
              20
              loop-pict2
              #:next
              #:go (coord .5 .5 'cc)
              (rotate (shadow-frame (t/cant "AST nodes lack context" 50)
                                    #:shadow-descent 5) (/ pi 6)))

(pslide/title "Detecting loops"

              #:go (coord .5 .5 'cc)
              (t/cant "Construct control flow graph dynamically" size2)
              ;#:go c
              (scale (bitmap "loop-diag.png") .55)
              50
              (t/cant "Combine with added context" size2)
              20
              loop-pict
)
                    

;(go 14 19 #t #:title "Detecting loops")

(tslide "Optimizations")

(pslide/title "Optimization in the interpreter"
              #:go (coord .1 .35 'lc)
              20
              (t* "A-normalization")
              (t* "Assignment conversion")
              (t* "Environment optimization")
              (t* "Data structure specialization")
)

(pic "Storage Strategies" "strategy" .75)

(pslide/title "Optimizations we don't do"
              #:go (coord .1 .45 'lc)
              40
              (t* "Closure conversion")
              (t* "Pointer tagging (64-bit integers!)")
              )

(tslide "How well does it work?")

(require slideshow pdf-read)

(define (pdf->slide file [i 0] #:scale [s 1] #:title [title #f])
  (if title
      (pslide #:go (coord .5 .5 'cc)
              (scale (page->pict (pdf-page file i)) s)
              #:go (coord 0.05 0.05 'lc)
              (t/quat title size2))
      (pslide #:go (coord .5 .5 'cc)
              (scale (page->pict (pdf-page file i)) s))))

(define (pdf->slide/bitmap file [i 0] #:scale [s 1] #:title [title #f])
      (pslide #:go (coord .5 .55 'cc)
              (scale (bitmap (page->bitmap (pdf-page file i))) s)
              #:go (coord 0.05 0.05 'lc)
              (if title (t/quat title size2) (blank 1))))


;(go 26 27 #t #:title "Benchmarks")
(pdf->slide/bitmap #:title "Scheme benchmarks"
                   #:scale .7
                   "larceny-big.pdf")
(pdf->slide/bitmap #:title "Shootout benchmarks"
                   #:scale .8
                   "shootout1-big.pdf")
(pdf->slide/bitmap #:title "Shootout benchmarks"
                   #:scale .8
                   "shootout2-big.pdf")
(pdf->slide/bitmap #:title "Contract benchmarks"
                   #:scale .7
                   "contracts-big.pdf")

(pslide/title "Gradual Typing benchmarks"
              #:go (coord .5 .55 'cc)
              (bitmap "suffixtree.png"))


;(go 29 29 #t #:title "Contract benchmarks")

#;(pslide/title "Scheme benchmarks"
              #:go (coord .3 .8 'cc) (scale (bitmap (~a "legend" ".png")) .8)
              #:go (coord .5 .4 'cc) (scale (bitmap (~a "chart" ".png")) 1.5))

#;(pslide #:go (coord .5 .5 'cc) (scale (bitmap (~a "contract-bench" ".png")) .9))


(tslide "The future of Pycket")

(pslide/title "What works ..."
              
              #:go (coord .1 .15 'lt)
              20
              (t/cant "Basic Scheme")
              0
              (t/inc "  lambda, call-with-values, call/cc, complex?")
              20
              (t/cant "Core Racket")
              0
              (t/inc "  continuation-marks, make-hash, contract")
              20
              (t/cant "Structures and classes")
              0
              (t/inc "  struct, struct-property, object%, mixin")
              20
              (t/cant "Input/output")
              0
              (t/inc "  print, read, call-with-input-file")
              20
              (t/cant "Typed Racket")
              0
              (t/inc "  #lang typed/racket")
              20
              (t/cant "Contracts")
              0
              (t/inc "  chaperone-procedure, make-contract")
              )

(pslide/title "What doesn't work ..."
              
              #:go (coord .1 .15 'lt)
              20
              (t/cant "Concurrency and parallelism")
              0
              (t/inc "  thread, future, place")
              20
              (t/cant "FFI")
              0
              (t/inc "  make-ctype, editor%")
              20
              (t/cant "DrRacket")
              0
              (t/inc "  ")
              20
              (t/cant "Scribble")
              0
              (t/inc "  ")
              20
              (t/cant "Compilation at runtime")
              0
              (t/inc "  eval, compile")
              20
              (t/cant "Networking")
              0
              (t/inc "  web-server, tcp-connect")
              )

(pslide/title "Next steps"
              #:go c
              (t/cant "AOT + JIT = ♥" (+ 10 size1)))

(pslide/title "Next steps"
              #:go c
              (t/cant "Expose tracing to programs" (+ 10 size1)))

(pslide/title "Next steps"
              #:go c
              (t/cant "Accelerate branchy programs" (+ 10 size1)))


(define (wrap-up [thanks? #t])
  (slide  #:layout 'center
          (vc-append 20
           (shadow-frame
            (vl-append
             (t/cant "Tracing JIT compilers:" size2)
             (t/cant " ★ great for functional languages" size2)
             (t/cant " ★ great for generic functions" size2)
             (t/cant " ★ great for gradual typing" size2)
             )
            #:shadow-descent 10)

         (t "")
         (blank 40)
         (t/inc "github.com/samth/pycket" size2))))


(parameterize ([current-background-pict (bitmap plt-background-path)])
  (wrap-up #t))
;(tslide (t/inc "github.com/samth/pycket" size1))
