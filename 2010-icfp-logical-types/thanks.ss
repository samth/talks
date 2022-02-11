#lang slideshow


(require slideshow/play (except-in "beamer.ss" title) "lib.ss" "config.ss")

(define (thank . names)
  (for/list ([n names])
    (list (t (string-append "Thanks to " n)))))

(define names (list "Matthias Felleisen" "Ryan Culpepper" "Stevie Strickland" "Ivan Gazeau" "Carl Eastlund" "Matthew Flatt"
                    "Guy Steele" "Mitch Wand" "Olin Shivers" "Felix Klock" "Jesse Tov" "Aaron Turon" "Dan Brown"
                    "Vincent St-Amour" "Robby Findler" "Dave Herman" "Eric Allen" "Victor Luchangco" "David Chase" "Christine Flood"
                    "Sukyoung Ryu" "Jan-Willem Maessen" "Katie Edmonds" "James Jungbauer" "James Hamblin" "Lazlo Babai"
                    "Elizabeth Tobin" "Steve Hochstadt"))

(define (shuffle l) (sort l < #:cache-keys? #t #:key (lambda _ (random))))

(define (name-picts) (for/list ([n (shuffle names)])
                     (parameterize ([current-font-size 24])
                       (htl-append (t "Thanks to ") (apply ltl-superimpose (t n) (map (compose ghost t) names))))))

(define (thanks)
  (play-n #:steps 1 #; (* (length names) 1)
          #:delay 0.5
          #:skip-first? #t
          (let ([state (name-picts)])
            (lambda (n)
              (begin0
                (inset
                 (cc-superimpose
                  (current-title-background-pict)
                  (vc-append
                   (blank 40)
                   (text "Thank You" (current-title-font) title-text-size)
                   (blank 60)
                   (text "Code and Documentation" (current-main-font) (current-font-size))
                   (text "http://www.racket-lang.org" `(bold . ," Inconsolata") (current-font-size))
                   (blank 20)
                   (text "samth@ccs.neu.edu" " Inconsolata" (current-font-size))
                   (blank 40)
                   #;(parameterize ([current-font-size 20])
                   #;(para "Thanks to the Mozilla Corporation"))))
                 (- (get-margin)))
                (if (null? (cdr state))
                    (set! state (name-picts))
                    (set! state (cdr state))))))))
(provide thanks)
