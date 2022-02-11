#lang slideshow

(require slideshow/code "lib.ss" scheme/gui (except-in "beamer.ss" title))

;; setup code
(current-keyword-list (list* "define:" "let:" "require/typed" "require/contract" ":" "→" 
                             "∪" "∀" "." "Any" "U" "rename-out" "λ" "Symbol" "|" ";"
                             "else" "provide/contract" "..." "define-type-alias" "Refinement" "⊃"
                             "->" "Number" "String" "Bool" "Number," "Pair" "Boolean" "Id" "," "match"
                             "Ids" "Stx" "Stxs" "type:" "filter:" "object:" "env:" "x:Any" "x:Number"
                             "s:(U Symbol Number String)" "s:Symbol" "s:(U Number String)" "s:Number" "s:String"
                             "   y:(U Number String)" "y:Number" "Pict" "⊢" "@" "∨" "∧" "(U Number String)" "::="
                             "list" "?" "app" "list-no-order" "dispatch-rules" "string-arg" "integer-arg"
                             "#lang racket" "planet" "define-view" "define-match-rule" "not-false" "syntax-parser"
                             "syntax-parse" "define-matcher" "cons"
                             (current-keyword-list)))
(set-page-numbers-visible! #f)
(code-scripts-enabled #t)

(define pnf (send the-font-list find-or-create-font 16 'default 'normal 'normal))
(current-page-number-font pnf)

(current-title-font " LMSans17")
(current-main-font " LMSans17")
(unless #f #;printing? (current-code-font " Inconsolata"))
#;(when printing? (current-code-tt (lambda (s) (text s (current-code-font) (- (current-font-size) 2)))))
;(current-code-font " Anonymous Pro")
;(current-code-font " LMTypewriter12")

(define dark? #f)

(current-background-pict (if #t #;printing? 
                             (blank 1024 768)
                             (if dark?
                                 (bitmap plt-dark-background-path)
                                 (bitmap plt-background-path))))
(current-title-background-pict (bitmap plt-title-background-path))
(current-base-color "black")
#;(current-literal-color "black")
#;(current-id-color "black")

