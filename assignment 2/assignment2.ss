;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(planet plai/plai:1:3/lang/reader)
; Assignment 2
(define-type AE
 [num (n number?)]
 [add (lhs AE?)
      (rhs AE?)]
 [sub (lhs AE?)
      (rhs AE?)]
 [multi (lhs AE?)
      (rhs AE?)]
 [div (lhs AE?)
      (rhs AE?)])

(define (parse sexp)
(cond
[(number? sexp) (num sexp)]
[(list? sexp)
(case (first sexp)
[(+) (add (parse (second sexp))
(parse (third sexp)))]
[(-) (sub (parse (second sexp))
(parse (third sexp)))]
[(*) (multi (parse (second sexp))
(parse (third sexp)))] 
[(/) (div (parse (second sexp))
(parse (third sexp)))] 
)]))

; Assignment 2 test cases
(parse '{+ 1 2})
(parse '{- 1 2})
(parse '{* 1 2})
(parse '{/ 1 2})