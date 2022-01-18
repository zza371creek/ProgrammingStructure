;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(planet plai/plai:1:3/lang/reader)
(define (parse sexp)
  (cond
    [(number? sexp)(num sexp)]
    [(list? sexp)
     (case (first sexp)
       [(*) (mult (parse (second sexp))
                  (parse (third sexp)))]
       [(/) (div (parse (second sexp))
                 (parse (third sexp)))]
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(-) (sub (parse (second sexp))
                 (parse (third sexp)))]
       [('with) (with (parse (first second)
                      (parse (second second)
                      (parse third))))]
       [('fun) (fun (first sexp) (parse (second sexp)))]
       [else (app (first sexp)(parse (second sexp)))])]))