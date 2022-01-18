;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(planet plai/plai:1:3/lang/reader)
(define (parse sexp)
  (cond
    [(number? sexp)(num sexp)]
    [(list? sexp)
     (case (first sexp)
       [(*) (multi (parse (second sexp))
                  (parse (third sexp)))]
       [(/) (div (parse (second sexp))
                 (parse (third sexp)))]
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(-) (sub (parse (second sexp))
                 (parse (third sexp)))]
       [('fun)  (fun  (first sexp)
                      (parse (second sexp)))]
       [else ((app (first sexp)(parse (second sexp))))])]))


(define-type FAE
[num (n number?)]
[add (lhs FAE?) (rhs FAE?)]
[sub (lhs FAE?) (rhs FAE?)]
[multi (lhs FAE?) (rhs FAE?)] 
[div (lhs FAE?) (rhs FAE?)]
[id (name symbol?)]
[fun (param symbol?) (body FAE?)]
[app (fun-expr FAE?) (arg-expr FAE?)])
(define-type FAE-Value
[numV (n number?)]
[closureV (param symbol?)
(body FAE?)
(ds DefrdSub?)])
(define-type DefrdSub
[mtSub]
[aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

;; lookup : symbol DefrdSub! FAE-Value

(define (lookup name ds)
(type-case DefrdSub ds
[mtSub () (error 'lookup "no binding for identifier")]
[aSub (bound-name bound-value rest-ds)
(if (symbol=? bound-name name)
bound-value
(lookup name rest-ds))]))
;; num+ : numV numV −! numV

;(define (lookup name ds)
;(type-case DefrdSub ds
;[mtSub () (error 'lookup "no binding for identifier")]
;[aSub (bound-name bound-value rest-ds)
;(if (symbol=? bound-name name)
;bound-value;(lookup name rest-ds))]))

(define (num+ n1 n2)
(numV (+ (numV-n n1) (numV-n n2))))
(define (num- n1 n2)
  (numV (- (numV-n n1) (numV-n n2))))
(define (num* n1 n2)
  (numV (* (numV-n n1) (numV-n n2))))
(define (num/ n1 n2)
  (numV (/ (numV-n n1) (numV-n n2))))

;; interp : FAE DefrdSub!FAE-Value
(define (interp expr ds)
(type-case FAE expr
[num (n) (numV n)]
[add (l r) (num+ (interp l ds) (interp r ds))]
[sub (l r) (num- (interp l ds) (interp r ds))]
[multi (l r) (num* (interp l ds) (interp r ds))]
[div (l r) (num/ (interp l ds) (interp r ds))]
[id (v) (lookup v ds)]
[fun (bound-id bound-body)
(closureV bound-id bound-body ds)]
[app (fun-expr arg-expr)
(local ([define fun-val (interp fun-expr ds)])
(interp (closureV-body fun-val)
(aSub (closureV-param fun-val)
(interp arg-expr ds)
(closureV-ds fun-val))))]))

;(parse '{fun 'x {+ x x})