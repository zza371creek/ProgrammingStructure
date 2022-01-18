;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(planet plai/plai:1:3/lang/reader)
; this is  Assignment 2 + assignment 3


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


(define-type WAE
[num (n number?)]
[add (lhs WAE?) (rhs WAE?)]
[sub (lhs WAE?) (rhs WAE?)]
[multi (lhs WAE?) (rhs WAE?)]
[div (lhs WAE?) (rhs WAE?)] 
[with (name symbol?) (named-expr WAE?) (body WAE?)]
[id (name symbol?)])

;; subst : WAE symbol WAE!WAE
(define (subst expr sub-id val)
(type-case WAE expr
[num (n) expr]
[add (l r) (add (subst l sub-id val)
(subst r sub-id val))]
[sub (l r) (sub (subst l sub-id val)
(subst r sub-id val))]
[multi (l r) (sub (subst l sub-id val)
(subst r sub-id val))]
[div (l r) (sub (subst l sub-id val)
(subst r sub-id val))]
[with (bound-id named-expr bound-body)
(if (symbol=? bound-id sub-id)
(with bound-id
(subst named-expr sub-id val)
bound-body)
(with bound-id
(subst named-expr sub-id val)
(subst bound-body sub-id val)))]
[id (v) (if (symbol=? v sub-id) val expr)]))

;; calc : WAE!number
(define (calc expr)
(type-case WAE expr
[num (n) n]
[add (l r) (+ (calc l) (calc r))]
[sub (l r) (- (calc l) (calc r))]
[multi (l r) (* (calc l) (calc r))]
[div (l r) (/ (calc l) (calc r))]
[with (bound-id named-expr bound-body)
(calc (subst bound-body
bound-id
(num (calc named-expr))))]
[id (v) (error 'calc "free identifier")]))


(define-type FAE
[num (n number?)]
[add (lhs FAE?) (rhs FAE?)]
[sub (lhs FAE?) (rhs FAE?)]
[multi (lhs FAE?) (rhs FAE?)]
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

(define (num+ n1 n2)
(numV (+ (numV-n n1) (numV-n n2))))
(define (num- n1 n2)
  (numV (- (numV-n n1) (numV-n n2))))
(define (num* n1 n2)
  (numV (* (numV-n n1) (numV-n n2))))

;; interp : FAE DefrdSub!FAE-Value
(define (interp expr ds)
(type-case FAE expr
[num (n) (numV n)]
[add (l r) (num+ (interp l ds) (interp r ds))]
[sub (l r) (num- (interp l ds) (interp r ds))]
[multi (l r) (num* (interp l ds) (interp r ds))]
[id (v) (lookup v ds)]
[fun (bound-id bound-body)
(closureV bound-id bound-body ds)]
[app (fun-expr arg-expr)
(local ([define fun-val (interp fun-expr ds)])
(interp (closureV-body fun-val)
(aSub (closureV-param fun-val)
(interp arg-expr ds)
(closureV-ds fun-val))))]))






; test cases
(calc (parse '{+ 1 2}))
(calc (parse '{- 1 2}))
(calc (parse '{* 1 2}))
(calc (parse '{/ 1 2}))
