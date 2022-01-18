;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(planet plai/plai:1:3/lang/reader)
;Review example.. 

  ; Ch 5 deffered sub.. 

 
 (define (lookup name ds)
   (type-case Env ds
      [mtSub () (error 'lookup "no binding for identifier")]
      [aSub  (bound-name bound-value rest-ds)
	     (if (symbol=? bound-name name)
		 bound-value
                 (lookup name rest-ds))]))
              
 
 
;parser.. 
(define (parse sexp)
  (cond
    [(number? sexp)(num sexp)]
    [(symbol? sexp)(id sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [('with) (with (first (second sexp))
                      (parse (second (second sexp)))
                      (parse (third sexp)))]
       [else (app (first sexp)(parse (second sexp)))])]))
             



(define-type CFAE/L
[num (n number?)]
[add (lhs CFAE/L?) (rhs CFAE/L?)] 
[id (name symbol?)]
[fun (param symbol?) (body CFAE/L?)]
[with (name symbol?)(name-expr CFAE/L?)(body CFAE/L?)]
[app (fun-expr CFAE/L?) (arg-expr CFAE/L?)])

(define-type CFAE/L-Value
[numV (n number?)]
[closureV (param symbol?)
(body CFAE/L?)
(env Env?)]
[exprV (expr CFAE/L?)
(env Env?)])
(define-type Env
[mtSub]
[aSub (name symbol?) (value CFAE/L-Value?) (env Env?)])

;; num+ : CFAE/L-Value CFAE/L-Value −! numV
(define (num+ n1 n2)
(numV (+ (numV-n (strict n1)) (numV-n (strict n2)))))
;; num-zero? : CFAE/L-Value !boolean

(define (num-zero? n)
(zero? (numV-n (strict n))))

;; strict : CFAE/L-Value ! CFAE/L-Value [excluding exprV]

(define (strict e)
(type-case CFAE/L-Value e
[exprV (expr env)
(local ([define the-value (strict (interp expr env))])
(begin
(printf "Forcing exprV to  ̃a  n" the-value)
the-value))]
[else e]))


;; interp : CFAE/L Env !CFAE/L-Value
(define (interp expr env)
(type-case CFAE/L expr
[num (n) (numV n)]
[add (l r) (num+ (interp l env) (interp r env))]
[with (bound-id named-expr bound-body)
             (interp bound-body
                            expr
                            (interp named-expr expr env)
                                 env)]
[id (v) (lookup v env)]
[fun (bound-id bound-body)
(closureV bound-id bound-body env)]
[app (fun-expr arg-expr)
(local ([define fun-val (strict (interp fun-expr env))]
[define arg-val (exprV arg-expr env)])
(interp (closureV-body fun-val)
(aSub (closureV-param fun-val)
arg-val
(closureV-env fun-val))))]))

;;(interp (add (num 1) (num 2)) (+ 2 3))
