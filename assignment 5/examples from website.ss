;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(planet plai/plai:1:3/lang/reader)
;Review example.. 
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)
       (rhs F1WAE?)]
  [sub (lhs F1WAE?)
       (rhs F1WAE?)]
  [div (lhs F1WAE?)
       (rhs F1WAE?)]
  [mult (lhs F1WAE?)
        (rhs F1WAE?)]
  [with (name symbol?)(name-expr F1WAE?)(body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?) (arg F1WAE?)]
  )
  ; Ch 5 deffered sub.. 
 (define-type DefrdSub
 [mtSub]
 [aSub (name symbol?)(value number?)(ds DefrdSub?)])
 
 (define (lookup name ds)
   (type-case DefrdSub ds
      [mtSub () (error 'lookup "no binding for identifier")]
      [aSub  (bound-name bound-value rest-ds)
	     (if (symbol=? bound-name name)
		 bound-value
                 (lookup name rest-ds))]))
              
 
 
 
(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

(define (lookup-fundef fun-name fundefs)
  (cond
    [(empty? fundefs) (error fun-name "Function not found")]
    [else (if (symbol=? fun-name (fundef-fun-name (first fundefs)))
              (first fundefs)
              (lookup-fundef fun-name (rest fundefs)))]))

; sub 
;(define (subst expr sub-id val)
 ; (type-case F1WAE expr
  ;      [num (n) expr]
   ;     [add (l r) (add (subst l sub-id val)
    ;                    (subst r sub-id val))]
     ;   [sub (l r) (sub (subst l sub-id val)
      ;                  (subst r sub-id val))]
       ; [div (l r) (div (subst l sub-id val)
        ;                (subst r sub-id val))]
        ;[mult (l r) (mult (subst l sub-id val)
         ;                 (subst r sub-id val))]
        ;[with (bound-id named-expr bound-body)
         ;     (if (symbol=? bound-id sub-id)
          ;        (with bound-id
           ;             (subst named-expr sub-id val)
            ;            bound-body)
             ;     (with bound-id
              ;          (subst named-expr sub-id val)
               ;         (subst bound-body sub-id val)))]
      ;  [id (v) (if (symbol=? v sub-id) val expr)]
       ; [app (fun-name arg-expr)
        ;     (app fun-name (subst arg-expr sub-id val))]))

;example on function use..
;(define x (fundef 'foo 'x (num 3)))
;(FunDef? x)
;(fundef-fun-name x)
;(fundef-arg-name x)
;(fundef-body x)

; interp the string
(define (interp expr fun-defs ds)
  (type-case F1WAE expr
       [num (n) n]
       [add (l r) (+ (interp l fun-defs ds)(interp r fun-defs ds))]
       [sub (l r) (- (interp l fun-defs ds)(interp r fun-defs ds))]
       [div (l r) (/ (interp l fun-defs ds)(interp r fun-defs ds))]
       [mult (l r)(* (interp l fun-defs ds)(interp r fun-defs ds))]
       [with (bound-id named-expr bound-body)
             (interp bound-body
                            fun-defs
                            (interp named-expr fun-defs ds)
                                 ds)]
       [id (v) (lookup v ds)]
       [app (fun-name arg-expr)
            (local ([define the-fun-def (lookup-fundef fun-name fun-defs)])
              (interp (fundef-body the-fun-def)
                      fun-defs
                      (aSub  (fundef-arg-name the-fun-def)
                             (interp arg-expr fun-defs ds)
                      (mtSub))))]))

;parser.. 
(define (parse sexp)
  (cond
    [(number? sexp)(num sexp)]
    [(symbol? sexp)(id sexp)]
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
       [('with) (with (first (second sexp))
                      (parse (second (second sexp)))
                      (parse (third sexp)))]
       [else (app (first sexp)(parse (second sexp)))])]))
             

; parse the string
; ch 5 example.. 
(parse '{double 5})
(parse '{double {+ 3 5}})
; interp ch 5 no differed sub.. 
(interp (parse '{double {+ 3 5}}) (list (fundef 'double 'n (add (id 'n)(id 'n)))) (mtSub))
;interp the string
;(interp (parse '{double {double 5}}) (list (fundef 'double 'n (add (id ;'n) (id 'n)))))


(parse '{with {x 3} {+ x 5}})

;second example
;(interp (parse '{double {square 5}}) (list (fundef 'double 'n (add (id ;'n) (id 'n)))
;                                           (fundef 'square 'n (mult (id ;'n) (id 'n)))))
(parse '{with {x 5} {+ x {with {y 3} x}}})