#lang plai-typed
(require (typed-in racket
                   (car : (('a * 'b) -> 'a))
                   (cdr : (('a * 'b) -> 'b))
                   (cons : ('a 'b -> ('a * 'b)))))
(define-type WAE
  [numC (n : number)]
  ;; operator (+,-,/,*) and the two operands
  [binop (operator : symbol) (op1 : WAE) (op2 : WAE)]
  [withC (name : symbol)
         (expr : WAE)
         (body : WAE)]
  [idC (name : symbol)])

;; symbols: +,-,/,and * operators
;; used to compare to key in the get-binop-func and get-func-helper
;; functions below
(define symbols (list '+ '- '/ '*))
;; functions for addition, subtraction, multiplication, and division
;; returned by get-binop-func based on the key passed to get-binop-func
(define functions (list
                   (lambda (n1 n2)
                (+ n1 n2))
                    (lambda (n1 n2)
                (- n1 n2))
                    (lambda (n1 n2)
                (/ n1 n2))
                    (lambda (n1 n2)
                (* n1 n2))))

;; parse : s-expression -> WAE
;; converts s-expressions to WAEs
(define (parse [s : s-expression]) : WAE
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         ;; the WAE is either 'with, a binop, or undefined symbol
         [(not (equal? 'with (s-exp->symbol (first sl))))
             (binop (s-exp->symbol (first sl)) (parse (second sl)) (parse (third sl)))]
         [(equal? 'with (s-exp->symbol (first sl)))
           (let ([bindlist (s-exp->list (second sl))])
                  (process-bindlist bindlist sl))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

;; process-bindlist :: s-expr or listof-s-expr, listof-symbols -> WAE
;; processes all the bindings in the with statement,
;; outputting the abstract syntax for with statements
(define (process-bindlist  bindlist sl)
  (cond [(empty? bindlist) (parse (third sl))]
        ;; just a single assignment, so no need for
        ;; recursively calling process-bindlist as below
        [(s-exp-symbol? (first bindlist))
         (withC (s-exp->symbol (first  bindlist))
                          (parse (second bindlist))
                          (parse (third sl)))
         ]
        ;; bindlist is a list, so we must make sure to process
        ;; each assignment in bindlist 
        [else (withC
                 (s-exp->symbol (first (s-exp->list (first  bindlist))))
                 (parse (second (s-exp->list (first  bindlist))))
                 (process-bindlist (rest bindlist) sl))])
        )

;; get-func-helper :: listof-symbols symbol number -> number
;; returns the index of the function that corresponds with symbol
;; (i.e. returns index of addition function in the functions
;; list above if key = '+)
(define (get-func-helper listof-symbols [key : symbol] [count : number])
  (cond [(empty? listof-symbols) -1]
        [(equal? (first listof-symbols) key) count]
        [else
         (get-func-helper (rest listof-symbols) key (+ count 1))]))

;;(define symbols (list '+ '- '/ '*))
(test (get-func-helper symbols '+ 0) 0)
(test (get-func-helper symbols '_ 0) -1)
(test (get-func-helper symbols '- 2) 3)
(test (get-func-helper symbols '- 0) 1)

;; get-binop-func :: key -> <#procedure>
;; returns the respective function based on the
;; symbol given (i.e. returns an addition function if key = '+)
(define (get-binop-func key)
  (let ([count (get-func-helper symbols key 0)])
    (cond [(equal? count -1) (error 'binops "invalid binary operator")]
          [else (list-ref functions count)])))

;(test ((get-binop-func '~) 2 2) 0) ;; throws exception b/c invalid binary operator
(test ((get-binop-func '+) 2 2) 4)
(test ((get-binop-func '*) 2 2) 4)
(test ((get-binop-func '/) -2 2) -1)
(test ((get-binop-func '-) 2 2) 0)

;; subst :: WAE symbol WAE -> WAE
(define (subst [expr : WAE] [subst-id : symbol] [val : WAE]) : WAE
  (type-case WAE expr
      [numC (n) expr]
      [binop (oper l r)
             (binop oper
             (subst l subst-id val)
             (subst r subst-id val))]
      [withC (bound-id named-expr bound-body)
            (if (symbol=? bound-id subst-id)
                (withC bound-id 
                      (subst named-expr subst-id val)
                      bound-body)
                (withC bound-id
                      (subst named-expr subst-id val)
                      (subst bound-body subst-id val)))]
      [idC (v) (if (symbol=? v subst-id) 
                  val
                  expr)]))



(define (interp [wae : WAE]) : number
  (type-case WAE wae
    [numC  (n) n]
    [binop (oper n1 n2) ((get-binop-func oper) (interp n1) (interp n2))]
    [withC (name expr body)
           (interp (subst body name               ;; number from interp
                          (numC (interp expr))))] ;; wrapped in num so subst works right
    [idC (name) (error 'interp "free identifier (must be bound)")]))

;(process-bindlist '{with {x {+ 5 5}} {+ x x}} {x {+ 5 5}})
;; parse test cases 
(test (parse '3) (numC 3))
(test (parse '(+ 1 2)) (binop '+ (numC 1) (numC 2)))
(test (parse '{with {x {+ 5 5}} {+ x x}})
      (withC 'x (binop '+ (numC 5) (numC 5))
             (binop '+ (idC 'x) (idC 'x))))

(test (interp (parse '(with [(x 2) (y 2) (z 2)]
                    (with [(k (+ x (+ y z)))]
                      (+ x (+ z (- k y))))))) 8)

(test (parse '(with [(x 2) (y 2) (z 2)]
                    (with [(k (* x (+ y z)))]
                      (+ x (* z (/ k y))))))

 (withC 'x (numC 2)
   (withC 'y (numC 2)
     (withC 'z (numC 2)
        (withC 'k (binop '* (idC 'x)
                         (binop '+ (idC 'y) (idC 'z)))
          (binop '+ (idC 'x)
                 (binop '* (idC 'z)
                        (binop '/ (idC 'k) (idC 'y)))))))))
                      
          

                    

(test (interp (parse '3)) 3)
(test (interp (parse '(+ (* 2 4) 8))) 16)
(test (interp (parse '(with (x (+ 5 5)) x))) 10)
(test (interp (parse '(with (x (+ 6 4)) (with (y (+ x 2)) y)))) 12)

(test (interp (parse '{+ 3 4})) 7)
(test (interp (parse '{+ {+ 3 4} 7})) 14)
(test (interp (parse '{with {x {+ 5 5}} {+ x x}})) 20)
(test (interp (parse '{with {x 5} {+ x x}})) 10) 
(test (interp (parse '{with {x 5} {+ x {with {x 3} 10}}})) 15) 
(test (interp (parse '{with {x 5} {+ x {with {x 3} x}}})) 8) 
(test (interp (parse '{with {x 5} {+ x {with {y 3} x}}})) 10) 
(test (interp (parse '{with {x 5} {with {y x} y}})) 5) 
(test (interp (parse '{with {x 5} {with {x x} x}})) 5)

;; subtraction test cases 
(test (interp (parse '{* {- 1 3} {/ 4 2}})) -4)
(test (interp (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) 14) 
(test (interp (parse '{with {x 5} {with {y {- x 3}} {+ y y}}})) 4)

;; multi-armed with test cases 
(test (interp (parse '(with [(x 2)
                             (y 3)]
                      (with [(z (+ x y))]
                      (+ x z))))) 7)

(test (interp (parse '(with [(x 10)
                             (y 5)]
                      (with [(z (/ x y))]
                      (* x z))))) 20)

(test (interp (parse '(with [(x 0)
                             (y 0)]
                      (with [(z (+ x y))]
                      (+ x z))))) 0)

(test (interp (parse '(with [(x -4)
                             (y 5)]
                      (with [(z (+ x y))]
                      (+ x z))))) -3)

;(test/exn (interp (parse 'x)) "free")
