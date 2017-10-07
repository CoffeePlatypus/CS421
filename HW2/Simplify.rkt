;siplify
(define (simplify exp)
  (cond ((number? exp) exp)
        ((equal? "ERROR" exp) exp)
        ((symbol? exp) exp)
        ( else (operate (car exp) (cadr exp) (caddr exp)))))

(define (operate op exp1 exp2)
  (cond ((equal? exp1 "ERROR") "ERROR")
        ((equal? exp2 "ERROR") "ERROR")
        ((equal? '+ op) (add (simplify exp1) (simplify exp2)))
        ((equal? '- op) (minus (simplify exp1) (simplify exp2)))
        ((equal? '/ op) (divide (simplify exp1) (simplify exp2)))
        ((equal? '* op) (multiply (simplify exp1) (simplify exp2)))))

(define (zeroo? exp)
  (equal? exp 0))

(define (add exp1 exp2)
  (cond ((equal? "ERROR" exp1) exp1)
        ((equal? "ERROR" exp2) exp2)
        ((zeroo? exp1) exp2)
        ((zeroo? exp2) exp1)
        ((and (number? exp1) (number? exp2)) (+ exp1 exp2))
        ((and (number? exp1) (symbol? exp2)) (list '+ exp2 exp1))
        ((and (symbol? exp1) (number? exp2)) (list '+ exp1 exp2))
        (else (cons '+ (order exp1 exp2))))) ;wrongish

(define (minus exp1 exp2)
  (cond ((equal? "ERROR" exp1) exp1)
        ((equal? "ERROR" exp2) exp2)
        ((equal? exp1 exp2) 0)
        ((zeroo? exp2) exp1)
        ((and (number? exp1) (number? exp2)) (- exp1 exp2))
        (else (list '- exp1 exp2))))

(define (divide exp1 exp2)
  (cond ((equal? "ERROR" exp1) exp1)
        ((equal? "ERROR" exp2) exp2)
        ((equal? exp2 1) exp1)
        ((zeroo? exp2) "ERROR")
        ((zeroo? exp1) 0)
        ((equal? exp1 exp2) 1)
        ((and (number? exp1) (number? exp2)) (/ exp1 exp2))
        (else (list '/ exp1 exp2))))

(define (multiply exp1 exp2)
  (cond ((equal? "ERROR" exp1) exp1)
        ((equal? "ERROR" exp2) exp2)
        ((zeroo? exp1) exp1)
        ((zeroo? exp2) exp2)
        ((equal? exp1 1) exp2)
        ((equal? exp2 1) exp1)
        ((and (number? exp1) (number? exp2)) (* exp1 exp2))
        (else (cons '* (order exp1 exp2)))))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (order exp1 exp2)
  (cond ((and (number? exp1) (symbol? exp2)) (list exp2 exp1))
        ((and (symbol? exp1) (number? exp2)) (list exp1 exp2))
        ((and (symbol? exp1) (symbol? exp2))
         (cond ((symbol<? exp1 exp1) (list exp1 exp2))
               (else (list exp2 exp1))))
        ((symbol? exp1) (list exp1 exp2))
        ((symbol? exp2) (list exp2 exp1))
        ((number? exp1) (list exp1 exp2))
        ((number? exp2) (list exp2 exp1))
        (else (list exp1 exp2))))


(order '(+ 3 d) 'a)
(simplify '(* (+ 3 d) 1))
  
;(simplify '(/(/ 0 0) 0)) 
;(simplify '(+ (+ 3 5) (* (/ y 1) (+ x 0)))); ==> (+ 8 (* x y)) 
;(simplify '(+ z (/ 53 0))); ==> ERROR 
;(simplify '(+ z a)); ==> (+ a z) 
;(simplify '12); ==> 12 
;(simplify '(+ (- 5 2) 9)); ==> 12 
;(simplify '(+ x a)); ==> (+ a x)
;(simplify '(/ (+ 3 a) (+ a 3))); ==> 1
(simplify '(+ a 3)); ==> (+ 3 a)
(simplify '(* (- 1 a) 3)); ==> (+ 3 (- 1 a))
;(simplify '12); ==> 12
;(simplify 'x); ==> x
;(simplify '(- (+ z (+ (+ (+ a (+ z 2)) (- (- 7 x) (- 0 b))) (* (/ (/ a z) (- x 1)) y))) y))
;==> (- y (+ z (+ (+ (+ a (+ 2 z)) (- b (- 7 x))) (* y (/ (/ a z) (- 1 x))))))       
       


  
       