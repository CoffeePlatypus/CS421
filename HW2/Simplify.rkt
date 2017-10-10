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
        (else (cons '+ (order exp1 exp2))))) ;wrongish maybe

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

(define (oper<? op1 op2)
  (cond ((equal? op1 '*) #t)
        ((equal? op2 '*) #f)
        ((equal? op1 '+) #t)
        ((equal? op2 '+) #f)
        ((equal? op1 '-) #t)
        ((equal? op2 '-) #f)
        (else #t)))

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
        ((oper<? (car exp1) (car exp2)) (list exp1 exp2))
        (else (list exp2 exp1))))

;map
(define (map-make) '())
(define (map-key map) (car (car map)))
(define (map-value map) (cadr (car map)))

(define (map-put map k v)
  (cons (cons k (cons v '())) map))

(define (map-get map k)
  (cond ((null? map) '())
        ((equal? k (map-key map)) (map-value map))
        (else (map-get (cdr map) k))))

(define (map-contains map k)
  (cond ((equal? map '()) #f)
        ((equal? k (map-key map)) #t)
        (else (map-contains (cdr map) k))))

(define (map-remove map k)
  (cond ((null? map) map)
        ((equal? (map-key map) k) (cdr map))
        (else (cons (car map) (map-remove (cdr map) k)))))

(define (map-maker keys values)
  (cond ((null? keys) (map-make))
        (else (map-put (map-maker (cdr keys) (cdr values)) (car keys) (car values)))))
;footprint
(define (footprint exp)
  (* 4 (tallier (count exp (map-make)))))

(define (tallier map)
  (cond ((equal? map '()) 0)
        (else (+ 1 (tallier (cdr map))))))

(define (isoperand? c) (or (equal? c '+) (equal? c '-) (equal? c '/) (equal? c '*)))

(define (count exp map)
  (cond ((equal? exp '()) map)
        ((isoperand? (car exp)) (count (cdr exp) map))
        ((list? (car exp)) (count (cdr exp) (count (car exp) map)))
        ((map-contains map (car exp)) (count (cdr exp) (map-put (map-remove map (car exp)) (car exp) (+ (map-get map (car exp)) 1))))
        (else (count (cdr exp) (map-put map (car exp) 1)))))

(footprint '(+ (+ 5 5) (* (/ y 1) (+ y 5))))




;(map-remove (map-maker '(4 5 4 4) '(3 4 6 7)) 4)
;map

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
(simplify '(- (+ z (+ (+ (+ a (+ z 2)) (- (- 7 x) (- 0 b))) (* (/ (/ a z) (- x 1)) y))) y))
;==> (- y (+ z (+ (+ (+ a (+ 2 z)) (- b (- 7 x))) (* y (/ (/ a z) (- 1 x))))))       
       


  
       