;pair 
(define (pair x y )
  (cond ((null? x) '())
        ((null? y) '())
        (else (cons (cons (car x) (cons (car y) '())) (pair (cdr x) (cdr y))))))

;double
(define (double list ele)
  (cond ((null? list) '())
        ((equal? (car list) ele) (cons (car list) (cons (car list) (double (cdr list) ele))))
        (else (cons (car list) (double (cdr list) ele)))))

;list-replace
(define (list-replace list sym val)
  (cond ((null? list) '())
        ((list? (car list)) (cons (list-replace (car list) sym val) (list-replace (cdr list) sym val)))
        ((equal? (car list) sym) (cons val (list-replace (cdr list) sym val)))
        (else (cons (car list) (list-replace (cdr list) sym val)))))

;repeat
(define (repeat val count)
  (cond ((= count 0) '())
        (else (cons val (repeat val (- count 1))))))

(define (carnotlist l)
  (not (list? (car l))))

(define (pluscdr l)
  (cons (cons (car (car l)) (list (+ (car (cdr (car l))) 1))) (cdr (cdr l))))

;rle
(define (rle l)
  (cond ((null? l) l)
        ((null? (cdr l)) l)
        ((carnotlist l) (rle (cons (cons (car l) (cons 1 '())) (cdr l))))
        ((equal? (car (car l)) (cadr l)) (rle (pluscdr l)))
        (else (cons (car l) (rle (cdr l))))))

;rld ----> todo fix (rld (rle '( 1 1 2 3 3 3)))
(define (rld list)
  (cond ((null? list) '())
        (else (cons (repeat (car (car list)) (car(cdr (car list)))) (rld (cdr list))))))


;pick
(define (pick l n)
  (cond ((>= 0 n) '())
        (else (supercar (group l n)))))

(define (startgroup l)
  (cons (list (car l) (cadr l)) (cdr (cdr l))))

(define (extendgroup l)
  (cons (cons (car (car l)) (cons (car (cdr l)) (cdr (car l)))) (cdr (cdr l))))

(define (supercar l)
  (cond ((null? l) l)
        ((carnotlist l) l)
        (else (cons (car (car l)) (supercar (cdr l))))))

(define (group l n)
  (cond ((null? l) '())
        ((null? (cdr l)) l)
        ((carnotlist l) (group (startgroup l) n))
        ((= (length (car l)) n) (cons (car l) (group (cdr l) n)))
        (else (group (extendgroup l) n))))

;infix
(define (infix exp)
  (cond ((null? exp) exp)
        ((and (null? (cdr exp)) (carnotlist exp)) exp)
        ((null? (cdr exp)) (list (infix (car exp))))
        ((carnotlist (cdr exp)) (cons (cadr exp) (cons (car exp) (infix(cdr (cdr exp)))))) 
        (else (cons (infix(cadr exp)) (cons (car exp) (infix (cdr (cdr exp))))))))

;env
(define (start-env) '())

(define (def-var env var val)
  (cons (cons var (cons val '())) env))

(define (get env var)
  (cond ((null? env) "undefined")
        ((equal? (car (car env)) var) (car (cdr (car env))))
        (else (get (cdr env) var))))
; vars to add to env; 
(define (make-env vars env)
  (cond ((null? vars) env)
        (else (def-var (make-env (cdr vars) env) (car(car vars)) (cadr (car vars))))))

;evaluate
(define (evaluate exp)
  (evaluator exp (start-env)))

(define (operate op exp1 exp2)
  (cond ((equal? exp1 "undefined") exp1)
        ((equal? exp2 "undefined") exp2)
        ((equal? '+ op) (+ exp1 exp2))
        ((equal? '- op) (- exp1 exp2))
        ((equal? '/ op) (/ exp1 exp2))
        ((equal? '* op) (* exp1 exp2))))

(define (is-bin-op? op)
  (or (equal? '+ op) (equal? '- op) (equal? '/ op) (equal? '* op)))

(define (evaluator exp env)
  (cond ((number? exp) exp)
        ((equal? "undefined" exp) exp)
        ((symbol? exp) (get env exp))
        ((is-bin-op? (car exp)) (operate (car exp) (evaluator (cadr exp) env) (evaluator (caddr exp) env)))
        (else (evaluator (cadr exp) (make-env (car exp) env)))))
