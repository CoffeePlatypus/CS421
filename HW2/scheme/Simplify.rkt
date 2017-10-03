
(define (simplify exp)
  (cond ((number? exp) exp)
        ((equal? "ERROR" exp) exp)
        ((symbol? exp) exp)
        ( else (operate (car exp) (evaluator (cadr exp) env) (evaluator (caddr exp) env)))))

(define (operate op exp1 exp2)
  (cond ((equal? exp1 "ERROR") "ERROR")
        ((equal? exp2 "ERROR") "ERROR")
        ((equal? '+ op) (add exp1 exp2))
        ((equal? '- op) (minus exp1 exp2))
        ((equal? '/ op) (divide exp1 exp2))
        ((equal? '* op) (multiply exp1 exp2))))

(define (add exp1 exp2)
  (cond ((number? exp1)
         (cond ((number? exp2) (+ exp1 exp2))
               ((symbol? exp2) '(+ exp2 exp1))
                (cond ((equal? exp1 0) exp2)
                      (else exp2))
               (else '(+ exp1 (simpify exp)))))
        ((symbol? exp1)
         (cond (


  
       