;((lambda (x) (- x 3)) 12)
;(cons (cdr '(1 (2 3) 5 6 (7 8))))
;(map cons '((1 2) (3 4) (5 6)))

(define (smallest list)
  (cond ((null? list) list)
        ((null? (cdr list)) (car list))
        ((< (car list) (cadr list)) (smallest(cons (car list) (cdr (cdr list)))))
        (else (smallest (cdr list)))))

(smallest '( 6 2 4 1 3))
        
(define (mono? num1 num2) (= 1 (- num2 num1)))


(define (listify list)
  (cond ((null? list) list)
        ((mono? (car list) (cadr list)) (cons (car list) (listify (cdr list))))
        (else (list (car list) (listify (cdr list))))))

(listify '(1 2 3 1 2 1))
        
