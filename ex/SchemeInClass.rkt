(define (nth x n)
  (cond ((= n 0) (car x))
        (else (nth (cdr x) (- n 1)))))

(nth '(1 2 3) 2)

(define (appendy x y)
  (cond ((null? x) y )
        (else (cons (car x) (appendy (cdr x) y)))))

(appendy '(1 2) '(3 4))

(define (reverser x)
  (cond ((null? x) x)
        (else (appendy (reverser (cdr x)) (cons (car x) '())))))

(reverser '(1 2 3 4))

