(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (sq x) (* x x))

((compose sq sq) 2)

((compose car cdr) '(1 2 3 4 5))

((compose (lambda (x) (apply + x)) cdr) '(1 2 3 4 5))

(define (filter f list)
  (cond ((null? list) '())
        ((f (car list)) (cons (car list) (filter f (cdr list))))
        (else (filter f (cdr list)))))

(filter string? '(() 3 "hell0"))
