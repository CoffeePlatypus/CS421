(define (pair x y )
  (cond ((null? x) '())
        ((null? y) '())
        (else (cons (cons (car x) (cons (car y) '())) (pair (cdr x) (cdr y))))))

(define (double list ele)
  (cond ((null? list) '())
        ((equal? (car list) ele) (cons (car list) (cons (car list) (double (cdr list) ele))))
        (else (cons (car list) (double (cdr list) ele)))))

(double '(a b c) 'a)
(double '(a (a b c) c) 'a)
(double '() 'a)
(double '(a (a (a)) (a)) '(a))

(define (list-replace list sym val)
  (cond ((null? list) '())
        ((list? (car list)) (cons (list-replace (car list) sym val) (list-replace (cdr list) sym val)))
        ((equal? (car list) sym) (cons val (list-replace (cdr list) sym val)))
        (else (cons (car list) (list-replace (cdr list) sym val)))))

(list-replace '(a b c) 'a 3)
(list-replace '(a (a b c) c) 'a 3)
(list-replace '() 'a 3)
(list-replace '(a (a (a))) 'a '(3))

(define (repeat val count)
  (cond ((= count 0) '())
        (else (cons val (repeat val (- count 1))))))

(repeat 3 3)


(define (rle listl)
  (cond ((null? listl) '())
        ( > 1 (length (car listl))) (rle (cons ( cons (car listl) (cons 1 '())) (car listl))))
        ((equal? (car (car listl)) (car (cdr listl))) (rle (cons ( list (car (car listl)) (+ (car (cdr(car listl))) 1)) (car listl))))
        (else ( cons (car listl) (rle (cdr list))))))

(rle '(1 1 1 3 2 2 2 9 9 9 9 9 9))
    
        

(define (rld list)
  (cond ((null? list) '())
        (else (cons (repeat (car (car list)) (car(cdr (car list)))) (rld (cdr list))))))

(rld '((1 3) (3 1) (2 3) (9 6)))


(define (pick l n)
  (map (car (group l n))))
(define (group l n)
  (cond ((null? l) '())
        (
  