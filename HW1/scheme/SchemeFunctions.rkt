(define (pair x y )
  (cond ((null? x) '())
        ((null? y) '())
        (else (cons (cons (car x) (cons (car y) '())) (pair (cdr x) (cdr y))))))
;(pair '(1 2 3 4 5 6) '(a b c d e))

(define (double list ele)
  (cond ((null? list) '())
        ((equal? (car list) ele) (cons (car list) (cons (car list) (double (cdr list) ele))))
        (else (cons (car list) (double (cdr list) ele)))))

(define (list-replace list sym val)
  (cond ((null? list) '())
        ((list? (car list)) (cons (list-replace (car list) sym val) (list-replace (cdr list) sym val)))
        ((equal? (car list) sym) (cons val (list-replace (cdr list) sym val)))
        (else (cons (car list) (list-replace (cdr list) sym val)))))

(define (repeat val count)
  (cond ((= count 0) '())
        (else (cons val (repeat val (- count 1))))))

(define (carnotlist l)
  (not (list? (car l))))

(define (pluscdr l)
  (cons (cons (car (car l)) (list (+ (car (cdr (car l))) 1))) (cdr (cdr l))))

(cadr '((1 2) 2 3 4))
  
  

(define (rle l)
  (cond ((null? l) l)
        ((null? (cdr l)) l)
        ((carnotlist l) (rle (cons (cons (car l) (cons 1 '())) (cdr l))))
        ((equal? (car (car l)) (cadr l)) (rle (pluscdr l)))
        (else (cons (car l) (rle (cdr l))))))

(rle '(1 1 1 3 2 2 2 9 9 9 9 9 9))
    
(define (rld list)
  (cond ((null? list) '())
        (else (cons (repeat (car (car list)) (car(cdr (car list)))) (rld (cdr list))))))

;(define (pick l n)
;  (map (car (group l n))))

(define (startgroup l)
  (cons (list (car l) (cadr l)) (cdr (cdr l))))

(define (extendgroup l)
  (cons (cons (car (car l)) (cons (car (cdr l)) (cdr (car l)))) (cdr (cdr l))))
(extendgroup '((1 2) 3 4))

(length '(1 2 3))
(define (group l n)
  (display list)
  (cond ((null? l) '())
        ((carnotlist l) (group (startgroup l) n))
        ((= (length (car l)) n) (cons (car l) (group (cdr l) n)))
        (else (group (extendgroup l) n))))
(group '(1 2 3 4 5 6 7) 3)

;(pick '(1 2 3 4 5 6) 2)

  