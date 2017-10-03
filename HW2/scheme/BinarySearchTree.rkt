;binay search tree
(define (bst-create-empty)
  '())

(define (bst-create root left right)
  (list root left right))

(define (bst-is-leaf? bst)
  (and (equal? (cadr bst) '()) (equal? (caddr bst) '())) #t)
        

;(bst-create 3 '(2 () ()) '())


;insert
(define (bst-insert bst f x)
  (cond ((equal? bst '()) (bst-create x (bst-create-empty) (bst-create-empty)))
        ((f (car bst) x) (bst-insert-left bst f x))
        (else (bst-insert-right bst f x))))
  
(define (bst-insert-left bst f x)
  (cond ((equal? (cadr bst) '()) (list (car bst) (bst-create x (bst-create-empty) (bst-create-empty)) (caddr bst))) 
        (else (list (car bst) (bst-insert (cadr bst) f x) (caddr bst))))) ;insert on left child of bst

(define (bst-insert-right bst f x)
  (cond ((equal? (caddr bst) '()) (list (car bst) (cadr bst) (bst-create x (bst-create-empty) (bst-create-empty)))) 
        (else (list (car bst) (cadr bst) (bst-insert (caddr bst) f x)))))
;insert

;contains
(define (bst-contains bst f g x)
  (display bst)
  (cond ((equal? bst '()) #f)
        ((f (car bst) x) #t)
        ((g (car bst) x) (bst-contains (cadr bst) f g x))
        (else (bst-contains (caddr bst) f g x))))
;contains

;remove
(define (bst-remove bst f g x)
  (display bst)
  (display "\n")
  (cond ((equal? bst '()) '())
        ((f (car bst) x) (bst-remove-root bst))
        ((g (car bst) x) (list (car bst) (bst-remove (cadr bst) f g x) (caddr bst)))
        (else (list (car bst) (cadr bst) (bst-remove (caddr bst) f g x)))))

(define (bst-remove-root bst)
  (cond ((bst-is-leaf? bst) '(() () ()))
        ((bst-is-leaf? (cadr bst)) (list (cadr bst) '() (caddr bst)))
        ((bst-is-leaf? (caddr bst)) (list (caddr bst) (cadr bst) '()))
        (else '()))) ;no
;remove

;preorder
;(define (bst-pre-elements bst)

;tree maker
(define (list->bst xs f)
  (tree-maker (bst-create-empty) xs f))

(define (tree-maker bst xs f)
  (cond ((equal? xs '()) bst)
        (else (tree-maker (bst-insert bst f (car xs)) (cdr xs) f))))
;tree maker
  
(list->bst '(6 5 7 3 4 9) >)  
(bst-remove (bst-insert (bst-insert (bst-create-empty) < 1) > 3) = > 1)
