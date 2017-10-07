;binay search tree
(define (bst-create-empty)
  '())

(define (bst-create root left right)
  (list root left right))

(define (bst-is-leaf? bst)
  (and (equal? (cadr bst) '()) (equal? (caddr bst) '())))
        
;insert
(define (bst-insert bst f x)
  (cond ((equal? bst '()) (bst-create x (bst-create-empty) (bst-create-empty)))
        ((f x (car bst)) (bst-insert-left bst f x))
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
        ((f x (car bst)) #t)
        ((g x (car bst)) (bst-contains (cadr bst) f g x))
        (else (bst-contains (caddr bst) f g x))))
;contains

;remove
(define (bst-remove bst f g x)
  (display bst)
  (display "\n")
  (cond ((equal? bst '()) '())
        ((f x (car bst)) (bst-remove-element bst f g))
        ((g x (car bst)) (list (car bst) (bst-remove (cadr bst) f g x) (caddr bst)))
        (else (list (car bst) (cadr bst) (bst-remove (caddr bst) f g x)))))

(define (bst-remove-element bst f g)
  (cond ((bst-is-leaf? bst) '(() () ()))
        ((bst-is-leaf? (cadr bst)) (list (cadr bst) '() (caddr bst)))
        ((bst-is-leaf? (caddr bst)) (list (caddr bst) (cadr bst) '()))
        (else (list (bst-max (cadr bst)) (bst-remove (cadr bst) f g(bst-max (cadr bst))) (caddr bst))))) ;no'=

(define (bst-max bst)
  (cond ((null? bst) bst)
        ((null? (caddr bst)) (car bst))
        (else (bst-max (caddr bst)))))

;preorder
(define (bst-pre-elements bst)
  (cond ((null? bst) '())
        (else (cons (car bst) (append (bst-pre-elements (cadr bst)) (bst-pre-elements (caddr bst)))))))
;inorder
(define (bst-in-elements bst)
  (cond ((null? bst) bst)
        (else (append (bst-in-elements (cadr bst)) (cons (car bst) (bst-in-elements (caddr bst)))))))
;postorder
(define (bst-post-elements bst)
  (cond ((null? bst) bst)
        (else (append (bst-post-elements (cadr bst)) (append (bst-post-elements (caddr bst)) (cons (car bst) '()))))))

;tree maker
(define (list->bst xs f)
  (tree-maker (bst-create-empty) xs f))

(define (tree-maker bst xs f)
  (cond ((equal? xs '()) bst)
        (else (tree-maker (bst-insert bst f (car xs)) (cdr xs) f))))
;tree maker
(bst-is-leaf? (list->bst '(6 5 7 3 4 9) <))
(bst-remove (list->bst '(6 5 7 3 4 9) <) = < 6)
(bst-post-elements (list->bst '(6 5 7 3 4 9) <) )
(bst-pre-elements (list->bst '(6 5 7 3 4 9) <) )
(bst-in-elements (list->bst '(6 5 7 3 4 9) <) ) 
;(bst-pre-elements (bst-insert (bst-insert (bst-create-empty) < 1) > 3) = > 1)
(6 (5 (3 () (4 () ())) ()) (7 () (9 () ())))