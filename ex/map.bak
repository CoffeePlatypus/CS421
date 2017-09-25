(define (map-make) '())

(define (map-put map k v)
  (cons (cons k (cons v '())) map))

(define (map-get map k)
  (cond ((null? map) '())
        ((equals? k (car (car map))) (cadr map))
        (else (map-get (cdr map) k))))

(define (map-remove map k)
  (cond ((null? map) map)
        ((equal? (car (car map)) k) (cdr map))
        (else (cons (car map) (map-remove (cdr map) k)))))
                 
                                     