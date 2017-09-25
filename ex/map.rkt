;objecs are collections of definitions
(define (map-make) '())

(define (map-put map k v)
  (cons (cons k (cons v '())) map))

;entity (k v)
;function that takes a list as input 
(define (entity-key car)) ;entity-key is car
(define (entity-value cadr)) ;second function

(define (map-get map k)
  (cond ((null? map) '())
        ((equals? k (entity-key(car map))) (entity-value(car map)))
        (else (map-get (cdr map) k))))

(define (map-remove map k)
  (cond ((null? map) map)
        ((equal? (entity-key (car map)) k) (cdr map))
        (else (cons (car map) (map-remove (cdr map) k)))))
                 
(define keys '(a b c d e f))
(define values '(1 2 3 4 5 6))

(define (map-maker keys values)
  (cond ((null? keys) (map-create))
        (else (map-put (map-maker (cdr keys) (cdr values)) (car keys) (car values)))))

(map-maker keys values)