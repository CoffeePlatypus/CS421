;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converts an s-expression into a string
;; INPUT: an S-expression EXP
;; OUTPUT: a SCHEME String corresponding to EXP
(define (exp->string exp)
  (cond ((number? exp) (number->string exp))
        ((symbol? exp) (symbol->string exp))
        ((list? exp) (exp->string (car exp)))))

;; INPUT: a list of lists
;; OUTPUT: a list containing all elements of the first-level lists
(define (flatten list-of-lists)
  (cond ((null? list-of-lists) '())
        (else (append (car list-of-lists) (flatten (cdr list-of-lists))))))

;; this is for all error handling.
;; programmers don't use this function but
;; the interpreter calls this function to
;; signal some type of programmer error
(define (error msg)
  (display "ERROR: ")
  (display msg)
  (newline))

;; THERE ARE TWO SUPPORTED TYPES: 'int and 'boolean
;; INPUT: an element of the ART-C language
;; OUTPUT: the type of that element
(define (type-of val)
  (cond ((or (number? val) (equal? val 'int)) 'int)
        ((or (bool? val) (equal? val 'boolean)) 'boolean)))

;; A MAP is a list of key-value pairs
;; INPUT: a MAP and a KEY
;; OUTPUT: The value associated with the key or 'error
(define (map-get map x)
  (cond ((null? map) 'error)
        ((equal? (car (car map)) x) (cadr (car map)))
        (else (map-get (cdr map) x))))

;; INPUT : A MAP AND KEY
;; OUTPUT : true if the key is in the map and false otherwise
(define (map-contains map x)
  (cond ((null? map) #f)
        ((equal? (car (car map)) x) #t)
        (else (map-contains (cdr map) x))))

;; INPUT : A MAP, KEY and VALUE
;; OUTPUT: The map that results from replacing the key with the new value.  If
;; the map doesn't contain KEY, then 'error is returned
(define (map-replace map key val)
  (cond ((null? map) 'error)
        ((equal? (car (car map)) key)
         (cons (list key val) (cdr map)))
        (else
         (cons (car map) (map-replace (cdr map) key val)))))

;; INPUT : A MAP, Key and Value
;; OUTPUT : The map that results from adding a key-value pair.  This
;; allows for duplicate keys (the most-recently added is nearer the front of the list
(define (map-add map key val)
  (cons (list key val) map))

;; INPUT: A MAP and KEY
;; OUTPUT: The map that results from deleting the key.  No errors occur if the map
;; doesn't contain the key
(define (map-delete map key)
  (cond ((null? map) map)
        ((equal? (car (car map)) key) (cdr map))
        (else (cons (car map)
                    (map-delete (cdr map) key)))))

;had to add filter
(define (filter f list)
  (cond ((null? list) '())
        ((f (car list)) (cons (car list) (filter f (cdr list))))
        (else (filter f (cdr list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPEMAP : A SEMANTIC DOMAIN DATA TYPE
;; A typemap is a list of block-level declarations.
;; FORM: (((var1 type1) (var2 type2)) ((var3 type3) (var4 type4) (var5 type5)) ... )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT: NONE
;; OUTPUT: AN empty typemap
(define (typemap-create-empty) '())

;; INPUT: A TYPEMAP
;; OUTPUT: The largest address in use in the ENVIRONMENT
(define (typemap-type-of tm x)
  (map-get tm x))

;; INPUT: A TYPEMAP
;; OUTPUT: THE TYPEMAP THAT RESULTS FROM INSERTING A DECLARATIONS
(define (typemap-add tm decl)
  (map-add tm (car decl) (cadr decl)))

(define (typemap-delete tm key)
  (map-delete tm key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATE : A SEMANTIC DOMAIN DATA TYPE
;; A LIST OF (VAR, VALUE) pairs
;; FORM :  ( (var1 val1) (var2 val2) ... )
;; NOTE: A map can contain duplicate keys but innermost KEYS occur
;;       before outermost KEYS and hide them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INPUT : NONE
;; OUTPUT: AN EMPTY STATE
(define (state-create-empty) '())
  
;; INPUT: STATE and ID
;; OUTPUT: a new state such that the innermost scope now contains a
;;         new binding for the specified ID.  The bindings value is 'undefined.
(define (state-add state id)
  (map-add state id 'undefined))

;; INPUT : STATE and ID
;; OUTPUT: A new state such that the innermost id is removed
(define (state-delete state id)
  (map-delete state id))

;; INPUT: STATE and ID
;; OUTPUT: The value associated with the specified ID in the given state
(define (state-get-value state id)
  (map-get state id))

;; INPUT: STATE and ID
;; OUTPUT: A new state that results from changing the mapping from id->value in
;;         the specified state
(define (state-update state id value)
  (map-replace state id value))

;; INPUT: STATE and LIST-OF-IDS (VARIABLES)
;; OUTPUT: A new state that results from deleting all ids (the variables) from
;;         the specified state
(define (state-delete-all state variables)
  (cond ((null? variables) state)
        (else (state-delete-all (state-delete state (car variables)) (cdr variables)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THESE CLASSES CORRESPOND TO THE ABSTRACT SYNTAX SUCH THAT A "PROGRAM"
;; REPRESENT A PARSE-TREE.  THESE FUNCTIONS OPERATE AT THE 'SYNTACTIC' LEVEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (PROGRAM BODY)
(define (program-get-body stmt)
  (cadr stmt))

;; (BLOCK S1...SN)
(define (block-get-body stmt)
  (filter (lambda (x) (not (is-declaration? x))) (cdr stmt)))
  
(define (block-get-declarations stmt)
  (filter (lambda (x) (is-declaration? x)) (cdr stmt)))

;; (DECLARE TYPE VAR)
(define (declaration-get-type stmt)
  (cadr stmt))

(define (declaration-get-var stmt)
  (caddr stmt))

(define (is-declaration? stmt)
  (and (list? stmt) 
       (equal? (car stmt) 'declare)))

;; (:= VAR EXP)
(define (assignment-get-var stmt)
  (cadr stmt))

(define (assignment-get-exp stmt)
  (caddr stmt))

;; (IF TEST THEN [ELSE])
(define (if-get-test stmt)
  (cadr stmt))

(define (if-get-then stmt)
  (caddr stmt))

(define (if-has-else? stmt)
  (= (length stmt) 4))

(define (if-get-else stmt)
  (cadddr stmt))

;; (WHILE TEST BODY)
(define (while-get-test stmt)
  (cadr stmt))

(define (while-get-body stmt)
  (caddr stmt))

;; (SPRINT LABEL EXP)
(define (sprint-has-exp? stmt)
  (and (list? stmt)
       (= (length stmt) 3)))

(define (sprint-get-label? stmt)
  (cadr stmt))

(define (sprint-get-exp stmt)
  (caddr stmt))

;; INPUT: an expression EXP
;; OUTPUT: the operator of EXP (an element of ART-C)
(define (exp-get-operator exp)
  (car exp))

;; INPUT: an expression EXP
;; OUTPUT: the left-operand (an expression) of EXP
(define (exp-get-left-operand exp)
  (car (cdr exp)))

;; INPUT: an expression EXP
;; OUTPUT: the exp-get-right-operand (an expression) of EXP
(define (exp-get-right-operand exp)
  (car (cdr (cdr exp))))

;; INPUT: an expression EXP
;; OUTPUT: #t if the expression is a boolean literal and #f otherwise
(define (bool? exp)
  (or (equal? exp 'true)
      (equal? exp 'false)))

;; INPUT: a symbol
;; OUTPUT: #t if the symbol is 'true and #f if it is 'false and 'void' if neither
(define (symbol->bool sym)
  (cond ((equal? sym 'true) #t)
        ((equal? sym 'false) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;evaluator

(define (add exp1 exp2)
  (cond ((and (number? exp1) (number? exp2)) (+ exp1 exp2))
        (else (error "invalid operation on type"))))

(define (sub exp1 exp2)
  (cond ((and (number? exp1) (number? exp2)) (- exp1 exp2))
        (else (error "invalid operation on type"))))

(define (mult exp1 exp2)
  (cond ((and (number? exp1) (number? exp2)) (* exp1 exp2))
        (else (error "invalid operation on type"))))

(define (div exp1 exp2)
  (cond ((and (number? exp1) (number? exp2)) (/ exp1 exp2))
        (else (error "invalid operation on type"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (exph exp1 exp2)
  (cond ((= 1 exp2) exp1)
        (else (* exp1 (exph exp1 (- exp2 1))))))

(define (expon exp1 exp2)
  (cond ((and (number? exp1) (number? exp2)) (exph exp1 exp2)) ;write exp helper
        (else (error "invalid operation on type"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;todo \/
;(define (calrem exp1 exp2)
; found remainder function

(define (rem exp1 exp2)
  (cond ((and (number? exp1) (number? exp2)) (remainder exp1 exp2))
        (else (error "invalid operation on type"))))

(define (lt exp1 exp2)
  (cond ((and (number? exp1) (number? exp2))
         (cond ((< exp1 exp2) 'true)
               (else 'false)))
        (else (error "invalid operation on type"))))
(define (gt exp1 exp2)
  (cond ((and (number? exp1) (number? exp2))
         (cond ((> exp1 exp2) 'true)
               (else 'false)))        
        (else (error "invalid operation on type"))))

(define (gteq exp1 exp2)
  (cond ((and (number? exp1) (number? exp2))
         (cond((>= exp1 exp2) 'true)
              (else 'false)))
        (else (error "invalid operation on type"))))

(define (lteq exp1 exp2)
  (cond ((and (number? exp1) (number? exp2))
         (cond ((<= exp1 exp2) 'true)
               (else 'false)))
        (else (error "invalid operation on type"))))

(define (eq exp1 exp2)
  (cond ((and (number? exp1) (number? exp2))
         (cond ((= exp1 exp2) 'true)
               (else 'false)))
        (else (error "invalid operation on type"))))

(define (land exp1 exp2)
  (cond ((or (number? exp1) (number? exp2)) (error "invalid operation on type"))
        ((and (equal? exp1 'true) (equal? exp2 'true)) 'true)
        (else 'false)))

(define (lor exp1 exp2)
  (cond ((or (number? exp1) (number? exp2)) (error "invalid operation on type"))
        ((equal? exp1 'true) 'true)
        ((equal? exp2 'true) 'true)
        (else 'false)))

(define (operate op exp1 exp2)
  (cond ((equal? exp1 "undefined") exp1)
        ((equal? exp2 "undefined") exp2)
        ((equal? exp1 'int) "undefined")
        ((equal? exp1 'boolean) "undefined")
        ((equal? '+ op) (add exp1 exp2))
        ((equal? '- op) (sub exp1 exp2))
        ((equal? '/ op) (div exp1 exp2))
        ((equal? '* op) (mult exp1 exp2))
        ((equal? '@ op) (expon exp1 exp2))
        ((equal? '? op) (rem exp1 exp2))
        ((equal? '< op) (lt exp1 exp2))
        ((equal? '> op) (gt exp1 exp2))
        ((equal? '= op) (eq exp1 exp2))
        ((equal? '<= op) (lteq exp1 exp2))
        ((equal? '>= op) (gteq exp1 exp2))
        ((equal? '& op) (land exp1 exp2))
        ((equal? '% op) (lor exp1 exp2))))

;; "-" will be problem; both bi and un -> maybe fixed
(define (is-un-op? op exp2)
  ;(display exp2)
  (or (equal? op '~) (and (equal? op '-) (null? exp2))))

(define (is-bin-op? op)
  (or (equal? '+ op) (equal? '- op) (equal? '/ op) (equal? '* op)
      (equal? '@ op) (equal? '? op) (equal? '< op) (equal? '> op)
      (equal? '= op) (equal? '<= op) (equal? '>= op) (equal? '& op)
      (equal? '% op)))
;; im not convinced these are correct
(define (nott exp1)
  (cond ((number? exp1) (error "invalid operation on type"))
        ((symbol->bool exp1) true)
        (else 'false)))

(define (neg exp1)
  (cond ((number? exp1) (- exp1))
        (else (error "invalid operation on type"))))

(define (unoper op exp1)
  (cond ((equal? exp1 "undefined") exp1)
        ((equal? '~ op) (nott exp1))
        ((equal? '- op) (neg exp1))))

(define (evaluator exp state)
  (cond ((number? exp) exp)
        ((equal? "undefined" exp) exp)
        ((equal? 'true exp) exp)
        ((equal? 'false exp) exp)
        ((symbol? exp) (state-get-value state exp))
        ((is-un-op? (car exp) (cdr(cdr exp))) (unoper (car exp) (evaluator (cadr exp) state)))
        ((is-bin-op? (car exp)) (operate (car exp) (evaluator (cadr exp) state) (evaluator (caddr exp) state)))
        ))

;; INPUT: A PROGRAM
;; A PROGRAM has syntactic structure (program stmt)
;; OUTPUT: THE STATE that results from executing the program
;;         in an empty state.
(define (interpret-program pgm)
  (interpret (program-get-body pgm) (state-create-empty)))


;; INPUT: A PROGRAM
;; A PROGRAM has syntactic structure (program stmt)
;; OUTPUT: THE STATE that results from executing the program
;;         in an empty state.
(define (interpret-program pgm)
  (interpret (program-get-body pgm) (state-create-empty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the INTERPRETER class
;; An INTERPRETER is simply a collection of functions that
;; operates on TYPES, STATES, BINDING, SCOPES and PROGRAMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT: STATEMENT and STATE
;; OUTPUT: The state that results from executing STATEMENT in STATE
(define (interpret stmt state)
  ;(display "stmt: ")(display stmt) (newline) (display "state: ")(display state) (newline)
  (let ((kind (car stmt)))
    (cond ((equal? kind 'block) (interpret-block stmt state))
          ((equal? kind 'declare) (interpret-declaration stmt state))
          ((equal? kind ':=) (interpret-assignment stmt state))
          ((equal? kind 'if) (interpret-if stmt state))
          ((equal? kind 'sprint) (interpret-sprint stmt state))
          ((equal? kind 'while) (interpret-while stmt state))       
          (else (error (string-append "statement expected but saw (" (exp->string stmt) "...) instead."))))))

;block
(define (interpret-block stmt state)
  ;(display "block ") (newline)
  ;(display (null? (cdr stmt)))
  (cond ((or (null? stmt) (null? (cdr stmt))) state)
        ((null? (cadr stmt)) (interpret-block (cdr stmt) state))
        (else (interpret-block (cdr stmt) (interpret (cadr stmt) state)))))
 
;declare put in state as (name type)
(define (interpret-declaration stmt state)
  ;(display "declare ") (newline)
  (state-update (state-add state (caddr stmt)) (caddr stmt) (cadr stmt)))

;assign;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (type-match? name exp state)
  (equal? (type-of(state-get-value state name)) (type-of exp)))


(define (assign-check name exp state)
  (cond ((type-match? name exp state) (state-update state name exp))
        (else (error "assignment to wrong type") state)))

(define (interpret-assignment stmt state) ;;;;;;
  (assign-check (cadr stmt) (evaluator (caddr stmt) state) state))

;while
(define (interpret-while stmt state)
  ;(display "while ") (newline)
  (cond ((symbol->bool (evaluator (cadr stmt) state)) (interpret-while stmt (interpret (caddr stmt) state)));do stuff
        (else state))) ;dont

;if
(define (interpret-if stmt state)
  ;(display "if ") (newline)
  (cond ((symbol->bool (evaluator (cadr stmt) state)) (interpret (caddr stmt) state));do stuff
        ((null? (cadddr stmt)) state)
        (else (interpret (cadddr stmt) state))))

;sprint
(define (interpret-sprint stmt state)
  (cond ((null? (cdr (cdr stmt))) (newline) (display (cadr stmt)) state)
        (else (newline) (display (cadr stmt)) (display (evaluator (caddr stmt) state)) state)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Sample program
(define pgm '(program 
              (block
               (declare int n)
               (declare boolean error)
               (declare int result)   
               (:= error false)
               (:= result 1)
               (block 
                (declare int local)
                (:= n 5)
                (:= local n)
                (while (> local 0)
                       (block
                        (:= result (* result local))
                        (:= local (- local 1)))))
              (sprint "result: " result)
              (if (~ error) (sprint "a") (sprint "b")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define simpgm '(program
                 (block
                  (declare int n)
                  (declare boolean error)
                  (:= n 6)
                  (:= error false)
                  (if (> n 0)
                         (block
                          (:= n (? n 3))
                          (:= error true)
                      )))))

(interpret-program simpgm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;valid oh gosh whyy? ->better to type check dynamiclly 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (aand bool list)
  (cond ((null? list) bool)
         (else (aand (and bool (car list)) (cdr list)))))

(define (valid-var-name name)
  (cond ((or (number? name) (equal? name 'int) (equal? name 'boolean)
            (bool? name) (equal? name 'program) (equal? name 'block) (equal? name 'declare)
            (equal? name 'if) (equal? name 'while) (equal? name 'sprint)) #f)
        (else #t)))

(define (valid-dec pgm)
  (cond ((and (equal? (car pgm) 'declare) (valid-var-name (caddr pgm))))
        (else #f)))
(define (false pgm call)
  (newline)
  ;(display "        false ")
  ;(display call)
  ;(display " ")
  ;(display pgm)
  )

(define (valid-statement pgm)
  ;(newline)
  ;(display (car pgm))
  (let ((kind (car pgm)))
    (cond ((equal? kind 'block) (valid-block pgm))
          ((equal? kind 'if) (valid-if pgm))
          ((equal? kind ':=) (valid-assign pgm))
          ((equal? kind 'sprint) (valid-sprint pgm))
          ((equal? kind 'while) (valid-while pgm))       
          (else (false pgm 'smt) #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (valid-exp pgm)
  (cond ((number? pgm) #t)
        ((bool? pgm) #t)
        ((symbol? pgm) #t)
        ((not (list? pgm)) (false pgm) #f)
        ((is-un-op? (car pgm) (cdr(cdr pgm))) (valid-exp (cadr pgm)))
        ;((null? (cdr(cdr(pgm)))) #f)
        ((is-bin-op? (car pgm)) (and (valid-exp (cadr pgm)) (valid-exp (caddr pgm))))
        (else (false pgm 'exp) #f)))

(define (valid-if pgm) ;; cadr or car
  (cond ((and (valid-exp (cadr pgm)) (valid-statement (caddr pgm)))
         (cond ((null? (cdr(cdr(cdr pgm)))) #t)
               (else (valid-statement (cadddr pgm)))))
        (else (false pgm 'if) #f)))

(define (valid-sprint pgm)
  (cond ((string? (cadr pgm))
         (cond ((null? (cdr(cdr pgm))) #t)
               (else (valid-exp (caddr pgm)))))
        (else (false pgm 'sprint) #f)))

(define (valid-while pgm)
  (cond ((valid-exp (cadr pgm)) (valid-statement (caddr pgm)))
        (else (false pgm 'while) #f)))

(define (valid-assign pgm)
  (cond ((symbol? (cadr pgm)) (valid-exp (caddr pgm)))
        (else (false pgm 'ass) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (valid-block pgm)
  (cond ((equal? (car pgm) 'block) ;(valid-dec (cadr pgm) typemap))
         (and (aand #t (map (lambda (x) (valid-dec x)) (block-get-declarations pgm)))
         (aand #t (map (lambda (x)(valid-statement x)) (block-get-body pgm))))) 
        (else #f)))
               
;return bool
(define (valid-pgm pgm)
  (cond ((equal? (car pgm) 'program) (valid-block (cadr pgm)))
        (else #f)))

(define (is-program-valid? pgm)
  (valid-pgm pgm))

;(is-program-valid? simpgm)