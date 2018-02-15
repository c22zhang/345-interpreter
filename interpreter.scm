;;Chris Zhang
;;Jeremy Chan
;;EECS 345 Interpreter project, part 1

(load "simpleParser.scm")

(define keyword car)
(define state-vars car)
(define state-vals cadr)

(define var-declaration?
  (lambda (expression)
    (cond
	 ((null? expression) #f)
	 ((eq? 'var (keyword expression)) #t)
	 (else #f))))

(define assignment?
  (lambda (expression)
    (cond
	 ((null? expression) #f)
	 ((eq? '= (keyword expression)) #t)
	 (else #f))))

(define return?
  (lambda (expression)
    (cond
	 ((null? expression) #f)
	 ((eq? 'return (keyword expression)) #t)
	 (else #f))))

(define arithmetic-operator?
  (lambda (op)
    (cond
	 ((eq? '+ op) #t)
	 ((eq? '- op) #t)
	 ((eq? '* op) #t)
	 ((eq? '/ op) #t)
	 ((eq? '% op) #t)
	 (else #f))))

(define atom?
  (lambda (a)
    (cond
	 (not (or (pair? a) (null? a) ) #t)
	 (else #f))))

(define contains?
  (lambda (var state-var-list)
    (cond
	 ((null? state-var-list) #f)
	 ((eq? var (car state-var-list)) #t)
	 (else (contains? var (cdr state-var-list))))))

(define store-variable-in-state
  (lambda (var state)
    (if (contains? var (state-vars state))
		state
        (appendList (state-vars state) var))))

;;helper method for appending item to end of list
(define appendList
  (lambda (list val)
    (cond
      ((null? list) (cons val '()))
(else (cons (car list) (appendList (cdr list) val))))))

(define store-variable-value-in-state
  (lambda (var value state)
	(cond
         ((eq? getVariableValue #f)(add-value-to-variable var value state))
	 ((eq? (getVariableValue state var) value) state)
	 (else (cons (cons var (state-vars state)) (cons (cons value (state-vals state)) '()))))))

;;helper function for add-value-to-variable
(define remove-variable-from-list
  (lambda (var lis)
    (cond
    ((null? lis) '())
    ((eq? (eq? (car lis) var) #f)
     (cons (car lis) (remove-variable-from-list var (cdr lis))))
    (else (cdr lis)))))
    
;;adds value to variable if it already exists but uninitialized in state
(define add-value-to-variable
  (lambda (var value state)
    (cons (cons var (remove-variable-from-list var (state-vars state))) (cons(cons value (state-vals state)) '()))))
      ;;remove the variable from the list, then readd value with variable
    
;;takes state as input, returns matching variable value, assumes everything lined up
(define getVariableValue
  (lambda (state var)
    (cond
	 ((null? state) (error "Variable not declared"))
	 ((null? (state-vals state)) #f) ;;variable is initialized but not declared
	 ((eq? (car (state-vars state)) var) (car (state-vals state)))
	 (else (getVariableValue (cons (cdr (state-vars state)) (cons (cdr (state-vals state)) '())) var)))))

;;declare variable, or initialize variable
(define M_state_declare
  (lambda (e state)
	(cond
	 ((null? (varValue e)) (store-variable-in-state (varName e) state))
	 (else (store-variable-value-in-state (varName e) (varValue e) state)))))

;;assign value to variable if it exists
(define M_state_assign
  (lambda (e state)
    (cond
      ((eq?(contains? (varName e) (state-vars state)) #f) (error "variable not declared"))
      ;;what to do if duplicate value?->remove variable and value from state
      ((eq?(getVariableValue state (varName e))#f) (add-value-to-variable (varName e) (varValue e) state)))))
   
(define m.value.int
  (lambda (e state)
	(cond
	 ((number? e) e) 
	 ((eq? '+ (operator e)) (+ (m.value.int (operand1 e) state) (m.value.int(operand2 e) state)))
	 ((eq? '- (operator e)) (- (m.value.int (operand1 e) state) (m.value.int(operand2 e) state)))
	 ((eq? '* (operator e)) (* (m.value.int (operand1 e) state) (m.value.int(operand2 e) state)))
	 ((eq? '/ (operator e)) (quotient (m.value.int (operand1 e) state) (m.value.int(operand2 e) state)))
	 ((eq? '% (operator e)) (remainder (m.value.int (operand1 e) state) (m.value.int(operand2 e) state)))
	 (else (error 'badop "undefined operator")))))

(define m.value.boolean
  (lambda (e state)
	(cond
	 ((or (arithmetic-operator? e) (number? e)) (m.value.int e))
	 ((eq? 'true e) #t)
	 ((eq? 'false e) #f)
	 ((eq? '> (operator e)) (> (m.value.boolean(operand1 e) state) (m.value.boolean(operand2 e) state)))
	 ((eq? '>= (operator e)) (>= (m.value.boolean(operand1 e) state) (m.value.boolean(operand2 e) state)))
	 ((eq? '< (operator e)) (< (m.value.boolean(operand1 e) state) (m.value.boolean(operand2 e) state)))
	 ((eq? '<= (operator e)) (<= (m.value.boolean(operand1 e) state) (m.value.boolean(operand2 e) state)))
	 ((eq? '== (operator e)) (eq? (m.value.boolean(operand1 e) state) (m.value.boolean(operand2 e) state)))
	 ((eq? '&& (operator e)) (and (m.value.boolean(operand1 e) state) (m.value.boolean(operand2 e) state)))
	 ((eq? '|| (operator e)) (or (m.value.boolean(operand1 e) state) (m.value.boolean(operand2 e) state)))
	 ((eq? '!= (operator e)) (not (eq? (m.value.boolean(operand1 e) state) (m.value.boolean(operand2 e) state))))
	 (else (error 'badop "undefined operator")))))

(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define varName cadr)
(define varValue caddr)
