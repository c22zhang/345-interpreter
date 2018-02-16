;;Chris Zhang
;;Jeremy Chan
;;EECS 345 Interpreter project, part 1

(load "simpleParser.scm")

(define keyword car)
(define state-vars car)
(define state-vals cadr)
(define init-state
  (lambda ()
    '(() ())))

(define interpret
  (lambda (filename)
    (parser filename)))

(define interpret-state
  (lambda(parse-tree state)
    (cond
      ((null? parse-tree) state)
      ((var-declaration? (car parse-tree)) (interpret-state (cdr parse-tree) (M_state_declare (car parse-tree )))))))

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

(define if-statement?
  (lambda (expression)
	(cond
	 ((null? expression) #f)
	 ((eq? 'if (keyword expression)) #t)
	 (else #f))))

(define while-statement?
  (lambda (expression)
	(cond
	 ((null? expression) #f)
	 ((eq? 'while (keyword expression)) #t)
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
	 ((list? a) #f)
	 ((null? a) #f)
	 (else #t))))

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
        (append-var state var))))

;;helper method for appending item to end of list
(define append-var
  (lambda (state var)
    (cond
      ((null? list) (cons val '()))
      (else (cons (append (car state) (cons var '())) (cdr state))))))

(define store-variable-value-in-state
  (lambda (var value state)
	(cond
	 ((eq? getVariableValue #f)(add-value-to-variable var value state))
	 ((eq? (getVariableValue state var) value) state)
	 (else (cons (cons var (state-vars state)) (cons value (state-vals state)))))))

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
    (cons (cons var (remove-variable-from-list var (state-vars state))) (cons value (state-vals state)))))
;;remove the variable from the list, then readd value with variable

;;takes state as input, returns matching variable value, assumes everything lined up
(define getVariableValue
  (lambda (state var)
    (cond
	 ((null? state) (error "Variable not declared"))
	 ((null? (state-vals state)) #f) ;;variable is initialized but not declared
	 ((eq? (car (state-vars state)) var) (car (state-vals state)))
	 (else (getVariableValue (cons (cdr (state-vars state)) (cons (cdr (state-vals state)) '())) var)))))

;;returns stateVals with deleted variable value
(define modifyStateVals
  (lambda (var stateVars stateVals)
    (cond
	 ((null? stateVars) stateVals)
	 ((eq? (car stateVars) var) (cdr stateVals))
	 (else (cons (car stateVals) (modifyStateVals var (cdr stateVars) (cdr stateVals)))))))

;;helper method that allows variables to be revalued 
(define modifyVariableValue
  (lambda (var val state)
    (cons (cons var (remove-variable-from-list var (state-vars state))) (cons(cons val (modifyStateVals var (state-vars state) (state-vals state))) '()))))

;;declare variable, or initialize variable
(define M_state_declare
  (lambda (e state)
	(cond
	 ((null? (cddr e)) (store-variable-in-state (varName e) state))
	 (else (store-variable-value-in-state (varName e) (varValue e) state)))))

;;assign value to variable if it exists
(define M_state_assign
  (lambda (e state)
    (cond
      ((eq?(contains? (varName e) (state-vars state)) #f) (error "variable not declared"))
      ((eq?(getVariableValue state (varName e))#f) (add-value-to-variable (varName e) (varValue e) state))
      ((eq?(eq?(getVariableValue state (varName e)) (varValue e))#f) (modifyVariableValue (varName e) (varValue e) state)))))

;;determines proper method to call based on statement
(define M_state_stmt
  (lambda (e state)
	(cond
	 ((var-declaration? e) (M_state_declare e state))
	 ((assignment? e) (M_state_assign e state) )
	 ((return? e) (M_state_return e state))
	 ((if-statement? e) (M_state_if_else e state))
	 ((while-statement? e) (M_state_while e state)))))
  
(define cond_stmt cadr)
(define then_stmt caddr)
(define else_stmt cadddr)

;;main if statement controller
(define M_state_if
  (lambda (e state)
	(if(eq?(null? (cdddr e)) #f)
	   (if_else e state)
	   (if_only e state))))

;;helper method for if else statements
(define if_else
  (lambda (e state)
	(if(m.value.boolean (cond_stmt e) state)
	   (M_state_stmt (then_stmt e) state)
	   (M_state_stmt (else_stmt e) state))))


;;helper method for if only statements
(define if_only
  (lambda (e state)
	(if (m.value.boolean (cond_stmt e) state)
		(M_state_stmt (then_stmt e) state))))

(define m.value.int
  (lambda (e state)
    (cond
	 ((number? e) e)
	 ((atom? e) (getVariableValue state e))
	 ((eq? '+ (operator e)) (+ (m.value.int (operand1 e) state) (m.value.int(operand2 e) state)))
	 ((eq? '- (operator e)) (- (m.value.int (operand1 e) state) (m.value.int(operand2 e) state)))
	 ((eq? '* (operator e)) (* (m.value.int (operand1 e) state) (m.value.int(operand2 e) state)))
	 ((eq? '/ (operator e)) (quotient (m.value.int (operand1 e) state) (m.value.int(operand2 e) state)))
	 ((eq? '% (operator e)) (remainder (m.value.int (operand1 e) state) (m.value.int(operand2 e) state)))
	 (else (error 'badop "undefined operator")))))

										;expression evaluator for boolean/comparison operators/expressions
(define m.value.boolean
  (lambda (e state)
	(cond
	 ((or (arithmetic-operator? e) (number? e)) (m.value.int e state))
	 ((atom? e) (m.value.int e state))
	 ((or (arithmetic-operator? (operator e)) (number? (operator e))) (m.value.int e state))
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
(define varValue cddr)

