;;Chris Zhang
;;Jeremy Chan
;;EECS 345 Interpreter project, part 1
;;written in Scheme/Pretty Big

(load "simpleParser.scm")

;bindings for specific keywords
(define keyword car)
(define current-expression car)
(define next-expressions cdr)
(define state-vars car)
(define state-vals cadr)
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define varName cadr)
(define varValue cddr)
(define cond-stmt cadr)
(define then-stmt caddr)
(define else-stmt cadddr)

;binding for initial state
(define init-state '(() ()))

;top level interpreter that takes in the file name
(define interpret
  (lambda (filename)
    (call/cc
     (lambda (return)
       (interpret-state (parser filename) init-state return)))))

;interpreter to interpret all individual statements returned by the parser
(define interpret-state
  (lambda (parse-tree state return)
    (cond
	 ((null? parse-tree) state)
	 ((var-declaration? (current-expression parse-tree)) (interpret-state (next-expressions parse-tree) (M-state-declare (current-expression parse-tree) state) return))
	 ((assignment? (current-expression parse-tree)) (interpret-state (next-expressions parse-tree) (M-state-assign (current-expression parse-tree) state) return))
	 ((return? (current-expression parse-tree)) (return (get-return-value (current-expression parse-tree) state)))
         ((if-statement? (current-expression parse-tree)) (interpret-state (next-expressions parse-tree) (M-state-if (current-expression parse-tree) state) return))
	 ((while-statement? (current-expression parse-tree)) (interpret-state (next-expressions parse-tree) (M-state-while (current-expression parse-tree) state) return))
	 (else (interpret-state (next-expressions parse-tree) state return)))))

;finds the return value associated with a return statement
(define get-return-value
  (lambda (expression state)
    (if (return?  expression)
		(m-value-expr (cadr expression) state)
		(error 'badop "no return value"))))

;returns true if an expression is a var-declaration
(define var-declaration?
  (lambda (expression)
    (cond
	 ((null? expression) #f)
	 ((eq? 'var (keyword expression)) #t)
	 (else #f))))

;returns true if an expression is an assignment statement
(define assignment?
  (lambda (expression)
    (cond
	 ((null? expression) #f)
	 ((eq? '= (keyword expression)) #t)
	 (else #f))))

;returns true if an expression is a return statement
(define return?
  (lambda (expression)
    (cond
	 ((null? expression) #f)
	 ((eq? 'return (keyword expression)) #t)
	 (else #f))))

;returns true if an expression is an if statement
(define if-statement?
  (lambda (expression)
	(cond
	 ((null? expression) #f)
	 ((eq? 'if (keyword expression)) #t)
	 (else #f))))

;returns true if an expression is a while statement
(define while-statement?
  (lambda (expression)
	(cond
	 ((null? expression) #f)
	 ((eq? 'while (keyword expression)) #t)
	 (else #f))))

;returns true if an expression is an arithmetic operator
(define arithmetic-operator?
  (lambda (op)
    (cond
	 ((eq? '+ op) #t)
	 ((eq? '- op) #t)
	 ((eq? '* op) #t)
	 ((eq? '/ op) #t)
	 ((eq? '% op) #t)
	 (else #f))))

;returns true if an expression is an atom
(define atom?
  (lambda (a)
    (cond
	 ((list? a) #f)
	 ((null? a) #f)
	 (else #t))))

;returns true if the state-var-list contains var
(define contains?
  (lambda (var state-var-list)
    (cond
	 ((null? state-var-list) #f)
	 ((eq? var (car state-var-list)) #t)
	 (else (contains? var (cdr state-var-list))))))

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
    (cons (cons var (remove-variable-from-list var (state-vars state))) (cons (append (cons (m-value-expr value state) '()) (state-vals state)) '()))))
;;remove the variable from the list, then readd value with variable

;;takes state as input, returns matching variable value, assumes everything lined up
(define getVariableValue
  (lambda (state var)
    (cond
	 ((null? state) (error "Variable not declared"))
	 ((null? (state-vals state)) #f) ;;variable is initialized but not declared
	 ((eq? (car (state-vars state)) var) (car (state-vals state)))
         ((and (list? var) (eq? (car (state-vars state)) (car var))) (car (state-vals state)))
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
    (cons (cons var (remove-variable-from-list var (state-vars state))) (cons (cons (m-value-expr val state) (modifyStateVals var (state-vars state) (state-vals state))) '()))))

;;declare variable, or initialize variable
(define M-state-declare
  (lambda (e state return-cont)
	(cond
	 ((null? (cddr e)) (return-cont (store-variable-in-state (varName e) state return-cont)))
	 (else (store-variable-value-in-state (varName e) (varValue e) state)))))

;stores the variable in the state
(define store-variable-in-state
  (lambda (var state return-cont)
    (if (contains? var (state-vars state))
	(return-cont state)
        (append-var state var))))

;stores the value associated with the variable in the sate
(define store-variable-value-in-state
  (lambda (var value state)
	(cond
	 ((eq? getVariableValue #f)(add-value-to-variable var value state))
	 ((eq? (getVariableValue state var) value) state)
	 (else (cons (cons var (state-vars state)) (cons (append (cons (m-value-expr value state) '()) (state-vals state)) '()))))))

;;helper method for appending item to end of list
(define append-var
  (lambda (state var return-cont)
    (cond
	 ((null? list) (cons val '()))
	 (else (cons (append (car state) (cons var '())) (cdr state))))))

;returns true if the variable exists in the state
(define var-exist-in-state?
  (lambda (varName stateVars)
    (cond
      ((null? stateVars) #f)
      ((eq?(car stateVars) (car varName)) #t)
      (else (var-exist-in-state? varName (cdr stateVars))))))

;returns true if an atom is a variable
(define variable?
  (lambda (varName)
    (if (and(eq?(number? varName)#f) (eq?(boolean-operator?  varName)#f))
        #t
        #f)))

;returns true if the input is an expression
(define expression?
  (lambda (e)
    (cond
      ((null? e) #f)
      ((atom? e) #t)
      ((number? e) #t)
      ((and (list? e) (or (boolean-operator? (operator e)) (arithmetic-operator? (operator e)))) #t)
      (else #f))))
     
;;assign value to variable if it exists
(define M-state-assign
  (lambda (e state)
    (cond
	 ((eq?(contains? (varName e) (state-vars state)) #f) (error "variable not declared")) 
	 ((eq?(getVariableValue state (varName e))#f) (add-value-to-variable (varName e) (varValue e) state))
	 ((eq?(eq?(getVariableValue state (varName e)) (varValue e))#f) (modifyVariableValue (varName e) (caddr e) state)))))

;;determines proper method to call based on statement
(define M-state-stmt
  (lambda (e state)
	(cond
	 ((arithmetic-operator? (car e)) (M-state-assign e state))
	 ((var-declaration? e) (M-state-declare e state))
	 ((assignment? e) (M-state-assign e state) )
	 ((return? e) (get-return-value e state))
	 ((if-statement? e) (M-state-if-else e state))
	 ((while-statement? e) (M-state-while e state)))))

;;main if statement controller
(define M-state-if
  (lambda (e state)
	(if(eq?(null? (cdddr e)) #f)
	   (if-else e state) 
	   (if-only e state))))

;;helper method for if else statements
(define if-else
  (lambda (e state)
	(if(m-value-boolean (cond-stmt e) state)
	   (M-state-stmt (then-stmt e) state)
	   (M-state-stmt (else-stmt e) state))))


;;helper method for if only statements
(define if-only
  (lambda (e state)
    (if (m-value-boolean (cond-stmt e) state)
	(M-state-stmt (then-stmt e) state)
        state)))

;modify the state based on the expression/condition of a while loop
(define M-state-while
  (lambda (e state)
    (if(m-value-boolean (cond-stmt e) state)
       (M-state-while e (M-state-stmt (then-tmt e) state))
	   state)))

;returns true if an operator is a boolean operator
(define boolean-operator?
  (lambda (exp)
    (cond
      ((eq? 'true exp) #t)
      ((eq? 'false exp) #t)
      ((eq? '< exp) #t)
      ((eq? '> exp) #t)
      ((eq? '>= exp) #t)
      ((eq? '<= exp) #t)
      ((eq? '&& exp) #t)
      ((eq? '|| exp) #t)
      ((eq? '== exp) #t)
      ((eq? '!= exp) #t)
      ((eq? '! exp) #t)
      (else #f))))

;evaluates an boolean/arithmetic expression
(define m-value-expr
  (lambda (e state)
    (cond
      ((null? e) (error "empty expression"))
      ((number? e) (m-value-int e state))
      ((atom? e) (getVariableValue state e))
      ((list? (car e)) (m-value-expr (car e) state))
      ((and (not (boolean-operator? (operator e))) (or (or (arithmetic-operator? (operator e)) (number? (operator e))) (atom? (operator e))) (m-value-int e state)))
      (else (boolean-wrapper (m-value-boolean e state))))))

;evaluates an arithmetic expression and returns an integer value
(define m-value-int
  (lambda (e state)
    (if (and (eq? (list? e)#f) (boolean-operator? e))
          (if (and (variable? e) (eq?(getVariableValue state e)#f))
              (error "Value undeclared")))
    (cond
	 ((number? e) e)
         ((and (atom? e) (eq? (getVariableValue state e) #f)) (error "no value for variable"))
	 ((atom? e) (getVariableValue state e))
	 ((number? (operator e)) (m-value-int (operator e) state))
	 ((eq? '+ (operator e)) (+ (m-value-int (operand1 e) state) (m-value-int(operand2 e) state)))
         ((and (eq? '- (operator e)) (null? (cddr e))) (* -1 (m-value-int (operand1 e) state)))
	 ((eq? '- (operator e)) (- (m-value-int (operand1 e) state) (m-value-int(operand2 e) state)))
	 ((eq? '* (operator e)) (* (m-value-int (operand1 e) state) (m-value-int(operand2 e) state)))
	 ((eq? '/ (operator e)) (quotient (m-value-int (operand1 e) state) (m-value-int(operand2 e) state)))
	 ((eq? '% (operator e)) (remainder (m-value-int (operand1 e) state) (m-value-int(operand2 e) state)))
         ((atom? (operator e)) (m-value-int (operator e) state))
	 (else (error 'badop "undefined operator")))))

;;expression evaluator for boolean/comparison operators/expressions
(define m-value-boolean
  (lambda (e state)
	(cond
	 ((or (arithmetic-operator? e) (number? e)) (m-value-int e state))
	 ((eq? 'true e) #t)
	 ((eq? 'false e) #f)
	 ((atom? e) (m-value-int e state))
	 ((or (arithmetic-operator? (operator e)) (number? (operator e))) (m-value-int e state))
         ((eq? '! (operator e)) (not (m-value-boolean(operand1 e) state)))
	 ((eq? '> (operator e)) (> (m-value-boolean(operand1 e) state) (m-value-boolean(operand2 e) state)))
	 ((eq? '>= (operator e)) (>= (m-value-boolean(operand1 e) state) (m-value-boolean(operand2 e) state)))
	 ((eq? '< (operator e)) (< (m-value-boolean(operand1 e) state) (m-value-boolean(operand2 e) state)))
	 ((eq? '<= (operator e)) (<= (m-value-boolean(operand1 e) state) (m-value-boolean(operand2 e) state)))
	 ((eq? '== (operator e)) (eq? (m-value-boolean(operand1 e) state) (m-value-boolean(operand2 e) state)))
	 ((eq? '&& (operator e)) (and (m-value-boolean(operand1 e) state) (m-value-boolean(operand2 e) state)))
	 ((eq? '|| (operator e)) (or (m-value-boolean(operand1 e) state) (m-value-boolean(operand2 e) state)))
	 ((eq? '!= (operator e)) (not (eq? (m-value-boolean(operand1 e) state) (m-value-boolean(operand2 e) state))))
	 (else (error 'badop "undefined operator")))))

;true/false wrapper for #t/#f
(define boolean-wrapper
  (lambda (bool)
    (cond
	 ((eq? #t bool) 'true)
	 ((eq? #f bool) 'false)
	 (else error "expected boolean"))))

