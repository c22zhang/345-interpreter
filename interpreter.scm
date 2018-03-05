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
;default return continuation
(define default-continuation (lambda (v) v))

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
	 ((var-declaration? (current-expression parse-tree)) (interpret-state (next-expressions parse-tree) (M-state-declare (current-expression parse-tree) state default-continuation) return))
	 ((assignment? (current-expression parse-tree)) (interpret-state (next-expressions parse-tree) (M-state-assign (current-expression parse-tree) state default-continuation) return))
	 ((return? (current-expression parse-tree)) (return (get-return-value (current-expression parse-tree) state)))
         ((if-statement? (current-expression parse-tree)) (interpret-state (next-expressions parse-tree) (M-state-if (current-expression parse-tree) state default-continuation) return))
	 ((while-statement? (current-expression parse-tree)) (interpret-state (next-expressions parse-tree) (M-state-while (current-expression parse-tree) state default-continuation) return))
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

(define try?
  (lambda (expression)
    (cond
      ((null? expression) #f)
      ((eq? 'try (keyword expression)) #t)
      (else #f))))

(define catch?
  (lambda (expression)
    (cond
      ((null? expression) #f)
      ((eq? 'catch (keyword expression)) #t)
      (else #f))))

(define break?
  (lambda (expression)
    (cond
      ((null? expression) #f)
      ((eq? 'break (keyword expression)) #t)
      (else #f))))

(define finally?
  (lambda (expression)
    (cond
      ((null? expression) #f)
      ((eq? 'finally (keyword expression)) #t)
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

(define block?
  (lambda (expr)
	(cond
	 ((null? expr) #f)
	 ((eq? 'begin (keyword expr)) #t)
	 (else #f))))

(define startBlock?
  (lambda (expr)
	(cond
	 ((null? expr) #f)
	 ((eq? '= (keyword expr)) #t)
	 (else #f))))
					
;stores the variable in the state
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

;;add layer to state
;;param newLayer is a state in form '((a b) (1 2))
(define addLayer
  (lambda (newState state)
    (cond
      ((atom? (caar state)) (cons newState (cons state '())))
      (else (cons newState state )))))

;;removes topmost layer from state
;;returns false if state is empty
(define removeTopLayer
  (lambda (state)
    (if (null? state)
        #f
        (cdr state))))

;;declare variable, or initialize variable
(define M-state-declare
  (lambda (e state return-cont)
	(cond
	 ((null? (cddr e)) (return-cont (store-variable-in-state (varName e) state return-cont)))
	 (else (return-cont (store-variable-value-in-state (varName e) (varValue e) state return-cont))))))

;stores the variable in the state
(define store-variable-in-state
  (lambda (var state return-cont)
    (if (contains? var (state-vars (top-state state)))
	(return-cont state)
        (return-cont (cons (append-var (top-state state) var return-cont) (cdr state))))))

(define top-state car)

;stores the value associated with the variable in the state
(define store-variable-value-in-state
  (lambda (var value state return-cont)
	(cond
	 ((eq? (getVariableValue state var return-cont) #f) (return-cont (cons (add-value-to-variable var value (top-state state) return-cont) (cdr state))))
	 ((eq? (getVariableValue state var default-continuation) value) (return-cont state))
         ((eq? (contains-helper? (car (top-state state)) var) #f) (cons (top-state state) (store-variable-value-in-state var value (cdr state) return-cont)))
	 (else (cons (cons var (state-vars (top-state state))) (return-cont (cons (append (cons (m-value-expr value state) '()) (state-vals (top-state state))) '())))))))

;;helper method for appending item to end of list
(define append-var
  (lambda (state var return-cont)
    (cond
	 ((null? list) (return-cont (cons val '())))
	 (else (return-cont (cons (append (car state) (cons var '())) (cdr state)))))))

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

(define startBlock?
  (lambda (e)
    (cond
      ((null? e) #f)
      ((eq? 'begin (operator e)) #t)
      (else #f))))

(define try?
  (lambda (e)
    (cond
      ((null? e) #f)
      ((eq? 'try (operator e)) #t)
      (else #f))))

;;assign value to variable if it exists
(define M-state-assign
  (lambda (e state return-cont)
    (cond
	 ((eq?(contains? (varName e) (state-vars state)) #f) (return-cont (error "variable not declared"))) 
	 ((eq?(getVariableValue state (varName e) return-cont) #f) (add-value-to-variable (varName e) (varValue e) state return-cont))
	 ((eq?(eq?(getVariableValue state (varName e) return-cont) (varValue e)) #f) (modifyVariableValue (varName e) (caddr e) state return-cont)))))

;;helper function for add-value-to-variable
(define remove-variable-from-list
  (lambda (var lis return-cont)
    (cond
	 ((null? lis) (return-cont '()))
	 ((eq? (eq? (car lis) var) #f) (remove-variable-from-list var (cdr lis) (lambda (v) (return-cont (cons (car lis) v)))))
	  ;(cons (car lis) (remove-variable-from-list var (cdr lis))))
	 (else (return-cont (cdr lis))))))

;;adds value to variable if it already exists but uninitialized in state
(define add-value-to-variable
  (lambda (var value state return-cont)
    ;(cond   
      ;((contains-helper? (state-vars state) var)
      (cons (cons var (remove-variable-from-list var (state-vars state) return-cont))
                                                      (cons (append (cons (m-value-expr value state) '()) (state-vals state)) '()))))
      ;(else (cons stat
  
;;remove the variable from the list, then readd value with variable

;;helper method that allows variables to be revalued 
(define modifyVariableValue
  (lambda (var val state return-cont)
    (return-cont (cons (cons var (remove-variable-from-list var (state-vars state) return-cont))
          (cons (cons (m-value-expr val state) (modifyStateVals var (state-vars state) (state-vals state) return-cont)) '())))))

;;returns stateVals with deleted variable value
(define modifyStateVals
  (lambda (var stateVars stateVals return-cont)
    (cond
	 ((null? stateVars) (return-cont stateVals))
	 ((eq? (car stateVars) var) (return-cont (cdr stateVals)))
	 (else (modifyStateVals var (cdr stateVars) (cdr stateVals) (lambda (v) (return-cont (cons (car stateVals) v))))))))
          ;(cons (car stateVals) (modifyStateVals var (cdr stateVars) (cdr stateVals)))))))

(define layerVars caar)
(define nextLayer cdr)
(define layer car)

;;checks if varList contains a var, pass in state-vars
(define contains-helper?
  (lambda (varList var)
	(cond
	 ((null? varList) #f)
	 ((eq? (car varList) var) #t)
	 (else (contains-helper? (cdr varList) var)))))

;;takes state as input, returns value of variable in state
;;returns error if var doesn't exist in state, #f if not inited
(define getVariableValue
  (lambda (state var return)
	(cond
	 ((null? state) (return #f))
	 ((list? (layerVars state))
	  (if (contains-helper? (layerVars state) var)
		  (return (getVariableValue-cps (layer state) var return))
		  (getVariableValue (nextLayer state) var return)))
	 (else (getVariableValue-cps state var return)))))
	
;;wrapper for getVariableValue that reads in state
;;if it returns z, that means variable not found in this state
(define getVariableValue-cps
  (lambda (state var return)
	(cond
	 ((null? state) (return 'z))
	 ((null? (state-vals state)) (return #f))
	 ((eq? (car (state-vars state)) var) (return (car (state-vals state))))
	 (else (getVariableValue-cps (cons (cdr (state-vars state)) (cons (cdr (state-vals state)) '())) var return)))))
	
;;determines proper method to call based on statement
(define M-state-stmt
  (lambda (e state return-cont)
	(cond
	 ((startBlock? e) (M-state-block (cdr e) state return-cont))
	 ((arithmetic-operator? (car e)) (M-state-assign e state return-cont))
	 ((var-declaration? e) (M-state-declare e state return-cont))
	 ((assignment? e) (M-state-assign e state return-cont) )
	 ((return? e) (return-cont (get-return-value e state)))
	 ((if-statement? e) (M-state-if-else e state))
	 ((while-statement? e) (M-state-while e state)))))

;;main if statement controller
(define M-state-if
  (lambda (e state return-cont)
	(if(eq?(null? (cdddr e)) #f)
	   (if-else e state return-cont) 
	   (if-only e state return-cont))))

;;helper method for if else statements
(define if-else
  (lambda (e state return-cont)
	(if(m-value-boolean (cond-stmt e) state)
	   (M-state-stmt (then-stmt e) state return-cont)
	   (M-state-stmt (else-stmt e) state return-cont))))


;;helper method for if only statements
(define if-only
  (lambda (e state return-cont)
    (if (m-value-boolean (cond-stmt e) state)
	(M-state-stmt (then-stmt e) state return-cont)
        (return-cont state))))

(define block-body car)
(define next-stmt cdr)
(define blank-state '(()()))

;;M-state controller for block statements
;;takes in expression -begin statement
(define M-state-block
  (lambda (e state return)
	(return (addLayer (M-state-block-ctrl e blank-state default-continuation) state))))

(define M-state-block-ctrl
  (lambda (e state return)
	(if (null? e)
		(return state)
		(M-state-block (next-stmt e) (M-state-stmt (block-body e) state return) return))))
	  
;modify the state based on the expression/condition of a while loop
(define M-state-while
  (lambda (e state return-cont)
    (if(m-value-boolean (cond-stmt e) state)
       (M-state-while e (M-state-stmt (then-stmt e) state return-cont) return-cont)
	   (return-cont state))))

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
      ((atom? e) (getVariableValue state e default-continuation))
      ((list? (car e)) (m-value-expr (car e) state))
      ((and (not (boolean-operator? (operator e))) (or (or (arithmetic-operator? (operator e)) (number? (operator e))) (atom? (operator e))) (m-value-int e state)))
      (else (boolean-wrapper (m-value-boolean e state))))))

;evaluates an arithmetic expression and returns an integer value
(define m-value-int
  (lambda (e state)
    (if (and (eq? (list? e)#f) (boolean-operator? e))
          (if (and (variable? e) (eq?(getVariableValue state e default-continuation)#f))
              (error "Value undeclared")))
    (cond
	 ((number? e) e)
         ((and (atom? e) (eq? (getVariableValue state e default-continuation) #f)) (error "no value for variable"))
	 ((atom? e) (getVariableValue state e default-continuation))
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

