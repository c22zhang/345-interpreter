; Chris Zhang
; Jeremy Chan
; EECS 345
; Interpreter Part 3

; If you are using racket instead of scheme, uncomment these two lines, comment the (load "simpleParser.scm") and uncomment the (require "simpleParser.scm")
; #lang racket
; (require "simpleParser.scm")
(load "classParser.scm")


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file)
    (scheme->language
     (call/cc
      (lambda (return)
        ;REPLACE GLOBAL LEVEL PARSE
        (multi-class-level-parse (parser file) (newenvironment) (lambda (v env) (myerror "Uncaught exception thrown"))))))))
      ;;  (run-main (multi-class-level-parse (parser file) (newenvironment) (lambda (v env) (myerror "Uncaught exception thrown"))) return
            ;;                      (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                ;;                  (lambda (v env) (myerror "Uncaught exception thrown"))))))))

(define class-name cadr)
(define class-extension caddr)
(define class-body cadddr)

(define multi-class-level-parse
  (lambda (class-statement-list environment throw)
    (cond
      ((null? class-statement-list) environment)
      (else (multi-class-level-parse (remaining-statements class-statement-list)
                                     (generate-class-closure (individual-statement class-statement-list) environment throw) throw)))))

; generates a class closure for a single class
(define generate-class-closure
  (lambda (class-statement environment throw)
    (insert (class-name class-statement)
            (cons (class-extension class-statement) (class-level-parse (class-body class-statement) (newenvironment) throw)) environment)))

(define class_Name cadr)

(define generate-instance-closure
  (lambda (statement environment throw)
    (find-class-closure (class_Name statement) environment)))

;;helper method that returns list of class closures from environment
;;may need to be changed later depending on where in env, class closure is
(define class-closure-list
  (lambda (environment)
    (car environment)))

(define get-class-closure-names
  (lambda (environment)
    (car (class-closure-list environment))))

(define get-class-closure-vals
  (lambda (environment)
    (cdr (class-closure-list environment))))

(define find-class-closure
  (lambda (className environment)
    (lookup-class-closure className (get-class-closure-names environment) (get-class-closure-vals environment))))

;;returns the corresponding class closure 
(define lookup-class-closure
  (lambda (className namesLis valsLis)
    (cond
      ((null? namesLis) (myerror "Class does not exist:" className))
      ((eq? className (car namesLis)) (car valsLis))
      (else (lookup-class-closure className (cdr namesLis) (cdr valsLis))))))
                    
; Does the first outer level parse of global variables and functions
(define class-level-parse
  (lambda (statement-list environment throw)
    (cond
      ((null? statement-list) environment)
      ((eq? 'var (statement-type (individual-statement statement-list)))
       (class-level-parse (remaining-statements statement-list) (interpret-declare (individual-statement statement-list) environment throw) throw))
      ((or (eq? 'function (statement-type (individual-statement statement-list)))
           (eq? 'static-function (statement-type (individual-statement statement-list))))
       (class-level-parse (remaining-statements statement-list) (insert-function (individual-statement statement-list) environment throw) throw))
      (else (myerror "Unsupported top-level statement: " (statement-type statement))))))

; interprets a list of statements.  The environment from each statement is used for the next ones.
; statement-list - what the parser returns
; environment - state '((()()))
; return - return call/cc
; break - break call/cc or error
; continue - continue call/cc or error
; throw - throw call/cc or error
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) return break continue throw))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw)
    (cond
      ((eq? 'new (statement-type statement)) (generate-instance-closure statement environment return))
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return throw))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment throw))
      ((and (eq? '= (statement-type statement)) (list? (caddr statement)) (eq? 'funcall (caaddr statement))) (interpret-assign statement (M-state-function (caddr statement) environment throw) throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment throw))
      ((eq? 'funcall (statement-type statement)) (M-state-function statement environment throw))
      ((eq? 'function (statement-type statement)) (insert-function statement environment throw))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; interprets the main function of the file
(define run-main
  (lambda (environment return break continue throw)
    ;;(get-function-body 'main environment)))
   (interpret-statement-list (get-function-body 'main environment throw) (push-frame environment) return break continue throw)))

; statement-list interpreter for when you want to return states instead of values
(define interpret-statement-list-for-env
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list-for-env (cdr statement-list) (interpret-statement-for-env (car statement-list) environment return break continue throw) return break continue throw))))

; statement interpreter that returns states instead of values
(define interpret-statement-for-env
  (lambda (statement environment return break continue throw)
    (if (eq? 'return (statement-type statement))
        (return environment)
        (interpret-statement statement environment return break continue throw))))

; gets the function body for a specified function
(define get-function-body
  (lambda (func-name environment throw)
    (func-body (get-function-closure func-name environment throw))))

;gets the function closure for a specified function
(define get-function-closure
  (lambda (func-name environment throw)
    ;don't question it, it just works
    (eval-expression func-name environment throw)))

; will work currently for 
(define generate-func-env
  (lambda (func-name function-closure func-call environment throw)
    (cons (generate-param-bindings func-name environment func-call throw) (get-layers-in-scope func-name environment))))

; gets the layer w/ global function and variable declarations
(define get-layers-in-scope
  (lambda (func-name environment)
    (cond
      ((null? (cdr environment)) environment)
      ((exists-in-list? func-name (caar environment)) environment)
      (else (get-layers-in-scope func-name (cdr environment))))))

;(list (list (car (get-function-closure 'fib (global-level-parse (parser "test4.txt") '((()()))))) (cddr '(funcall fib 10))))
; TODO: have this evaluate variables
(define generate-param-bindings
  (lambda (func-name environment func-call throw)
    (cond
      ((null? (cddr func-call)) (newframe))
      ((not (eq? (length (car (get-function-closure func-name environment throw))) (length (eval-params (cddr func-call) environment throw)))) (myerror "Mismatched parameters and arguments"))
      (else (list (car (get-function-closure func-name environment throw)) (eval-params (cddr func-call) environment throw))))))

; evaluates parameters for functions
(define eval-params
  (lambda (params-list environment throw)
    (cond
      ((null? params-list) '())
      (else (cons (eval-expression (car params-list) environment throw) (eval-params (cdr params-list) environment throw))))))

;TODO: REPLACE FUNC-ENV
(define insert-function
  (lambda (statement environment throw)
    (insert (function-name statement)
            (function-closure statement (lambda(name closure call env throw) (generate-func-env name closure call env throw))) environment)))

; evaluates the function environment function stored in the closure to get all bindings in scope for a function call
(define evaluate-func-env
  (lambda (name closure call env throw)
    ((caddr (get-function-closure name env throw)) name closure call env throw)))

(define funcall-name cadr)

; M-state for function for when the return value of a function is not being used
(define M-state-function
  (lambda (funcall environment throw)
    (call/cc
     (lambda (func-return)
       (M-state-function-helper funcall environment func-return (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  throw)))))

; helper for M-state-function that reinitializes the continuations
(define M-state-function-helper
  (lambda (funcall environment return break continue throw)
    (interpret-statement-list-for-env (get-function-body (funcall-name funcall) environment throw)
                                                 (evaluate-func-env (funcall-name funcall) (get-function-closure (funcall-name funcall) environment throw) funcall environment throw)
                                                 return break continue throw)))

; reinitializes the continuations for M-value-function
(define M-value-function
  (lambda (funcall environment throw)
    (call/cc
     (lambda (func-return)
        (M-value-function-helper funcall environment func-return (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  throw)))))
;interprets functions
(define M-value-function-helper
  (lambda (funcall environment return break continue throw)
    (interpret-statement-list (get-function-body (funcall-name funcall) environment throw)
                                                 (evaluate-func-env (funcall-name funcall) (get-function-closure (funcall-name funcall) environment throw) funcall environment throw)
                                                 return break continue throw)))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return throw)
    (return (eval-expression (get-expr statement) environment throw))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment throw)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment throw) environment)
        (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment throw)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment throw) environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) environment throw) (interpret-statement (get-then statement) environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment throw)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw)
    (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v env)))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment throw) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment throw)))))

(define func_name cadr)

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment throw)))
      ((eq? 'funcall (operator expr)) (M-value-function expr environment throw))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment throw)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment throw) environment throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment throw)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; Helper functions for interpreter part 3
(define function-name cadr)
(define function-parameters caddr)
(define function-body cadddr)
(define remaining-statements cdr)
(define individual-statement car)
(define env-names caar)
(define env-values caadar)
(define func-body cadr)

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable or function" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable or function" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Generates the function closure given a function definition
; Note env-func should be a function that generates the function environment (Not worked out yet)
(define function-closure
  (lambda (function-def env-func)
    (cond
      ((null? (function-name function-def)) (myerror "error: invalid function definition"))
      (else (list (function-parameters function-def) (function-body function-def) env-func)))))
      

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

