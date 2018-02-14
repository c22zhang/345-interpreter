;Chris Zhang
;Jeremy Chan
;EECS 345 Interpreter project, part 1

(load "simpleParser.scm")

(define var-declaration?
  (lambda (expression)
    (cond
      ((null? expression) #f)
      ((eq? 'var (car expression)) #t)
      (else #f))))

(define assignment?
  (lambda (expression)
    (cond
      ((null? expression) #f)
      ((eq? '= (car expression)) #t)
      (else #f))))

(define return?
  (lambda (expression)
    (cond
      ((null? expression) #f)
      ((eq? 'return (car expression)) #t)
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

(define m.value.int
  (lambda (e)
    (cond
      ((number? e) e)
      ((eq? '+ (operator e)) (+ (m.value.int (operand1 e)) (m.value.int(operand2 e))))
      ((eq? '- (operator e)) (- (m.value.int (operand1 e)) (m.value.int(operand2 e))))
      ((eq? '* (operator e)) (* (m.value.int (operand1 e)) (m.value.int(operand2 e))))
      ((eq? '/ (operator e)) (quotient (m.value.int (operand1 e)) (m.value.int(operand2 e))))
      ((eq? '% (operator e)) (remainder (m.value.int (operand1 e)) (m.value.int(operand2 e))))
      (else (error 'badop "undefined operator")))))

(define m.value.boolean
  (lambda (e)
    (cond
      ((or (arithmetic-operator? e) (number? e)) (m.value.int e))
      ((eq? '> (operator e)) (> (m.value.boolean(operand1 e)) (m.value.boolean(operand2 e))))
      ((eq? '>= (operator e)) (>= (m.value.boolean(operand1 e)) (m.value.boolean(operand2 e))))
      ((eq? '< (operator e)) (< (m.value.boolean(operand1 e)) (m.value.boolean(operand2 e))))
      ((eq? '<= (operator e)) (<= (m.value.boolean(operand1 e)) (m.value.boolean(operand2 e))))
      ((eq? '== (operator e)) (eq? (m.value.boolean(operand1 e)) (m.value.boolean(operand2 e))))
      ((eq? '&& (operator e)) (and (m.value.boolean(operand1 e)) (m.value.boolean(operand2 e))))
      ((eq? '|| (operator e)) (or (m.value.boolean(operand1 e)) (m.value.boolean(operand2 e))))
      ((eq? '!= (operator e)) (not (eq? (m.value.boolean(operand1 e)) (m.value.boolean(operand2 e)))))
      (else (error 'badop "undefined operator")))))

(define operator car)
  
(define operand1 cadr)
(define operand2 caddr)