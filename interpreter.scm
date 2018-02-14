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
