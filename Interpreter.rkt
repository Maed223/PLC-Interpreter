#lang racket

(require "simpleParser.rkt")

(define M_state
  (lambda (file)
    (M_statementlist (parser file) (newstate) (lambda (env) env))))

(define M_statementlist
  (lambda (statementlist state next)
      (if (null? statementlist)
          (next state)
          (M_statement (car statementlist) state (lambda (env) (M_statementlist (cdr statementlist) state next)))))) 

(define M_statement
  (lambda (expression state next)
    (cond
      ((eq? 'return (statement-type expression)) (M_statement_return expression state))
      ((eq? 'var (statement-type expression)) (M_statement_declare expression state next))
      ((eq? '= (statement-type expression)) (M_statement_assign expression state next))
      ((eq? 'if (statement-type expression)) (M_statement_if expression state next))
      ((eq? 'while (statement-type expression)) (M_statement_while expression state next))
      (else (error 'badop "invalid something or other")))))

; Calls the return continuation with the given expression value
(define M_statement_return
  (lambda (statement environment)
    (eval-expression (get-expr statement) environment)))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define M_statement_declare
  (lambda (statement environment next)
    (if (exists-declare-value? statement)
        (next (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment) environment))
        (next (insert (get-declare-var statement) 'novalue environment)))))

; Updates the environment to add a new binding for a variable
(define M_statement_assign
  (lambda (statement environment next)
    (next (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment) environment))))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define M_statement_if
  (lambda (statement environment next)
    (cond
      ((eval-expression (get-condition statement) environment) (M_statement (get-then statement) environment next))
      ((exists-else? statement) (M_statement (get-else statement) environment next))
      (else (next environment)))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define M_statement_while
  (lambda (statement environment next)
      (loop (get-condition statement) (get-body statement) environment next)))

(define loop
  (lambda (condition body environment next)
    ((eval-expression condition environment) (M_statement body environment (next environment)) )))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment) environment)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment))))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment)))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment))))))
      ;error

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))

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

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-value operand2)
(define get-declare-var operand1)
(define exists-declare-value? exists-operand2?)
(define exists-else? exists-operand3?)
(define get-condition operand1)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)

(define insert
  (lambda (var val environment)
    (cond
      ((exists-in-list? var (variables (car environment))) (cons (add-to-frame var val (car environment)) (cdr environment))))))
      
        

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    ((exists? var environment) (update-existing var val environment))))
        ;error

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (error "variable without assigned val")
          value))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

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

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (error "oops"))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (error "opps")
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      ((lookup-in-env var (cdr environment)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; create a new empty environment
(define newstate
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

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