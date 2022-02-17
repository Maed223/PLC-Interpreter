#lang racket/base
(require "simpleParser.scm")

;NAMING SCHEME -> Functions with "M_state....." belong to M_state actions while functions with "eval" belong to M_value and M_boolean actions



;M_STATE OPERATIONS
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  Sets default continuations for return, break, continue, throw, and "next statement"
(define interpret
  (lambda (file)
    (scheme->language
     (M_statements (parser file) (newenvironment) (lambda (v) v) (lambda (env) env)))))

; interprets a list of statements.  The state/environment from each statement is used for the next ones.
(define M_statements
  (lambda (statement-list environment return next)
    (if (null? statement-list)
        (next environment)
        (M_state (car statement-list) environment return (lambda (env) (M_statements (cdr statement-list) env return next))))))

; interpret a statement in the environment with continuations for return, break, continue, throw, and "next statement"
(define M_state
  (lambda (statement environment return next)
    (cond
      ((eq? 'return (statement-type statement)) (M_state_return statement environment return))
      ((eq? 'var (statement-type statement)) (M_state_declare statement environment next))
      ((eq? '= (statement-type statement)) (M_state_assign statement environment next))
      ((eq? 'if (statement-type statement)) (M_state_if statement environment return next))
      ((eq? 'while (statement-type statement)) (M_state_while statement environment return next))
      (else (error "Unknown statement")))))

; Calls the return continuation with the given expression value
(define M_state_return
  (lambda (statement environment return)
    (return (eval-expression (get-expr statement) environment))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define M_state_declare
  (lambda (statement environment next)
    (if (exists-declare-value? statement)
        (next (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment) environment))
        (next (insert (get-declare-var statement) 'novalue environment)))))

; Updates the environment to add a new binding for a variable
(define M_state_assign
  (lambda (statement environment next)
    (next (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment) environment))))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define M_state_if
  (lambda (statement environment return next)
    (cond
      ((eval-expression (get-condition statement) environment) (M_state (get-then statement) environment return next))
      ((exists-else? statement) (M_state (get-else statement) environment return next))
      (else (next environment)))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define M_state_while
  (lambda (statement environment return next)
    (letrec ((loop (lambda (condition body environment)
                     (if (eval-expression condition environment)
                         (M_state body environment return (lambda (env) (loop condition body env)))
                         (next environment)))))
      (loop (get-condition statement) (get-body statement) environment))))
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------

; M_VALUE and M_BOOLEAN OPERATIONS
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment)))
      (else (error "Unknown operator")))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------


; HELPERS
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------


; FUNCTIONS THAT WORK ON STATE
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; useful abstractions
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
          (error "error: variable without an assigned value")
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (error "error: undefined variable"))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (error "error: undefined variable"))
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
        (error "error: variable is being re-declared")
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (error "error: variable used but not defined"))))

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
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------

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


