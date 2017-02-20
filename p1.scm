; Programming Project, Part 1
; Ryan Rose, rtr29

; Load the parser and lexical analyzer
(load "simpleParser.scm")

; interpreter:
; inputs:
;   fd - file name of code to be interpreted
(define interpreter
  (lambda (fd)
    (interpret (parser fd) '(() ()) ) ))

; interpret
; inputs:
;  pt - parse tree
;  s - state
(define interpret
  (lambda (pt s)
    ()))

; decVal - declares and initializes a variable
; inputs:
;  name - variable name
;  value - variable value
;  state - the current state
; outputs:
;  the updated state
(define decVal 
  (lambda (name value state)
    (cond
      ; if name or value are null, error
      ((or (null? name) (null? value)) (error "failed adding variable to state"))
      ; if the var name already exists, error
      ((not (eqv? (getVal name state) #f)) (error "namespace for var already occupied"))
      (else
       ; add name and value to state
       (cons (cons name (car state)) (cons (cons value (cadr state)) '()) )))))

; setVal - sets the value of an initialized variable
; inputs:
;  name - variable name
;  value - variable value
;  state - the current state
; outputs:
; the updated state
(define setVal
  (lambda (name value state)
    (cond
      ; if the names or values of states are null, error
      ((and (null? (car state)) (null? (cadr state))) (error "variable name not found"))
      ; if it finds the var, set var 
      ((eqv? name (caar state)) (cons (car state) (cons (cons value (cdadr state)) '())))    
      ; else recurse on the next state value 
      (else (cons (cons (caar state) (car (setValRec name value state))) (cons (cons (caadr state) (cadr (setValRec name value state))) '()))) )))
       
(define setValRec
  (lambda (name value state)
    (setVal name value (cons (cdar state) (cons (cdadr state) '()))) ))

; getVal
; Wrapper method for getVal* to deconstruct state variable as necessary
; inputs:
;  name - the name of the variable to find
;  state - the state to look in
; returns:
;  --See return values for getVal*
(define getVal
  (lambda (name state)
    (cond
      ((null? name) (error "GETVAL ERROR: Name cannot be null."))
      ((null? state) (error "GETVAL ERROR: State cannot be null."))
      (else (getVal* name (car state) (cadr state))))))

; getVal*
; Gets the value of a given variable
; inputs:
;  name - the name of the variable to find
;  vars - the list of variable names from the current state
;  vals - the list of values in the current state
; returns:
;  Value of variable, if initialized
;  '() if defined but not initialized
;  #f if not defined
(define getVal*
  (lambda (name vars vals)
    (cond
      ((and (null? vars) (null? vals)) #f)
      ((and (not (null? vars)) (not (null? vals)))
       (if (eqv? name (car vars)) (car vals) (getVal* name (cdr vars) (cdr vals))))
      (else (error "STATE MISMATCH ERROR: Different number of Variables and Values.")))))
