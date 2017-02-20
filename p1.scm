; Programming Project, Part 1
; Ryan Rose, rtr29

; Load the parser and lexical analyzer
(load "simpleParser.scm")
(load "lex.scm")

; interpreter:
; inputs:
;   fd - file name of code to be interpreted
(define interpreter
  (lambda (fd)
    (interpret (parser fd) '(() ())) ))

; interpret
; inputs:
;  pt - parse tree
;  s - state
; state structure:
;  ( (list of variable names) (list of values) )
;  values are empty list when not initialized
(define interpret
  (lambda (pt s)
    display pt ))

; getVal
; Wrapper method for getVal* to deconstruct state variable as necessary
; inputs:
;  name - the name of the variable to find
;  state - the state to look in
; returns:
;  --See return values for getVal*
(define getVal
  (lambda (name state)
    (getVal* name (car state) (cadr state))))

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