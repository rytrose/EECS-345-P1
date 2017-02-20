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
    (interpret (parser fd) '(() ()) ) ))

; interpret
; inputs:
;  pt - parse tree
;  s - state
(define interpret
  (lambda (pt s)
    ()))

; setVal
; inputs:
;  name - variable name
;  value - variable value
;  state - the current state
; outputs:
;  the updated state
(define setVal 
  (lambda (name value state)
    (cond
      ; if name or value are null, error
      ((or (null? name) (null? value)) (error "failed adding variable to state"))
      ; if the var name already exists, error
      ((not (eqv? (getVal name state) #f)) (error "namespace for var already occupied"))
      ; add to state
      ; 
      (else
       ; add name and value to state
       (cons value (cdr (cons name (car state)))) ))))
    

