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
(define interpret
  (lambda (pt s)
    display pt ))
