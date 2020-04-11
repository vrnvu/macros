#lang racket

; transformer binding, this tells racket compiler to replace all foo encounters
(define-syntax lambda-foo
  (lambda (stx)
    (syntax "I am foo")))

; Same sugar syntax we can find in any function
(define-syntax (foo stx)
  (syntax "I am foo with sugar syntax"))

; Compact syntax form
(define-syntax (quote-foo stx)
  #'"I am foo with a ' from")

(define-syntax (greet-world stx)
  #'(displayln "Hello, world!"))

; What if we want to transform the input syntax
; (log '(+ 1 2 3 4 5))
(define-syntax (log stx)
  (print stx)
  #'(void))

; We can explore the s-expression
(define ex-stx #'(if x (list "true") #f))
(syntax-source ex-stx)
(syntax-line ex-stx)
(syntax-column ex-stx)
(syntax->datum ex-stx)
(syntax-e ex-stx)
(syntax->list ex-stx)

; Transforming the stx input
(define-syntax (reverse stx)
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))

; We need the values in order to generate the reverse: values "second" "first"
(reverse "first" "second" values)

; Step by step, first syntax->datum to convert stx to a list
; '(reverse "first" "second" values)
; cdr, we cannot use rest, to remove the procedure name
; '("first "second" values)
; Now we reverse and make use of the values
; '(values "second" "first")
; datum->syntax
; This is the output of our transformation function and what is then evaluated by the compiler
; (values "second" "first")

; Lets see an exemple of compile vs runtime
(define (runtime-if p t-expr f-expr)
  (cond [p t-expr]
        [else f-expr]))

(runtime-if #t "true" "false")

; The above example seems to work but in the following case
(define (display-and-return x)
  (displayln x)
  x)

(runtime-if #t
            (display-and-return "true")
            (display-and-return "false"))

; In our runtime-if, since we expand and evaluate the args the side-effectes are executed
; Notice again the cadr, caddr and cadddr usage to access the elements of stx
; We again simply convert the syntax to list and then construct our cond sexpression to return it
(define-syntax (compiletime-if stx)
  (define xs (syntax->list stx))
  (datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                            [else ,(cadddr xs)])))
(compiletime-if #t
                (display-and-return "true")
                (display-and-return "false"))

(define to-check-0 #'(compiletime-if #t "true" "false"))
(displayln (syntax->list to-check-0))

; The pair accessors are quite error prone, we would like to use pattern matchin instead
; For pattern matching at compile time we need to use (require racket/match)
(require (for-syntax racket/match))

(define-syntax (compiletime-if-match stx)
  (match (syntax->list stx)
    [(list _ p t-expr f-expr)
     (datum->syntax stx `(cond [,p ,t-expr]
                               [else ,f-expr]))]))

(compiletime-if-match #t "true" "false")

; To use our procedures at compile time we could require them as we did with for-syntax
; Defining our functions in a new module and importing them
; If instead we want the helper in the same module we need to define inside the begin-for-syntax
; Or use define-for-syntax which composes begin-for-syntax and define
