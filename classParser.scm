; A simple parser for a Java style language
; EECS 345: Programming Languages Concepts
;
; A recursive descent parser and a lexical analyzer for simple Java statements.
; The language allows assignments, all mathematical expressions, if statements,
; while statements (including break and continue), blocks, functions, objects, 
; and exceptions.
;
; To call the parser, use:
;     (parser filename)
;
; The return value is a parse tree in list format

(load "lex.scm")

(define parser
  (lambda (filename)
    (begin (start-lex filename)
           (let ((parse-tree (program-parse)))
             (end-lex)
             parse-tree))))

;===============================================
; The recursive descent parser

(define program-parse
  (lambda ()
    (if (eq? (car (get-next-symbol)) 'EOF)
       '()
       (begin
         (unget-next-symbol)
         (let ((parsetree (class-parse)))
           (cons parsetree (program-parse)))))))

; a program is made up of classes.  Each class should be a name, the class that is extended,
; and a body nested in braces

(define class-parse
  (lambda ()
    (if (not (eq? (car (get-next-symbol)) 'class))
        (error 'parser "Illegal start of class")
        (let* ((classname (get-next-symbol))
               (extendclass (extend-parse)))
          (if (not (eq? (car classname) 'ID))
              (error 'parser "Illegal class name")
              (if (not (eq? (car (get-next-symbol)) 'LEFTBRACE))
                  (error 'parser "Missing left brace at start of class")
                  (list 'class (cdr classname) extendclass (class-body-parse (cdr classname)))))))))

; parse the extends statement on a class, if it exists

(define extend-parse
  (lambda ()
    (if (not (eq? (car (get-next-symbol)) 'extends))
        (begin
          (unget-next-symbol)
          '()
          )
        (let ((extendclass (get-next-symbol)))
          (if (not (eq? (car extendclass) 'ID))
              (error 'parser "Illegal superclass name")
              (list 'extends (cdr extendclass)))))))

; parse the body of a class.  The body is assignment statements and method definitions.

(define class-body-parse
  (lambda (classname)
    (if (eq? (car (get-next-symbol)) 'RIGHTBRACE)
       '()
       (begin
         (unget-next-symbol)
         (let ((parsetree (top-level-parse classname)))
           (cons parsetree (class-body-parse classname)))))))

; parse the top level of a class.  The top level is a function definition (an identifier followed by a left
; parenthesis) or is a variable declaration.  Functions and variables can be static (class) or non-static (instance).

(define top-level-parse
  (lambda (classname)
    (let ((firstsymbol (get-next-symbol)))
      (if (eq? (car firstsymbol) 'static)
          (top-statement-parse classname #t)
          (begin
            (unget-next-symbol)
            (top-statement-parse classname #f))))))

; detect if the statement in a class definition is a variable declaration or a function definition.

(define top-statement-parse
  (lambda (classname static)
    (let ((firstsymbol (get-next-symbol)))
      (cond
        ((eq? (car firstsymbol) 'function) (top-function-parse static))
        ((eq? (car firstsymbol) 'var)
            (let ((parse-statement '()))
              (begin 
                (set! parse-statement (top-declare-parse static))
                (if (eq? (car (get-next-symbol)) 'SEMICOLON)
                    parse-statement
                    (error 'parser "Missing semicolon")))))
        ((eq? (car firstsymbol) 'ID)
           (cond
	      ((not (equal? (cdr firstsymbol) classname)) (error 'parser "Illegal language structure or constructor with different name as class"))
	      ((not (eq? (car (get-next-symbol)) 'LEFTPAREN)) (error 'parser "Illegal start of constructor definition"))
              (static (error 'parser "Constructor can not be static"))
	      (else (constructor-parse)))) 
        (else (error 'parser "Illegal start of function definition or variable declaration"))))))

; calls function-parse to parse a function name.  It is a static function, replaces the operator
; at the start of the returned expression with 'static-function

(define top-function-parse
  (lambda (static)
    (let ((function-def (function-parse #t)))
      (if static
          (if (eq? 'abstract-function (car function-def))
              (error 'parser "Static functions cannot be abstract")
              (cons 'static-function (cdr function-def)))
          function-def))))

; calls declare-parse to parse a variable declaration.  If it is a static variable, replaces the 
; operator at the start of the returned expression with 'static-var

(define top-declare-parse
  (lambda (static)
    (if static
        (cons 'static-var (cdr (declare-parse)))
        (declare-parse))))

; parse a function. A function is the name, followed by a formal parameter list, 
; followed by the body nested in braces

(define function-parse
  (lambda (toplevel)
    (let ((name (get-next-symbol)))
       (if (and (eq? (car name) 'ID) (eq? (car (get-next-symbol)) 'LEFTPAREN))
           (let ((paramlist (get-formalparameter-list))
                 (nextsymbol (car (get-next-symbol))))
              (if (not (eq? nextsymbol 'LEFTBRACE))
                  (if (and (eq? nextsymbol 'SEMICOLON) toplevel)
                      (list 'abstract-function (cdr name) paramlist)
                      (error 'parser "Missing left brace"))
                  (list 'function (cdr name) paramlist (compound-statement-parse))))
           (error "Illegal start of function definition")))))

; parse a constructor.  Very similar to a function parse but uses "constructor" instead of "function name" as the header
(define constructor-parse
  (lambda ()
    (let ((paramlist (get-formalparameter-list)))
       (if (not (eq? (car (get-next-symbol)) 'LEFTBRACE))
           (error 'parser "Missing left brace")
           (list 'constructor paramlist (compound-statement-parse))))))

; parse the formal parameter list.  The list is a sequence of identifiers separated by commas

(define get-formalparameter-list
  (lambda ()
    (let ((nextsymbol (get-next-symbol)))
      (cond
        ((eq? (car nextsymbol) 'RIGHTPAREN) '())
        ((eq? (car nextsymbol) 'ID)
          (let ((separator (car (get-next-symbol))))
            (cond
              ((eq? separator 'COMMA) (cons (cdr nextsymbol) (get-formalparameter-list)))
              ((eq? separator 'RIGHTPAREN) (list (cdr nextsymbol)))
              (else (error 'parser "Missing comma")))))
        ((and (eq? (car nextsymbol) 'BINARY-OP) (eq? (cdr nextsymbol) '&))
	   (let* ((id (get-next-symbol))
                  (separator (car (get-next-symbol))))
             (if (eq? (car id) 'ID)
                 (cond
                   ((eq? separator 'COMMA) (cons '& (cons (cdr id) (get-formalparameter-list))))
                   ((eq? separator 'RIGHTPAREN) (list '& (cdr id)))
                   (else (error 'parser "Missing comma")))
                 (error 'parser "Missing identifier after reference operator"))))
        (else (error 'parser "Illegal function parameter, missing right parenthesis?"))))))

; parse a statement that can be an if-statement, a while-statement, or a compound statement
; and if none of the above, it is a simple statement

(define statement-parse
  (lambda ()
    (let ((nextsymbol (car (get-next-symbol))))
      (cond
        ((eq? nextsymbol 'if) (if-parse))
        ((eq? nextsymbol 'while) (while-parse))
	((eq? nextsymbol 'function) (function-parse #f))
        ((eq? nextsymbol 'try) (try-parse))
        ((eq? nextsymbol 'LEFTBRACE) (cons 'begin (compound-statement-parse)))
        (else (begin
                (unget-next-symbol)
                (simple-statement-parse)))))))

; parse a simple statement that can be a return, break, continue, or an assignment statement

(define simple-statement-parse
  (lambda ()
    (let ((nextsymbol (get-next-symbol))
          (parse-statement '()))
      (begin
        (cond ((eq? (car nextsymbol) 'return) (set! parse-statement (return-parse)))
              ((eq? (car nextsymbol) 'var) (set! parse-statement (declare-parse)))
              ((eq? (car nextsymbol) 'break) (set! parse-statement (list 'break)))
              ((eq? (car nextsymbol) 'continue) (set! parse-statement (list 'continue)))
              ((eq? (car nextsymbol) 'throw) (set! parse-statement (list 'throw (value-parse))))
              ((eq? (car nextsymbol) 'ID) (begin (unget-next-symbol) (set! parse-statement (function-or-assign-parse))))
              (else (error 'parse "language feature not implemented")))
         (if (eq? (car (get-next-symbol)) 'SEMICOLON)
             parse-statement
             (error 'parser "Missing semicolon"))))))

; parse a compound statement.  We already saw the left brace so continue until we see a right brace.

(define compound-statement-parse
  (lambda ()
    (if (eq? (car (get-next-symbol)) 'RIGHTBRACE)
        '()
        (begin
          (unget-next-symbol)
          (let ((s (statement-parse)))
            (cons s (compound-statement-parse)))))))

; parse a return statement: return followed by a value.

(define return-parse
  (lambda ()
    (list 'return (value-parse))))

; parse an if statement: a condition inside parentheses, an if statement, and an optional else

(define if-parse
  (lambda ()
    (if (not (eq? (car (get-next-symbol)) 'LEFTPAREN))
        (error 'parser "Missing opening parenthesis")
        (let ((condition (value-parse)))  ; changed
           (if (not (eq? (car (get-next-symbol)) 'RIGHTPAREN))
               (error 'parser "Missing closing parenthesis")
               (let ((if-statement (statement-parse)))
                  (if (eq? (car (get-next-symbol)) 'else)
                      (list 'if condition if-statement (statement-parse))
                      (begin
                        (unget-next-symbol)
                        (list 'if condition if-statement)))))))))

; parse a try block.  The try block is a compound statement followed by catch block and/or
; a finally block

(define try-parse
  (lambda ()
    (if (not (eq? (car (get-next-symbol)) 'LEFTBRACE))
        (error 'parser "Left brace expected")
        (let* ((tryblock (compound-statement-parse))
               (catchblock (catch-parse))
               (finallyblock (finally-parse)))
          (if (and (null? catchblock) (null? finallyblock))
              (error 'parser "try without catch of finally")
              (list 'try tryblock catchblock finallyblock))))))

; parse a catch block.  The catch block must contain a variable (the exception) inside
; parentheses and then a block of code.

(define catch-parse
  (lambda ()
    (let ((nextsymbol (car (get-next-symbol))))
      (if (not (eq? nextsymbol 'catch))
          (begin
            (unget-next-symbol)
            '())
          (let* ((firstsymbol (get-next-symbol))
                 (secondsymbol (get-next-symbol))
                 (thirdsymbol (get-next-symbol))
                 (fourthsymbol (get-next-symbol)))
            (cond ((not (eq? (car firstsymbol) 'LEFTPAREN)) (error 'parser "Missing left parenthesis"))
                  ((not (eq? (car secondsymbol) 'ID)) (error 'parser "Missing exception parameter"))
                  ((not (eq? (car thirdsymbol) 'RIGHTPAREN)) (error 'parser "Missing closing parenthesis"))
                  ((not (eq? (car fourthsymbol) 'LEFTBRACE)) (error 'parser "Missing opening brace"))
                  (else (list 'catch (list (cdr secondsymbol)) (compound-statement-parse)))))))))

; parse a finally block.  A finally block is a compound statement that starts with "finally"

(define finally-parse
  (lambda ()
    (let ((nextsymbol (get-next-symbol)))
      (if (not (eq? (car nextsymbol) 'finally))
          (begin
            (unget-next-symbol)
            '())
          (if (not (eq? (car (get-next-symbol)) 'LEFTBRACE))
              (error 'parser "Missing opening parenthesis")
              (list 'finally (compound-statement-parse)))))))

; parse a while statement: a condition followed by a statement

(define while-parse
  (lambda ()
    (if (not (eq? (car (get-next-symbol)) 'LEFTPAREN))
        (error 'parser "Missing opening parenthesis")
        (let ((condition (value-parse)))
          (if (not (eq? (car (get-next-symbol)) 'RIGHTPAREN))
              (error 'parser "Missing closing parenthesis")
              (list 'while condition (statement-parse)))))))

; parse either a function or an assignment statement.  Parse the left hand side.  If it is
; a function call return it.  Otherwise, assume it is a variable and this is an assignment
; statement

(define function-or-assign-parse
  (lambda ()
    (let ((id (id-parse (variable-parse (get-next-symbol)))))
      (if (and (pair? id) (eq? (car id) 'funcall))
          id
          (assign-parse id)))))

; parse an id in an expression or statement.  The id can be a variable, a function call,
; or a combination using the dot operator.  This function keeps nesting varaibles and
; functions with the dot operator until we get something other than a dot or a parenthesis.

(define id-parse
  (lambda (leftvalue)
    (let ((nextsymbol (get-next-symbol)))
      (cond
        ((eq? (car nextsymbol) 'LEFTPAREN) (id-parse (funcall-parse leftvalue)))
        ((and (eq? (car nextsymbol) 'BINARY-OP) (eq? (cdr nextsymbol) 'dot))
           (id-parse (list 'dot leftvalue (variable-parse (get-next-symbol)))))
        (else (begin
                (unget-next-symbol)
                leftvalue))))))

; parse a function call: an identifier followed by a parameter list

(define funcall-parse
  (lambda (name)
    (cons 'funcall (cons name (get-actualparameter-list)))))

; parse a parameter list: a list with an arbitrary number of identifiers separated by commas

(define get-actualparameter-list
  (lambda ()
    (let ((nextsymbol (get-next-symbol)))
      (if (eq? (car nextsymbol) 'RIGHTPAREN)
          '()
          (begin
            (unget-next-symbol)
            (let* ((parameter (value-parse))
                   (separator (car (get-next-symbol))))
              (cond
                ((eq? separator 'COMMA) (cons parameter (get-actualparameter-list)))
                ((eq? separator 'RIGHTPAREN) (list parameter))
                (else (error 'parser "Missing comma")))))))))

; parse a variable declaration: var then left-hand-side with optional = followed by a value

(define declare-parse
  (lambda ()
    (let* ((lhs (lhs-parse (get-next-symbol)))
           (op (get-next-symbol)))
      (if (and (eq? (car op) 'BINARY-OP) (eq? (cdr op) '=))
          (append (cons 'var lhs) (list (value-parse)))
          (begin
            (unget-next-symbol)
            (cons 'var lhs))))))
    

; parse an assignment statement: a left-hand-side followed by an = followed by a value

(define assign-parse
  (lambda (lhs)
    (let ((op (get-next-symbol)))
      (if (and (eq? (car op) 'BINARY-OP) (eq? (cdr op) '=))
          (append (list (cdr op) lhs) (list (value-parse)))
          (error 'parser "Unknown assignment operator")))))

; parse the left hand side of an assignment.  Only variables are allowed.

(define lhs-parse
  (lambda (lhs)
    (if (eq? (car lhs) 'ID)
        (list (cdr lhs))
        (error 'parser "Illegal left hand side of assignment"))))

; parse a value.  The top level of the parse is the assignment operator.

(define value-parse
  (lambda ()
    (assignterm-parse (get-next-symbol))))
;    (let* ((lhs (get-next-symbol))
;           (op (get-next-symbol)))
;      (if (and (eq? (car lhs) 'ID) (eq? (car op) 'BINARY-OP) (eq? (cdr op) '=))
;          (list (cdr op) (cdr lhs) (value-parse))
;          (begin
;            (unget-next-symbol)
;            (orterm-parse lhs))))))

; Parsing the value.  The top level is the assign operator

(define assignterm-parse
  (lambda (firstsymbol)
    (assignterm-parse-helper (orterm-parse firstsymbol))))

; parse the assign expression.

(define assignterm-parse-helper
  (lambda (firstoperand)
    (let ((op (get-next-symbol)))
      (if (and (eq? (car op) 'BINARY-OP) (eq? (cdr op) '=))
          (assignterm-parse-helper (list '= firstoperand (assignterm-parse (get-next-symbol))))
          (begin
            (unget-next-symbol)
            firstoperand)))))

; continuing parsing the value.  The second level is the OR operator

(define orterm-parse
  (lambda (firstsymbol)
    (orterm-parse-helper (andterm-parse firstsymbol))))

; parse the OR expression.

(define orterm-parse-helper
  (lambda (firstoperand)
    (let ((op (get-next-symbol)))
      (if (and (eq? (car op) 'BINARY-OP) (eq? (cdr op) '\|\|))
          (orterm-parse-helper (list '|| firstoperand (andterm-parse (get-next-symbol))))
          (begin
            (unget-next-symbol)
            firstoperand)))))

; the third level is the AND expression

(define andterm-parse
  (lambda (firstsymbol)
    (andterm-parse-helper (equalterm-parse firstsymbol))))

; parse the AND expression.

(define andterm-parse-helper
  (lambda (firstoperand)
    (let ((op (get-next-symbol)))
      (if (and (eq? (car op) 'BINARY-OP) (eq? (cdr op) '&&))
          (andterm-parse-helper (list (cdr op) firstoperand (equalterm-parse (get-next-symbol))))
          (begin
            (unget-next-symbol)
            firstoperand)))))

; the next level is the equal operators

(define equalterm-parse
  (lambda (firstsymbol)
    (equalterm-parse-helper (compareterm-parse firstsymbol))))

; parse the equals expression.

(define equalterm-parse-helper
  (lambda (firstoperand)
    (let ((op (get-next-symbol)))
      (if (and (eq? (car op) 'BINARY-OP) (or (eq? (cdr op) '==) (eq? (cdr op) '!=)))
          (equalterm-parse-helper (list (cdr op) firstoperand (compareterm-parse (get-next-symbol))))
          (begin
            (unget-next-symbol)
            firstoperand)))))

; next we have the comparison operators

(define compareterm-parse
  (lambda (firstsymbol)
    (compareterm-parse-helper (addterm-parse firstsymbol))))

; parse the comparison expression.

(define compareterm-parse-helper
  (lambda (firstoperand)
    (let ((op (get-next-symbol)))
      (if (and (eq? (car op) 'BINARY-OP) (or (eq? (cdr op) '<) (eq? (cdr op) '<=) (eq? (cdr op) '>) (eq? (cdr op) '>=)))
          (compareterm-parse-helper (list (cdr op) firstoperand (addterm-parse (get-next-symbol))))
          (begin
            (unget-next-symbol)
            firstoperand)))))

; continue parsing the value.  The next level is the addition and subtraction operators.

(define addterm-parse
  (lambda (firstsymbol)
    (addterm-parse-helper (multterm-parse firstsymbol))))

; parse the addition expression.

(define addterm-parse-helper
  (lambda (firstoperand)
    (let ((op (get-next-symbol)))
      (if (and (eq? (car op) 'BINARY-OP) (or (eq? (cdr op) '+) (eq? (cdr op) '-)))
          (addterm-parse-helper (list (cdr op) firstoperand (multterm-parse (get-next-symbol))))
          (begin
            (unget-next-symbol)
            firstoperand)))))

; continue parsing the value.  The next level is the multiplication and division operators.

(define multterm-parse
  (lambda (firstsymbol)
    (multterm-parse-helper (operand-parse firstsymbol))))

; parse the multiplication expression.

(define multterm-parse-helper
  (lambda (firstoperand)
     (let ((op (get-next-symbol)))
       (if (and (eq? (car op) 'BINARY-OP) (or (eq? (cdr op) '*) (eq? (cdr op) '/) (eq? (cdr op) '%)))
           (multterm-parse-helper (list (cdr op) firstoperand (operand-parse (get-next-symbol))))
           (begin
             (unget-next-symbol)
             firstoperand)))))

; continue parsing the value.  The final level is the unary operators, variables, numbers, and nested parentheses.

(define operand-parse
  (lambda (firstsymbol)
    (cond
      ((eq? (car firstsymbol) 'LEFTPAREN)
          (let ((retvalue (value-parse)))
            (if (eq? (car (get-next-symbol)) 'RIGHTPAREN)
                retvalue
                (error 'parser "Unmatched left parenthesis"))))
      ((and (eq? (car firstsymbol) 'BINARY-OP) (eq? (cdr firstsymbol) '-)) (list '- (operand-parse (get-next-symbol))))  ; this is a new line
      ((and (eq? (car firstsymbol) 'BINARY-OP) (eq? (cdr firstsymbol) '!)) (list '! (operand-parse (get-next-symbol))))  ; this is a new line
      ((eq? (car firstsymbol) 'NUMBER) (cdr firstsymbol))
      ((eq? (car firstsymbol) 'ID) (id-parse (variable-parse firstsymbol)))
      ((eq? (car firstsymbol) 'BOOLEAN) (cdr firstsymbol))
      ((eq? (car firstsymbol) 'new) (let* ((classid (get-next-symbol))
                                           (nextsymbol (get-next-symbol)))
                                      (cond ((not (eq? (car classid) 'ID)) (error 'parser "Illegal class name in new"))
                                            ((not (eq? (car nextsymbol) 'LEFTPAREN)) (error 'parser "Missing left parenthesis"))
                                            (else (id-parse (cons 'new (cdr (funcall-parse (cdr classid)))))))))
      (else (error 'parser "Unknown statmement")))));)

; parse a variable name.

(define variable-parse
  (lambda (idsymbol)
    (if (not (eq? (car idsymbol) 'ID))
        (error 'parser "Identifier expected")
        (cdr idsymbol))))
