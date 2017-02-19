(load "common.scm")

(define error? procedure?)
(define (errorMsg e) (e))

(define (var-initVal) "UNKOWN")

(define (assert-attr attr val)
  (lambda (x) (eq? (attr x) val)))

(define (env-make)
  '())


(define (var-make id value)
  (list id value))
  
(define (var-name var)
  (car var))

(define (var-value var)
  (cadr var))

(define (var-assignTo var value)
  (var-make (var-name var) value))

; get an var by id from env
; REQUIRE: that variable is defined
(define (env-getVar env id)
   (let ((var (filter+ (assert-attr var-name id) env)))
    (cond
      ((> (length var) 1) (lambda () "MultiDef"))
      ((pair? var) (car var))
      (else (error-make "Variable undeclared: " id)) )))

(define (env-varDefined? env id)
  (not (error? (env-getVar env id))))

; Inserts a new variable into environment
; REQUIRE : Current Environment does not contain a definition
(define (env-defineVar env var)
  (if (env-varDefined? env (var-name var))
      (error-make "Multiple Definition: " (var-name var))
      (cons var env) ))

; Deletes an identifier from environment
; REQUIRE : Current env contains such id
(define (env-undefVar env id)
  (if (env-varDefined? env id)
      (remove-first (assert-attr var-name id) env)
      (error-make "Trying to undef non-existent var:" id) ))

; Assigns to an existing variable in environment
; REQUIRE : There exists such variable
(define (env-assignToVar env id value)
  (let ((envp (env-undefVar env id))
        (var (env-getVar env id)) )
    (if (error? envp)
        (error-make "Assigning to undeclared variable:" id)
        (env-defineVar envp (var-assignTo var value)) )))

(define (env-hasReturn? env)
  (env-varDefined? env 'ret))

(define (env-setReturn env value)
  (if (env-hasReturn? env)
      (env-assignToVar env 'ret value)
      (env-defineVar env (var-make 'ret value)) ))

(define (env-return env)
  (if (env-hasReturn? env)
      (var-value (env-getVar env 'ret))
      (error-make "Accessing undefined return value" '!) ))

 
; Tests
;(equal? (env-getVar '((a 1)) 'a) '(a 1))
;(equal? (env-getVar '() 'a) '())
;(equal? ((env-getVar '((a 1) (a 1)) 'a)) "MultiDef")
;(equal? (env-getVar '((a 1) (b 1)) 'b) '(b 1))
;
;(define penv (env-defineVar (env-make) (var-make 'a 1)))
;(define ppenv (env-defineVar penv (var-make 'b 2)))
;(env-assignToVar ppenv 'a 2)
;(errorMsg (env-assignToVar ppenv 'c 2))