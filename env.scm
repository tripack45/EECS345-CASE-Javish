(load "common.scm")

(define error? procedure?)
(define (errorMsg e) (e))

(define (var-initVal) "UNKOWN")

(define (assert-attr attr val)
  (lambda (x) (eq? (attr x) val)))

; The 'value' class

(define (value-make val type)
  (dict-make+ (list 'v 'type 'attr)
              (list val type (dict-make))))

(define (lvalue-make boxed-rvalue)
  (let ([rvalue (unbox boxed-rvalue)])
    (dict-make+ (list 'v 'type 'attr)
                (list (value-v rvalue)
                      (value-type rvalue)
                      (dict-make+ (list 'lvalue)
                                  (list boxed-rvalue) )))))

(define (value-v value) (dict-get value 'v))

(define (value-type value) (dict-get value 'type))

(define (value-lvalue? value)
  (dict-exisist? (dict-get value 'attr) 'lvalue))

(define (value-lvalue value)
  (dict-get (dict-get value 'attr) 'lvalue))

(define (value-torvalue lvalue)
  (if (value-lvalue? lvalue)
      (unbox (dict-get (dict-get lvalue 'attr) 'lvalue))
      lvalue))
  

; ========= 'env' class =============
; Note due to involvement of "boxes" now
; 'env' objects are stateful objects.
; in particular, modification of variable value
; in the env class is a STATEFUL operation

(define env-make dict-make)
(define (env-make+ l1 l2)
  (foldl (lambda (id value env)
           (env-defineVar env id value) )
         (env-make)
         l1 l2))

(define (env-varDefined? env id)
  (dict-exisist? env id) )

; Inserts a new variable into environment
; REQUIRE : Current Environment does not contain a definition
(define (env-defineVar env id value)
  (if (env-varDefined? env id)
      (iException+ (list "Multiple Definition: " id))
      (dict-add env id (box value)) ))

; Deletes an identifier from environment
; REQUIRE : Current env contains such id
(define (env-undefVar env id)
  (if (env-varDefined? env id)
      (dict-remove env id)
      (iException (list "Trying to undef non-existent var:" id)) ))

; Assigns to an existing variable in environment
; REQUIRE : If the rvalue is already invalidated,
;           trying to assigning to this will result in undefined behavior!
(define (env-assignToLval env lval value)
  (if (not (value-lvalue? lval))
      (iException (list "Trying to assign to rvalue."))
      (let ([boxed-rvalue (value-lvalue lval)])
        (begin
          (set-box! boxed-rvalue (value-torvalue value))
          env ))))

; Returns a lvalue
(define (env-getVar env id)
  (if (not (dict-exisist? env id))
      (iException+ (list "Variable undeclared: " id))
        (lvalue-make (dict-get env id))))

;(define (env-hasReturn? env)
;  (env-varDefined? env 'ret))
;
;(define (env-setReturn env value)
;  (if (env-hasReturn? env)
;      (env-assignToVar env 'ret value)
;      (env-defineVar env (var-make 'ret value)) ))
;
;(define (env-return env)
;  (if (env-hasReturn? env)
;      (var-value (env-getVar env 'ret))
;      (error-make "Accessing undefined return value" '!) ))
