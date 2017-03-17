(load "common.scm")

(define error? procedure?)
(define (errorMsg e) (e))

(define (var-initVal) "UNKOWN")

(define (assert-attr attr val)
  (lambda (x) (eq? (attr x) val)))

; The 'value' class

(define (value-make val type)
  (dict-make+ (list 'v 'type 'ref)
              (list val type #f)))

(define (value-ref-make ref type)
  (dict-make+ (list 'v 'type 'ref)
              (list ref type ref)))

(define (value-v value) (dict-get value 'v))

(define (value-type value) (dict-get value 'type))

(define (value-ref? value) (dict-get value 'ref))

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

; get the left-value of id from env
; REQUIRE: that variable is defined
(define (env-getVar env id)
  (if (not (dict-exisist? env id))
      (iException+ (list "Variable undeclared: " id))
      (env-deref env (env-getRef env id)) ))

; Dereferences a reference within env
(define (env-deref env ref)
  (if (not (value-ref? ref))
      (iException+ (list "Trying to dereference a rvalue."))
      (unbox (value-v ref)) ))

(define (env-varDefined? env id)
  (dict-exisist? env id) )

; Inserts a new variable into environment
; REQUIRE : Current Environment does not contain a definition
(define (env-defineVar env id value)
  (if (env-varDefined? env id)
      (iException+ ("Multiple Definition: " id))
      (dict-add env id (box value)) ))

; Deletes an identifier from environment
; REQUIRE : Current env contains such id
(define (env-undefVar env id)
  (if (env-varDefined? env id)
      (dict-remove env id)
      (iException (list "Trying to undef non-existent var:" id)) ))

; Assigns to an existing variable in environment
; REQUIRE : If the reference is already invalidated,
;           trying to assigning to this will result in undefined behavior!
(define (env-assignToRef env ref value)
  (if (not (value-ref? ref))
      (iException (list "Trying to assign to rvalue."))
      (begin (set-box! (value-v ref) value) env) ))

; Returns a rvalue
(define (env-getRef env id)
  (if (not (dict-exisist? env id))
      (iException+ (list "Variable undeclared: " id))
      (let ([box-val (dict-get env id)])
        (value-ref-make box-val
                        (value-type (unbox box-val)) ))))

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
