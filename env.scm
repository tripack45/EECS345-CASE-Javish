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
; 'env' objects are stateful now!

(define (env-make)
  (dict-make+ (list 'stack 'cont)
              (list (list (layer-make))
                    (env-cont-make) )))

(define (env-make+ l1 l2)
  (dict-make+ (list 'stack 'cont)
              (list (list (layer-make+ l1 l2))
                    (env-cont-make) )))

(define (env-make-cont return throw)
  (dict-make+ (list 'stack 'cont)
              (list (list (layer-make))
                    (env-cont-make+ return throw) )))

(define (env-varDefined? env id) '())

(define (env-assign! env lval value)
    (if (not (value-lvalue? lval))
      (iException (list "Trying to assign to rvalue."))
      (let ([boxed-rvalue (value-lvalue lval)])
        (begin
          (set-box! boxed-rvalue (value-torvalue value))
          env ))))

(define (env-getVar env id)
  (define (getVar stack id)
    (if (null? stack)
        (iException+ (list "Variable undeclared:" id))
        ((lambda (lval)
           (if (iException? lval)
               (getVar (cdr stack) id)
               lval ))
         (layer-getVar (car stack) id) )))

  (getVar (dict-get env 'stack) id) )

(define (env-defineVar env id value)
  (call/cc
   (lambda (throw)
     (dict-update+ env 'stack
     (lambda (stack)
       (let ([n-layer (layer-defineVar (car stack) id value)])
         (if (iException? n-layer)
             (throw n-layer)
             (cons n-layer (cdr stack)) )))))))

(define (env-pushLayer env)
  (dict-update+ env 'stack
                (lambda (stack)
                  (cons (layer-make) stack) )))

(define (env-popLayer env)
  (let ([stack (dict-get env 'stack)])
    (if (null? (cdr stack))
        (iException "Cannot pop last layer")
        (dict-update env 'stack (cdr stack)) )))

; ========= 'Continuation Manager' ========
; Allows constructs to dynamically introduce new
; continuations and access them, modify them
; continuations are considered part of the program
; state
(define (env-cont-make)
  (dict-make+ (list 'return 'throw)
              (list id id)))

(define (env-cont-make+ ret thr)
  (dict-make+ (list 'return 'throw)
              (list ret thr) ))

(define (env-cont-add env id k)
  (call/cc
   (lambda (throw)
     (dict-update+ env 'cont
     (lambda (cont)
       ((lambda (n-cont)
          (if (iException? n-cont)
              (throw n-cont)
              n-cont ))
        (dict-add cont id k) ))))))

(define (env-cont-remove env id)
  (dict-update+ env 'cont
  (lambda (cont)
    (dict-remove cont id) )))

(define (env-cont-get env id)
   (let ([cont-id (dict-get (dict-get env 'cont) id)])
     (if (iException? cont-id)
         (iException+ (list "Continuation" id "does not exist"))
         cont-id )))

(define (env-cont-update+ env id f)
  (call/cc
   (lambda (throw)
     (dict-update+ env 'cont
     (lambda (cont)
       ((lambda (new-cont)
          (if (iException? new-cont)
              (throw new-cont)
              new-cont))
        (dict-update+ cont id f) ))))))
     
; two default continuations: 'throw' and 'return'
(define (env-throw env val)
  ((env-cont-get env 'throw) env val))

(define (env-return env val)
  ((env-cont-get env 'return) env val))

(define (env-setThrow+ env f)
  ((env-cont-update+ 'throw f)) )

(define (env-setReturn+ env val)
  ((env-cont-update+ 'return f)))

; ========= 'layer' class =================
; Note due to involvement of "boxes" now
; 'layer' objects are stateful objects.
; in particular, modification of variable value
; in the layer class is a STATEFUL operation

(define layer-make dict-make)
(define (layer-make+ l1 l2)
  (foldl (lambda (id value layer)
           (layer-defineVar layer id value) )
         (layer-make)
         l1 l2))

(define (layer-varDefined? layer id)
  (dict-exisist? layer id) )

; Inserts a new variable into environment
; REQUIRE : Current Environment does not contain a definition
(define (layer-defineVar layer id value)
  (if (layer-varDefined? layer id)
      (iException+ (list "Multiple Definition: " id))
      (dict-add layer id (box (value-torvalue value))) ))

; Deletes an identifier from environment
; REQUIRE : Current env contains such id
(define (layer-undefVar layer id)
  (if (layer-varDefined? layer id)
      (dict-remove layer id)
      (iException (list "Trying to undef non-existent var:" id)) ))

; Assigns to an existing variable in environment
; REQUIRE : If the rvalue is already invalidated,
;           trying to assigning to this will result in undefined behavior!
(define (layer-assign! layer lval value)
  (if (not (value-lvalue? lval))
      (iException (list "Trying to assign to rvalue."))
      (let ([boxed-rvalue (value-lvalue lval)])
        (begin
          (set-box! boxed-rvalue (value-torvalue value))
          layer ))))

; Returns a lvalue
(define (layer-getVar layer id)
  (if (not (dict-exisist? layer id))
      (iException+ (list "Variable undeclared: " id))
        (lvalue-make (dict-get layer id))))
