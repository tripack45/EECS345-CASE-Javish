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
  (if (not (dict? value)) #f
      (dict-exist? (dict-get value 'attr) 'lvalue)))

(define (value-lvalue value)
  (dict-get (dict-get value 'attr) 'lvalue))

(define (value-torvalue lvalue)
  (let ([rvalue (if (value-lvalue? lvalue)
                    (unbox (dict-get (dict-get lvalue 'attr) 'lvalue))
                    lvalue)])
    (if (Object? rvalue)
        (deepcopy rvalue)
        rvalue )) )

; ========= 'Env' class =============
; Closure is boxed object
; This allows for a recursive structure

(define (env-make)
  (dict-make+ (list 'closure 'cont)
              (list (box (closure-make '()))
                    (env-cont-make) )))

(define (env-make+ l1 l2)
  (dict-make+ (list 'closure 'cont)
              (list (box (closure-make+ l1 l2 '()))
                    (env-cont-make) )))

(define (env-make-cont return throw miss)
  (dict-make+ (list 'closure 'cont)
              (list (box (closure-make '()))
                    (env-cont-make+ return throw miss) )))

(define (env-varDefined? env id)
  (define (varDefined? boxed-closure id)
    (cond
      [(null? boxed-closure) #f]
      [(closure-varDefined? (unbox boxed-closure) id) #t]
      [else (varDefined? (closure-prev (unbox boxed-closure)) id)] ))
  (varDefined? (dict-get env 'closure) id) )

(define (env-varDefinedInClosure? env id)
  ((lambda (boxed-closure)
     (if (null? boxed-closure)
         #f
         (closure-varDefined? (unbox boxed-closure) id) ))
   (dict-get env 'closure) ))

(define (env-assign! env lval value)
   (begin
     (closure-assign! env lval value)
     env ))

(define (env-getVar env id)
  (define (getVar boxed-closure id)
    (if (null? boxed-closure)
        (iException+ 'undefined-ref (list "Env Variable undeclared:" id))
        (let ([closure (unbox boxed-closure)])
          (if (closure-varDefined? closure id)
              (closure-getVar closure id)
              (getVar (closure-prev closure) id) ))))
  (getVar (dict-get env 'closure) id) )

; =========================================
; ============== WARNING ==================
; =========================================
; Due to calls to `set-box!` now the following
; function calls are no longer side-effect-free
; I hope there is a better way to maintain the
; purity. Next time let's try monads.

(define (env-closureOp! op)
  (lambda (env id value)
    ((lambda (boxed-closure)
     (begin
       (set-box! boxed-closure
                 (op (unbox boxed-closure) id value ))
       env ))
     (dict-get env 'closure) )))

(define (env-layerOp! op)
  (lambda (env)
    ((lambda (boxed-closure)
     (begin
       (set-box! boxed-closure
                 (op (unbox boxed-closure)))
       env ))
     (dict-get env 'closure) )) )

(define (env-defineVar! env id value)
  ((env-closureOp! closure-defineVar) env id value) )

(define (env-defineRef! env id lvalue)
  ((env-closureOp! closure-defineRef) env id lvalue) )

(define (env-defineConst! env id value)
  ((env-closureOp! closure-defineConst) env id value))

(define (env-pushLayer! env)
  ((env-layerOp! closure-pushLayer) env) )

(define (env-popLayer! env)
  ((env-layerOp! closure-popLayer) env) )

(define (env-countTopClosureLayer env)
  (closure-countLayer (unbox (dict-get env 'closure))))

(define (env-countClosureDepth env)
  (letrec ([count (lambda (boxed-closure)
                    (if (null? boxed-closure)
                        0
                        (add1 (count (dict-get (unbox boxed-closure) 'prev))) ))])
    (count (dict-get env 'closure)) ))

; =========================================
; ============== END ======================
; =========================================

; Makes a new closure and links current closure to that closure
(define (env-pushClosure env)
  (dict-update env 'closure
               (box (closure-make (dict-get env 'closure) ))))

(define (env-getCurrentClosure env)
  (dict-get env 'closure) )

(define (env-replaceClosure env c)
  (dict-update env 'closure c) )

; ========= 'Closure' class =============
; 'Closure' objects are stateful now!

(define (closure-make prev)
  (dict-make+ (list 'stack 'prev)
              (list (list (layer-make))
                    prev )))

(define (closure-make+ l1 l2 prev)
  (dict-make+ (list 'stack 'prev)
              (list (list (layer-make+ l1 l2))
                    prev )))

(define (closure-prev c) (dict-get c 'prev))

(define (closure-assign! closure lval value)
   (begin
     (layer-assign! closure lval value)
     closure ))

(define (closure-varDefined? closure id)
  (define (varDefined? stack id)
    (cond
      [(null? stack) #f]
      [(layer-varDefined? (car stack) id) #t]
      [else (varDefined? (cdr stack) id)] ))
  (varDefined? (dict-get closure 'stack) id) )

(define (closure-getVar closure id)
  (define (getVar stack id)
    (if (null? stack)
        (iException+ 'undefined-ref (list "Closure: Variable undeclared:" id))
        ((lambda (layer)
           (if (layer-varDefined? layer id)
               (layer-getVar layer id)
               (getVar (cdr stack) id) ))
         (car stack) )))
  (getVar (dict-get closure 'stack) id) )

(define (closure-defineVar closure id value)
  (dict-update+ closure 'stack
  (lambda (stack)
    (cons (layer-defineVar (car stack) id value) (cdr stack)) )))

(define (closure-defineRef closure id lvalue)
  (dict-update+ closure 'stack
  (lambda (stack)
    (cons (layer-defineRef (car stack) id lvalue) (cdr stack)) )))

(define (closure-defineConst closure id value)
  (dict-update+ closure 'stack
  (lambda (stack)
    (cons (layer-defineConst (car stack) id value) (cdr stack)) )))

(define (closure-pushLayer closure)
  (dict-update+ closure 'stack
                (lambda (stack)
                  (cons (layer-make) stack) )))

(define (closure-popLayer closure)
  (let ([stack (dict-get closure 'stack)])
    (if (null? (cdr stack))
        (iException 'pop-last-layer "Cannot pop last layer")
        (dict-update closure 'stack (cdr stack)) )))

(define (closure-countLayer closure)
  (length (dict-get closure 'stack)))

; ========= 'Continuation Manager' ========
; Allows constructs to dynamically introduce new
; continuations and access them, modify them
; continuations are considered part of the program
; state
(define (env-cont-make)
  (dict-make+ (list 'return 'throw 'continue 'break)
              (list id id id id)))

(define (env-cont-make+ ret thr miss)
  (dict-make+ (list 'return 'throw 'continue 'break)
              (list ret
                    thr
                    (lambda (env rst) (miss 'continue env rst))
                    (lambda (env rst) (miss 'break env rst))  )))

(define (env-cont-exists? env id)
  (dict-exist? (dict-get env 'cont) id))

(define (env-cont-add env id k)
  (dict-update+ env 'cont
                (lambda (cont) (dict-add cont id k)) ))

(define (env-cont-remove env id)
  (dict-update+ env 'cont
                (lambda (cont) (dict-remove cont id)) ))

(define (env-cont-get env id)
  (dict-get (dict-get env 'cont) id) )

(define (env-cont-update+ env id f)
  (dict-update+ env 'cont
                (lambda (cont) (dict-update cont id f)) ))

(define (env-cont-map env f)
  (dict-update+ env 'cont
                (lambda (cont) (dict-map cont f))))

(define (env-cont update env id cont)
  (dict-update+ env id (lambda (k) cont)) )

(define (env-cont-save env id)
  ((lambda (cont)
     (if (dict-exist? cont id)
         (dict-get cont id)
         (list id (tvoid)) ))
   (dict-get env 'cont) ))

(define (env-cont-restore env saved-cont)
  (let* ([id (car saved-cont)]
         [cont (cadr saved-cont)]
         [e (env-cont-remove env id)])
    (if (not (tvoid? cont))
        (env-cont-add e id cont)
        e )))

(define (env-cont-saveall env)
  (dict-get env 'cont))

(define (env-cont-restoreall env cont)
  (dict-update+ env 'cont
                (lambda (c) cont) ))

; Create or update current continuation
(define (env-cont-patch env id cont)
  (env-cont-add (env-cont-remove env id) id cont))

(define (env-follow cont-id env val)
  (if (env-cont-exists? env cont-id)
      (let ([cont (env-cont-get env cont-id)])
        (cont env val))
      (env-throw env (Exception+ (list "There is no place to" cont-id))) ))


; two default continuations: 'throw' and 'return'
; two default goto's: 'continue' and 'break'

(define (env-throw env val) ((env-cont-get env 'throw) env val))
(define (env-return env val) ((env-cont-get env 'return) env val))
(define (env-continue env val) ((env-cont-get env 'continue) env val))
(define (env-break env val) ((env-cont-get env 'break) env val))

(define (env-setThrow+ env f) (env-cont-update+ env 'throw f))
(define (env-setReturn+ env f) (env-cont-update+ env 'return f) )
(define (env-setContinue+ env f) (env-cont-update+ env 'continue f) )
(define (env-setBreak+ env f) (env-cont-update+ env 'break f) )

;(define (env-saveThrow env) (env-cont-save env 'throw))
;(define (env-saveReturn env) (env-cont-save env 'return))
;(define (env-saveThrow env) (env-cont-save env 'throw))
;(define (env-savereTurn env) (env-cont-save env 'return))

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
  (dict-exist? layer id) )

; Inserts a new variable into environment
; REQUIRE : Current Environment does not contain a definition
(define (layer-defineVar layer id value)
  (if (layer-varDefined? layer id)
      (iException+ 'multidef (list "Multiple Definition: " id))
      (dict-add layer id
                (list (box (value-torvalue value))
                      (set-make) ))))

; Inserts a new identifier that reference to an exisiting
; variable into environment
; REQUIRE : Current Environment does not contain a definition
(define (layer-defineRef layer id lvalue)
  (if (layer-varDefined? layer id)
      (iException+ 'multidef (list "Multiple Definition: " id))
      (if (not (value-lvalue? lvalue))
          (iException+ 'expected-lval (list "Cannot create reference" id "to a rvalue"))
          (dict-add layer id
                    (list (value-lvalue lvalue)
                          (set-make+ '(reference)) )))))

; Inserts a new identifier that associates with an const
; A const is a variable that cannot be assigned to in any way
; variable into environment
; REQUIRE : Current Environment does not contain a definition
(define (layer-defineConst layer id value)
  (if (layer-varDefined? layer id)
      (iException+ 'multidef (list "Multiple Definition: " id))
      (dict-add layer id
                (list (box (value-torvalue value))
                      (set-make+ '(const)) ))))

; Deletes an identifier from environment
; REQUIRE : Current env contains such id
(define (layer-undefVar layer id)
  (if (layer-varDefined? layer id)
      (dict-remove layer id)
      (iException+ 'undefined-ref
                  (list "Trying to undef non-existent var:" id)) ))

; Assigns to an existing variable in environment
; REQUIRE : If the rvalue is already invalidated,
;           trying to assigning to this will result in undefined behavior!
(define (layer-assign! layer lval value)
  (if (not (value-lvalue? lval))
      (iException+ 'expected-lval (list "Trying to assign to rvalue."))
      (let ([boxed-rvalue (value-lvalue lval)])
        (begin
          (set-box! boxed-rvalue (value-torvalue value))
          layer ))))

; Returns a lvalue
(define (layer-getVar layer id)
  (if (not (dict-exist? layer id))
      (iException+ 'undefined-ref
                   (list "Variable undeclared: " id))
      ((lambda (var)
         (if (set-memberOf? (cadr var) 'const)
             (unbox (car var))
             (lvalue-make (car var)) ))
       (dict-get layer id) )))
