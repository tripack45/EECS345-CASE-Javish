(require racket/trace)

(load "simpleParser.scm")
(load "env.scm")
(load "common.scm")

(define sampleProg (parser "test.javaish"))

; The result type
; Result type encloses information about the execution result
; Execution result includes the updated environment and a value
(define (result-make env value)
  (list env value))

(define (result-env result) (car result))
(define (result-value result) (cadr result))

; Two specially named erros
(define (err-unimplemented) "Feature unimplemented yet!")
(define (err-noValue) "Not a value")

; The Continuation type k
; Continuation contiains three execution path
; k-norm "The normal path"
; k-err "Path to follow when an error happened"
; k-prem "The pre-mature exit path, 'break;' or 'return;'"

(define (k-make k-norm k-err k-prem)
  (list k-norm k-err k-prem))

(define (k-norm k) (car k))
(define (k-err k) (cadr k))
(define (k-prem k) (caddr k))

(define (k-setNorm k k-norm) (cons k-norm (cdr k)))
(define (k-setPrem k k-prem) (list (k-norm k) (k-err k) k-prem))
(define (k-setErr k k-err) (list (k-norm k) k-err (k-prem k)))

; Executes a seriers of code
(define (exec stmt-list env k)
    (if (null? stmt-list)
        ((k-norm k) env)
        (M-stat (car stmt-list) env (k-setNorm k 
        (lambda (rst)
          (exec (cdr stmt-list) (result-env rst) k) )))))

(define (interpret file)
  
  (define prog (parser file))
  (define env (env-make))

  (define (normal-cont rst)
    (let ([terminate-env (result-env rst)])
      (if (or (env-hasReturn? terminate-env)
              (error? (env-return terminate-env)))
          (env-return terminate-env)
          (error-handler (lambda () "Error: Missing return value!")))))

  (define err-cont err-stdReport)
  
  (define k (k-make normal-cont
                    err-cont
                    normal-cont))
  
  (exec prog env k) )

; This is a cps function, the continuation is given in k
(define (M-stat statement env k)
  (match statement
    ('() ((k-norm k) (result-make env err-noValue))) ; empty statement
    (('+ expr1 expr2) (M-binary-left-oper + expr1 expr2 env k))
    (('- expr1 expr2) (M-binary-left-oper - expr1 expr2 env k))
    (('- expr) (M-unary-oper - expr env k))
    (('* expr1 expr2) (M-binary-left-oper * expr1 expr2 env k))
    (('/ expr1 expr2) (M-binary-left-oper quotient expr1 expr2 env k))
    (('% expr1 expr2) (M-binary-left-oper modulo expr1 expr2 env k))
    (('== expr1 expr2) (M-binary-left-oper = expr1 expr2 env k))
    (('!= expr1 expr2) (M-binary-left-oper != expr1 expr2 env k))
    (('> expr1 expr2) (M-binary-left-oper > expr1 expr2 env k))
    (('< expr1 expr2) (M-binary-left-oper < expr1 expr2 env k))
    (('<= expr1 expr2) (M-binary-left-oper <= expr1 expr2 env k))
    (('>= expr1 expr2) (M-binary-left-oper >= expr1 expr2 env k))
    (('&& expr1 expr2) (M-binary-left-oper land expr1 expr2 env k))
    (('|| expr1 expr2) (M-binary-left-oper lor expr1 expr2 env k))
    (('! expr) (M-unary-oper not expr env k))
    (('if cond statT statF) (M-if cond statT statF env k))
    (('if cond statT) (M-if cond statT '() env k))
    (('while cond stmt) (M-while cond stmt env k))
    (('return expr) (M-return expr env k))
    (('var vname expr) (M-declare-expr vname expr env k))
    (('var vname) (M-declare vname var-initVal env k))
    (('= sym expr) (M-assignment sym expr env k))
    ((cons x y) (k-err (lambda () "Unrecogonized identifier")))
    ((? number? x) (M-num-literal x env k))
    (sym (M-symbol statement env k))
    (_ (k-err (lambda () "Match failed" ))) ))    

(define (M-binary-left-oper op lhs rhs env k)
  (M-stat lhs env (k-setNorm k
  (lambda (lrst)
    (M-stat rhs (result-env lrst) (k-setNorm k
    (lambda (rrst)
      ((k-norm k) (result-make (result-env rrst)
                               (op (result-value lrst)
                                   (result-value rrst) ))))))))))

(define (M-unary-oper op expr env k)
  (M-stat expr env (k-setNorm k
  (lambda (rst)
    ((k-norm k) (result-make (result-env rst)
                             (op (result-value rst)) ))))))

(define (M-num-literal n env k)
  ((k-norm k) (result-make env n)))

(define (M-symbol sym env k)
  (cond
    [(equal? sym 'false) ((k-norm k) (result-make env #f))]
    [(equal? sym 'true) ((k-norm k) (result-make env #t))]
    [else (let ((var (env-getVar env sym)))
            (cond
              ((error? var) ((k-err k) var))
              ((error? (var-value var))
               ((k-err k) (error-make "Use of uninitialized value: " (var-name var))) )
              (else ((k-norm k) (result-make env (var-value var)))) ))]))

(define (M-declare sym init env k)
  (let ((var (var-make sym init)))
    (let ((n-env (env-defineVar env var)))
      (if (error? n-env)
          ((k-err k) n-env)
          ((k-norm k) (result-make n-env err-noValue)) ))))

(define (M-declare-expr sym expr env k)
  (M-stat expr env (k-setNorm k
  (lambda (rst)
    (M-declare sym
               (result-value rst)
               (result-env rst)
               k )))))

(define (M-assignment sym expr env k)
  (M-stat expr env (k-setNorm k
  (lambda (rst)
    (let ((n-env (env-assignToVar (result-env rst)
                                  sym
                                  (result-value rst) )))
      (if (error? n-env) ((k-err k) n-env)
          ((k-norm k) (result-make n-env (result-value rst))) ))))))

(define (M-return expr env k)
  (M-stat expr env (k-setNorm k
  (lambda (rst)
    (let ((n-env (env-setReturn (result-env rst)
                                (result-value rst) )))
      (if (error? n-env) (k-err n-env)
          ((k-prem k) (result-make n-env err-noValue)) ))))))

(define (M-if condition expr1 expr2 env k)
  (M-stat condition env (k-setNorm k
  (lambda (bool)
    (cond
      [(eq? (result-value bool) #t)
       (M-stat expr1 (result-env bool) k)]
      [(eq? (result-value bool) #f)
       (M-stat expr2 (result-env bool) k)]
      [else ((k-err k)
             (lambda () "Type Error, if condition is not boolean")) ])))))

(define (M-while condition stmt env k)
  (M-stat condition env (k-setNorm k
  (lambda (bool)
    (cond
      [(eq? (result-value bool) #t)
       (M-stat stmt (result-env bool) (k-setNorm k
       (lambda (rst)
         (M-while condition stmt (result-env rst) k) )))]
      [(eq? (result-value bool) #f)
       ((k-norm k) (result-make (result-env bool) err-noValue))]
      [else ((k-err k)
             (lambda () "Type Error, if condition is not boolean") )])))))

(define err-stdReport errorMsg)
(define (err-throw e)
    (error "Intepretation Failed:" (e)) )

; (trace interpret)
; (trace M-stat)

(define (testall)
  (define pre "tests/")
  (define ext ".javaish")

  (define (make-path i)
    (string-append pre (number->string i) ext))

  (define (testall-helper count max)
    (if (<= count max)
        (begin
          (display count)
          (display ':)
          (display (interpret (make-path count)))
          (newline)
          (testall-helper (add1 count) max))
        (display "Test completed")))

  (testall-helper 1 29))