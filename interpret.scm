; Group Members
; LAST NAME, First name
; YAO, Yue
; QI, Peiyuan
; YAO, Kaiqi

(require racket/trace)

(load "simpleParser.scm")
(load "env.scm")
(load "common.scm")
(load "datatype.scm")

;(define sampleProg (parser "test.javaish"))

; Two specially named erros
(define (err-unimplemented) "Feature unimplemented yet!")
(define (err-noValue) "Not a value")
; Two error-handlers
(define err-stdReport errorMsg)
(define (err-throw e)
    (error "Error:\n Interpretation Failed due to\n" (e)) )

; The Continuation type k
; Continuation contiains three execution path
; k-norm "The normal path"
; k-err "Path to follow when an error happened"
; k-prem "The pre-mature exit path, 'break;' or 'return;'"
; Now all continuations takes 2 arguments, the environment and the result value
(define (k-make k-norm k-err k-prem)
  (list k-norm k-err k-prem))

(define (k-norm k) (car k))
(define (k-err k) (cadr k))
(define (k-prem k) (caddr k))

(define (k-setNorm k k-norm) (cons k-norm (cdr k))) ; normal
(define (k-setPrem k k-prem) (list (k-norm k) (k-err k) k-prem)) ; break
(define (k-setErr k k-err) (list (k-norm k) k-err (k-prem k))) ; exception

; Executes a seriers of code
(define (exec stmt-list env k)
    (if (null? stmt-list)
        ((k-norm k) env (tvoid))
        (M-stat (car stmt-list) env (k-setNorm k 
        (lambda (e rst)
          (exec (cdr stmt-list) e k) )))))

(define (interpret file) (interpret! file err-throw))

(define (interpret! file)
  
  (define prog (parser file))
  (define env (env-make))

  (define (normal-cont env rst)
    (if (tvoid? rst)
        (Exception "Error: Missing return value!")
        (dispValue rst) ))

  (define (err-cont env ex)
    (begin
      (print "Stack trace:")
      (newline)
      (print env)
      (newline)
      (print "Error: Unhandeled exception")
      (newline)
      (print (value-v ex))
      "" ))
  
  (define k (k-make normal-cont
                    err-cont
                    normal-cont))
  
  (exec prog env k) )

; This is a cps function, the continuation is given in k
(define (M-stat statement env k)
  (match statement
    ('() ((k-norm k) env (tvoid))) ; empty statement
    (('+ expr1 expr2) (M-binary-left-oper t/+ expr1 expr2 env k))
    (('- expr1 expr2) (M-binary-left-oper t/- expr1 expr2 env k))
    (('- expr) (M-unary-oper t/neg expr env k))
    (('* expr1 expr2) (M-binary-left-oper t/* expr1 expr2 env k))
    (('/ expr1 expr2) (M-binary-left-oper t// expr1 expr2 env k))
    (('% expr1 expr2) (M-binary-left-oper t/% expr1 expr2 env k))
    (('== expr1 expr2) (M-binary-left-oper t/== expr1 expr2 env k))
    (('!= expr1 expr2) (M-binary-left-oper t/!= expr1 expr2 env k))
    (('> expr1 expr2) (M-binary-left-oper t/> expr1 expr2 env k))
    (('< expr1 expr2) (M-binary-left-oper t/< expr1 expr2 env k))
    (('<= expr1 expr2) (M-binary-left-oper t/<= expr1 expr2 env k))
    (('>= expr1 expr2) (M-binary-left-oper t/>= expr1 expr2 env k))
    (('&& expr1 expr2) (M-binary-left-oper t/&& expr1 expr2 env k))
    (('|| expr1 expr2) (M-binary-left-oper t/|| expr1 expr2 env k))
    (('! expr) (M-unary-oper t/! expr env k))
    (('if cond statT statF) (M-if cond statT statF env k))
    (('if cond statT) (M-if cond statT '() env k))
    (('while cond stmt) (M-while cond stmt env k))
    (('return expr) (M-return expr env k))
    (('var vname expr) (M-declare-expr vname expr env k))
    (('var vname) (M-declare vname (tvoid) env k))
    (('= sym expr) (M-assignment sym expr env k))
    (('begin _ ...) (M-block (cdr statement) env k))
    ((cons x y) (k-err (lambda () "Unrecogonized identifier")))
    ((? number? x) (M-num-literal x env k))
    (sym (M-symbol statement env k))
    (_ (k-err (lambda () "Match failed" ))) ))


(define (M-binary-left-oper op lhs rhs env k)
  (M-stat lhs env (k-setNorm k
  (lambda (lenv lrst)
    (M-stat rhs lenv (k-setNorm k
    (lambda (renv rrst)
      (let ([val (op lrst rrst)])
        (if (iException? val)
            ((k-err k) renv (val))
            ((k-norm k) renv val) )))))))))

(define (M-unary-oper op expr env k)
  (M-stat expr env (k-setNorm k
  (lambda (e rst)
    ((k-norm k) e (op rst)) ))))

(define (M-num-literal n env k)
  ((k-norm k) env (num n)))

(define (M-symbol sym env k)
  (cond
    [(equal? sym 'true) ((k-norm k) env (bool #t))]
    [(equal? sym 'false) ((k-norm k) env (bool #f))]
    [else (let ((var (env-getVar env sym)))
            (cond
              [(iException? var) ((k-err k) env (Exception (var)))]
              [(tvoid? var)
               ((k-err k) env (Exception+ (list "Use of uninitialized value:" sym)))]
              [else ((k-norm k) env var)] ))]))

(define (M-declare sym init env k)
  (let ([n-env (env-defineVar env sym init)])
      (if (iException? n-env)
          ((k-err k) env (Exception (n-env)))
          ((k-norm k) n-env (tvoid)) )))

(define (M-declare-expr sym expr env k)
  (M-stat expr env (k-setNorm k
  (lambda (e rst)
      (M-declare sym rst e k) ))))


; Unlike C/Java assignment returns rvalue!
(define (M-assignment sym expr env k)
  (M-stat expr env (k-setNorm k
  (lambda (e rst)
    (let ([lval (env-getVar e sym)])
      (if (iException? lval)
          ((k-err k) e (Exception (lval)))
          (let ([n-env (env-assign! e lval rst)])
            (if (iException? n-env)
                ((k-err k) n-env (Exception (n-env)))
                ((k-norm k) n-env rst)) )))))))

(define (M-return expr env k)
  (M-stat expr env (k-setNorm k
  (lambda (e rst)
    ((k-prem k) e (value-torvalue rst)) ))))

(define (M-if condition expr1 expr2 env k)
  (M-stat condition env (k-setNorm k
  (lambda (e b)
    (cond
      [(not (bool? b)) (Exception+ (list "Type Error: 'if' condiction expects 'boolean', got" (value-type b)))]
      [(bool-true? b) (M-stat expr1 e k)]
      [else (M-stat expr2 e k)] )))))

(define (M-while condition stmt env k)
  (M-stat condition env (k-setNorm k
  (lambda (e b)
    (cond
      [(not (bool? b)) (Exception+ (list "Type Error: 'while' condiction expects 'boolean', got" (value-type b)))]
      [(bool-true? b) (M-stat stmt e (k-setNorm k
                      (lambda (e-stat rst)
                        (M-while condition stmt e-stat k) )))]
      [else ((k-norm k) e (tvoid))] )))))

(define (M-block stmt env k)
  (if (null? stmt)
      ((k-norm k) env (tvoid))
      (M-stat (car stmt) (env-pushLayer env) (k-setNorm k
      (lambda (e v)
        (M-block (cdr stmt) e (k-setNorm k
        (lambda (e-end v-end)
          ((k-norm k) (env-popLayer e-end) v-end) ))))))))

(define (dispValue v) v)

;(trace interpret)
;(trace M-stat)

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
          (display (interpret! (make-path count)))
          (newline)
          (testall-helper (add1 count) max))
        (display "Test completed")))

  (testall-helper 1 36))