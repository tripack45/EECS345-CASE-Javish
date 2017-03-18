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

; Executes a seriers of code
(define (exec stmt-list env k)
    (if (null? stmt-list)
        (k env (tvoid))
        (M-stat (car stmt-list) env  
                (lambda (e rst)
                  (exec (cdr stmt-list) e k) ))))

(define (interpret file) (interpret! file err-throw))

(define (interpret! file)

  (define (normal-cont env rst)
    (if (tvoid? rst)
        (Exception "Error: Missing return value!")
        (begin
          ;(print "Env:") (newline) (print env) (newline)
          (dispValue rst)) ))

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

  (define prog (parser file))
  (define env (env-make-cont normal-cont
                             err-cont
                             (lambda (id env rst)
                               (err-cont env (Exception+ (list "No where to" id))) )))
  
  (exec prog env normal-cont) )

; This is a cps function, the continuation is given in k
(define (M-stat statement env k)
  (match statement
    ('() (k env (tvoid))) ; empty statement
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
    (('break) (M-break env k))
    (('continue) (M-continue env k))
    ((cons x y) (k-err (lambda () "Unrecogonized identifier")))
    ((? number? x) (M-num-literal x env k))
    (sym (M-symbol statement env k))
    (_ (k-err (lambda () "Match failed" ))) ))


(define (M-binary-left-oper op lhs rhs env k)
  (M-stat lhs env 
          (lambda (lenv lrst)
            (M-stat rhs lenv
                    (lambda (renv rrst)
                      (let ([val (op lrst rrst)])
                        (if (iException? val)
                            (env-throw renv (Exception (iException-str val)))
                            (k renv val) )))))))

(define (M-unary-oper op expr env k)
  (M-stat expr env 
          (lambda (e rst) (k e (op rst)) )))

(define (M-num-literal n env k)
  (k env (num n)))

(define (M-symbol sym env k)
  (cond
    [(equal? sym 'true) (k env (bool #t))]
    [(equal? sym 'false) (k env (bool #f))]
    [else (let ((var (env-getVar env sym)))
            (cond
              [(iException? var) (env-throw env (Exception (iException-str var)))]
              [(tvoid? var)
               (env-throw env (Exception+ (list "Use of uninitialized value:" sym)))]
              [else (k env var)] ))]))

(define (M-declare sym init env k)
  (let ([n-env (env-defineVar env sym init)])
      (if (iException? n-env)
          (env-throw env (Exception (iException-str n-env)))
          (k n-env (tvoid)) )))

(define (M-declare-expr sym expr env k)
  (M-stat expr env 
          (lambda (e rst) (M-declare sym rst e k) )))


; Unlike C/Java assignment returns rvalue!
(define (M-assignment sym expr env k)
  (M-stat expr env 
  (lambda (e rst)
    (let ([lval (env-getVar e sym)])
      (if (iException? lval)
          (env-throw e (Exception (iException-str lval)))
          (let ([n-env (env-assign! e lval rst)])
            (if (iException? n-env)
                (env-throw n-env (Exception (iException-str n-env)))
                (k n-env rst)) ))))))

(define (M-return expr env k)
  (M-stat expr env 
          (lambda (e rst)
            (env-return e (value-torvalue rst)) )))

(define (M-if condition expr1 expr2 env k)
  (M-stat condition env 
  (lambda (e b)
    (cond
      [(not (bool? b)) (Exception+ (list "Type Error: 'if' condiction expects 'boolean', got" (value-type b)))]
      [(bool-true? b) (M-stat expr1 e k)]
      [else (M-stat expr2 e k)] ))))

(define (M-while condition stmt env k)
  (define k-save (env-cont-saveall env))

  (define k-finalize
    (lambda (env) (env-cont-restoreall env k-save) ))

  (define k-continue
    (lambda (env rst)
      (M-stat condition env 
              (lambda (e b)
                (cond
                  [(not (bool? b)) (Exception+ (list "Type Error: 'while' condiction expects 'boolean', got" (value-type b)))]
                  [(bool-true? b) (M-stat stmt e 
                                          (lambda (e-stat rst)
                                            (M-while condition stmt e-stat k) ))]
                  [else (k (k-finalize e) (tvoid))] )))))

  (define k-break (lambda (env rst) (k (k-finalize env) rst)))

  (define append-finalize
    (lambda (k-prev)
      (lambda (e v)
        (k-prev (k-finalize e) v) )))
 
  (let* ([env0 (env-cont-patch env 'break k-break)]
         [env1 (env-cont-patch env0 'continue k-continue)]
         [env2 (env-setThrow+ env1 append-finalize)]
         [env3 (env-setReturn+ env1 append-finalize)])
    (k-continue env3 (tvoid)) ))
        
(define (M-block stmt env k)

  (define k-save (env-cont-saveall env))

  (define (finalize e)
    (env-popLayer (env-cont-restoreall e k-save)))
    
  (define append-finalize
    (lambda (k-prev)
      (lambda (e v)
        (k-prev (finalize e) v) )))

  (define (executeInLayer stmt env k)
    (if (null? stmt)
        (k env (tvoid))
        (M-stat (car stmt) env
                (lambda (e v)
                  (executeInLayer (cdr stmt) e k)))))
  
  (let* ([env0 (env-setBreak+ env append-finalize)]
         [env1 (env-setContinue+ env0 append-finalize)]
         [env2 (env-setThrow+ env1 append-finalize)]
         [env3 (env-setReturn+ env2 append-finalize)]
         [env4 (env-pushLayer env3)])
    (if (null? stmt)
        (k e (tvoid))
        (executeInLayer stmt env4 (append-finalize k)) )))

(define (M-break env k)
  (env-follow 'break env (tvoid)) )

(define (M-continue env k)
  (env-follow 'continue env (tvoid) ))

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
  ;(testall-helper 39 39))
  (testall-helper 1 43))

(testall)