; Group Members
; LAST NAME, First name
; YAO, Yue
; QI, Peiyuan
; YAO, Kaiqi

(require racket/trace)

(load "functionParser.scm")
(load "env.scm")
(load "common.scm")
(load "datatype.scm")

;(define sampleProg (parser "test.javaish"))

(define (interpret file)

  (define prog (parser file))
  
  (define env (env-make-cont return-continuation
                             exception-continuation
                             exception-missing-cont ))

  (M-runner prog env fall-through-continuation) )

(define (fall-through-continuation e v)
  (if (tvoid? v)
      (begin
        (display "Execution Complete, no return value.")
        (newline) )
      (begin
        (display (tostring v)) )))

(define (return-continuation e v)
  (display (tostring v)))

(define (exception-continuation e v)
  (error
   (if (Exception? v)
       (string-append "\nUnhandled exception:\n"
                      (tostring v) )
       (string-append "\nUnhandled exception:\n"
                      "Exception Payload: " (tostring v) ))))

(define (exception-missing-cont id e v)
  (exception-continuation e (Exception+ (list "No place to" id) )))

; Evaluates the top most layer and executes the main function
(define (M-runner top-stat env k)
  (exec top-stat env
        (lambda (newEnv r)
          (M-callFunction 'main '() newEnv k) )))

; This is a cps function, the continuation is given in k
(define (M-stat statement env k)
  (match statement
    ('() (k env (tvoid))) ; empty statement
    (('function fname arglist body) (M-defFunction fname arglist body env k))
    (('funcall fname arglist ...) (M-callFunction fname arglist env k))
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
    (('try t-stmt '() ('finally f-stmt)) (M-try-no-catch t-stmt f-stmt env k))
    (('try t-stmt ('catch (id) c-stmt) '()) (M-try-no-finally t-stmt id c-stmt env k))
    (('try t-stmt ('catch (id) c-stmt) ('finally f-stmt)) (M-try t-stmt id c-stmt f-stmt env k))
    (('return expr) (M-return expr env k))
    (('throw expr) (M-throw expr env k))
    (('var vname expr) (M-declare-expr vname expr env k))
    (('var vname) (M-declare vname (tvoid) env k))
    (('= sym expr) (M-assignment sym expr env k))
    (('begin _ ...) (M-block (cdr statement) env k))
    (('break) (M-break env k))
    (('continue) (M-continue env k))
    ((cons x y) (error "Unrecogonized identifier"))
    ((? number? x) (M-num-literal x env k))
    (sym (M-symbol statement env k))
    (_ (Exception "Match failed")) ))


; Executes a seriers of code
(define (exec stmt-list env k)
    (if (null? stmt-list)
        (k env (tvoid))
        (M-stat (car stmt-list) env  
                (lambda (e rst)
                  (exec (cdr stmt-list) e k) ))))

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
  (let ([n-env (env-defineVar! env sym init)])
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
 
  (k-continue (env-cont-map env
                            (lambda (key cont)
                              (cond
                                [(eq? key 'break) k-break]
                                [(eq? key 'continue) k-continue]
                                [(eq? key 'throw) (append-finalize cont)]
                                [(eq? key 'return) (append-finalize cont)]
                                [else cont] )))
              (tvoid) ))
        
(define (M-block stmt env k)

  (define k-save (env-cont-saveall env))

  (define (finalize e)
    (env-popLayer! (env-cont-restoreall e k-save)))
    
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
  
  (let* ([env0 (env-cont-map env
                             (lambda (key cont)
                               (append-finalize cont)))]
         [env4 (env-pushLayer! env0)])
    (if (null? stmt)
        (k env (tvoid))
        (executeInLayer stmt env4 (append-finalize k)) )))

(define (M-break env k)
  (env-follow 'break env (tvoid)) )

(define (M-continue env k)
  (env-follow 'continue env (tvoid) ))

(define (M-throw expr env k)
  (M-stat expr env
          (lambda (e v)
            (env-throw e (value-torvalue v) ))))


; Must take care of corner cases:
; Break inside a catch-block etc.
; Need to take care of the scoping of catch(e)
(define (M-try t-stmt id c-stmt f-stmt env k-break)

  (define k-save (env-cont-saveall env))
  
  (define (M-try-block stmt env k)
   
    (define (exit-try e k)
      (k (env-cont-restoreall e k-save)) )
    (define modify-throw
      (lambda (k-throw)
      (lambda (e v)
        (exit-try e ; first clean up try block
        (lambda (exited-env)
          (M-catch-block c-stmt id v exited-env
          (lambda (e2 v2)
            (M-finally-block f-stmt e2 k-break) ))))))) 
    (define modify-other
      (lambda (k-prev)
        (lambda (e v)
          (exit-try e
          (lambda (exited-env)
            (M-finally-block f-stmt exited-env k-prev) )))))
    (define (introduce-try env k)
      (k (env-cont-map env
                       (lambda (key cont)
                         (cond
                           [(eq? key 'throw) (modify-throw cont)]
                           [(eq? key 'return) (modify-other cont)]
                           [(eq? key 'continue) (modify-other cont)]
                           [(eq? key 'break) (modify-other cont)]
                           [else cont] )))))

    (introduce-try env
                   (lambda (env-try)
                     (M-block stmt env-try
                              (lambda (e v)
                                (exit-try e
                                          (lambda (exited-e)
                                            (k exited-e v))) )))))

  
  (define (M-catch-block stmt id rst env k)
    
    (define (exit-catch e k)
      (k (env-popLayer! (env-cont-restoreall e k-save))) )
    (define modify-return/throw ; These two must preserve the passed value!
      (lambda (k-prev)
        (lambda (e v)
          (exit-catch e
                      (lambda (exited-env)
                        (M-finally-block f-stmt exited-env
                                         (lambda (ef vf)
                                           (k-prev ef v) )))))))
    (define modify-all
      (lambda (k-prev)
      (lambda (e v)
        (exit-catch e ; first clean up try block
        (lambda (exited-env)
          (M-finally-block f-stmt exited-env k-prev) )))))
    (define (introduce-catch id rst env k)
      (let* ([envp (env-cont-map env
                                 (lambda (key cont)
                                   (cond
                                     [(eq? key 'throw) (modify-return/throw cont)]
                                     [(eq? key 'return) (modify-return/throw cont)]
                                     [(eq? key 'continue) (modify-all cont)]
                                     [(eq? key 'break) (modify-all cont)]
                                     [else cont] )))]
             [env4 (env-pushLayer! envp)]
             [env5 (env-defineVar! env4 id rst)])
        (k env5) ))
    
    (introduce-catch id rst env
                     (lambda (env-catch)
                       (M-block stmt env-catch
                                (lambda (e v)
                                  (exit-catch e
                                              (lambda (exited-env)
                                                (k exited-env v) )))))))

  (define (M-finally-block stmt env k)
    (M-block stmt env k) )

  (M-try-block t-stmt env
               (lambda (e v)
                 (M-finally-block f-stmt e k-break) )))
  

(define (M-try-no-catch t-stmt f-stmt env k-break)

  (define k-save (env-cont-saveall env))

  (define (M-try-block stmt env k)
   
    (define (exit-try e k)
      (k (env-cont-restoreall e k-save)) )
    (define modify-throw
      (lambda (k-throw)
      (lambda (e v)
        (exit-try e ; first clean up try block
          (lambda (e2 v2)
            (M-finally-block f-stmt e2 k-throw) )))))
    (define modify-other
      (lambda (k-prev)
        (lambda (e v)
          (exit-try e
          (lambda (exited-env)
            (M-finally-block f-stmt exited-env k-prev) )))))
    
    (define (introduce-try env k)
      (k (env-cont-map env
                       (lambda (key cont)
                         (cond
                           [(eq? key 'throw) (modify-throw cont)]
                           [(eq? key 'return) (modify-other cont)]
                           [(eq? key 'continue) (modify-other cont)]
                           [(eq? key 'break) (modify-other cont)]
                           [else cont] )))))

    (introduce-try env
                   (lambda (env-try)
                     (M-block stmt env-try
                              (lambda (e v)
                                (exit-try e
                                          (lambda (exited-env)
                                            (k exited-env v) )))))))

  (define (M-finally-block stmt env k)
    (M-block stmt env k) )

  (M-try-block t-stmt env
               (lambda (e v)
                 (M-finally-block f-stmt e k-break) )))

(define (M-try-no-finally t-stmt id c-stmt env k)
  (M-try t-stmt id c-stmt '() env k) )


; We need to verfiy that there is no name confilct in arguments...
; This ensures the binding of arguments always succeeds.
(define (M-defFunction fname arg body env k)

  (define (packArguments argList)
    (cond
      [(null? argList) '()]
      [(equal? (car argList) '&)
       (cons (cons '& (cadr argList))
             (packArguments (cddr argList)))]
      [else (cons (car argList)
                  (packArguments (cdr argList)) )]))

  (let ([arglist (packArguments arg)])
    (if (not (unique? arglist))
        (env-throw env (Exception+
                        (list "Error in defining function" fname "\n"
                              "Argument list contains two identical names") ))
        (let* ([closure (env-getCurrentClosure env)]
               [newFunction (Function arglist body closure)]
               [n-env (env-defineVar! env fname newFunction)])
          (if (iException? n-env)
              (env-throw env (Exception+
                              (list "Identifier for function" fname
                                    "is already used!") ))
              (k env (tvoid)) )))))
  
; Steps in a function call:
; 0. Saves current closure
; 1. Request a valid callee from the state
; 2. Verify the number of arguments is correct
; 3. Evaluate all argument (this must happen here)
;    Since the evaluation must happen in current closure
; ==== Following operation cannot be interrupted ======
; ==== otherwise correctness is not guaranteed ========
; 4. Use the callee's closure to replace current closure
; 5. Push a new closure, (automatically a new layer)
; 6. Bind the real arguments into formal argument
; 7. Modify the continuations :
;    * break / continue : clean up, and throw
;    * return :  clean up, and invoke k
;    * throw : clean up, and invoke throw
; 
; -1. Clean up:
;     * Restore the original closure to replace
;     * Restore the the original continuation
;     * There is no need to "destroy" the old one (like layer).
;       Some function may be using it / leave it to GC

(define (M-callFunction fname realArg env k)

  (define currentClosure (env-getCurrentClosure env))

  (define k-save (env-cont-saveall env))

  (define (getCallee return)
    ((lambda (callee)
       (cond
         [(iException? callee)
          (env-throw env (Exception+ (list "Undefined reference to function" fname)))]
         [(not (Function? callee))
          (env-throw env (Exception+ (list fname "is not a function!")))]
         [else (return callee)] ))
     (env-getVar env fname)))

  ; Note order of evaluation is undefined
  ; We assume left-to-right evaluation
  ; Retuns (env-after-eval, evaluated arglist)
  (define (eval-arguments argList env k)
    (if (null? argList)
        (k env '())
        (M-stat (car argList) env
                (lambda (newEnv arg0)
                  (eval-arguments (cdr argList) newEnv
                                  (lambda (finalEnv restArgList)
                                    (k finalEnv (cons arg0 restArgList)) ))))))

  ; Assumes formalArg and realArg has the same number of arguments
  (define (bindArguments formalArg realArg env k)
    (if (null? formalArg) 
        (k env (tvoid))
        ((lambda (newEnv)
           (if (iException? newEnv)
               (env-throw (env-replaceClosure env currentClosure)
                          (Exception "Cannot take reference of rvalues"))
               (bindArguments (cdr formalArg)
                              (cdr realArg)
                              newEnv k )))
         (let ([farg (car formalArg)]
               [rarg (car realArg)])
           (if (pair? farg) ; We have a reference
               (env-defineRef! env (cdr farg) rarg)
               (env-defineVar! env (car formalArg) (car realArg)) )))))

  (define (exit-call env k)
    (k (env-replaceClosure (env-cont-restoreall env k-save) currentClosure)))

  (define (modify-break/continue cont)
    (lambda (env rst)
      (exit-call env
                 (lambda (restored-env)
                   (env-throw restored-env
                              (Exception "Error: Break/contiue not in a loop") )))))

  (define (modify-throw/return cont)
   (lambda (env rst)
     (exit-call env
                (lambda (restored-env)
                  (cont restored-env rst) ))))
  
  (define (introduceCall env kp)
    (kp (env-cont-map env
                      (lambda (key cont)
                        (cond
                          [(eq? key 'throw) (modify-throw/return cont)]
                          [(eq? key 'return) (modify-throw/return k)]
                          [(eq? key 'continue) (modify-break/continue cont)]
                          [(eq? key 'break) (modify-break/continue cont)]
                          [else cont] )))))
    
  
  (getCallee ; 1
  (lambda (callee)
    (let ([formalArg (Function-arg callee)])
      (if (not (equal? (length formalArg) ; 2
                       (length realArg)))
          (env-throw env (Exception+ (list "Function" fname "called with wrong number of arguments\n"
                                           "Expecting" (length formalArg)
                                           "| Got" (length realArg))))
          (eval-arguments realArg env ; 3
          (lambda (evaledEnv realArgValue)
            (let* ([calleePreClosure (env-replaceClosure evaledEnv (Function-closure callee))]
                   [calleeClosureEnv (env-pushClosure calleePreClosure)]) ; 4 5
              (bindArguments formalArg realArgValue calleeClosureEnv ; 6
               (lambda (bindedEnv v)
                 (introduceCall bindedEnv ; 7
                 (lambda (calleeEnv)
                   (exec (Function-body callee) calleeEnv
                   (lambda (terminateEnv value) ; -1, no return called internally
                     (env-return terminateEnv (tvoid)) ))))))))))))))
                                                                 


(define (dispValue v) v)

;(trace interpret)
;(trace M-stat)

(define (interpret! file)

  (define (normal-cont env rst)
    (if (tvoid? rst)
        (display "No return value!")
        (begin
          ;(print "Env:") (newline) (print env) (newline)
          (tostring rst)) ))

  (define (err-cont env ex)
    (begin
      ;(print "Stack trace:")
      ;(newline)
      ;(print env)
      ;(newline)
      (display "Unhandeled exception: ")
      (display (value-v ex))
      "" ))

  (define prog (parser file))
  (define env (env-make-cont normal-cont
                             err-cont
                             (lambda (id env rst)
                               (err-cont env (Exception+ (list "No where to" id))) )))
  
  (M-runner prog env normal-cont) )

(define (testall)
  (define pre "tests/")
  (define ext ".javaish")

  (define (make-path i)
    (string-append pre (number->string i) ext))

  (define (testall-helper count max)
    (if (<= count max)
        (begin
          (display count)
          (display ": ")
          (display (interpret! (make-path count)))
          (newline)
          (testall-helper (add1 count) max))
        (display "Test completed")))
  ;(testall-helper 52 52))
  (testall-helper 1 77))

(testall)