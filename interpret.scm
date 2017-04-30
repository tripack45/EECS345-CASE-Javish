; Group Members
; LAST NAME, First name
; YAO, Yue
; QI, Peiyuan
; YAO, Kaiqi

(require racket/trace)
(require racket/pretty)

(load "classParser.scm")
(load "env.scm")
(load "common.scm")
(load "datatype.scm")

;(define sampleProg (parser "test.javaish"))

(define (defaultIEHandler env)
  (lambda (ie) (env-throw env (Exception (cadr ie)))))

(define (interpret file class)

  (define prog (append (parser file)
                       (list (list 'return (list 'funcall
                                                 (list 'dot
                                                       (string->symbol class)
                                                       'main) )))))
  
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
  (begin
    (newline)
    (pretty-display env)
    (displayln statement)
;    (displayln (if (env-varDefined? env 'x)
;                   (value-torvalue (env-getVar env 'x))
;                   "Not Exists"))
;    (displayln (if (env-varDefined? env 'i)
;                   (value-torvalue (env-getVar env 'i))
;                   "Not Exists"))
;    (displayln (if (env-varDefined? env 'j)
;                   (value-torvalue (env-getVar env 'j))
;                   "Not Exists"))
;    (displayln (env-countTopClosureLayer env))
;    (displayln (env-countClosureDepth env))
  (match statement
    ('() (k env (tvoid))) ; empty statement
    (('class cname inheritance body) (M-declareClass cname inheritance body env k))
    (('new cname) (M-operatorNew cname env k))
    (('dot prefix attr) (M-dot-prop prefix attr env k))
    (('funcall ('dot prefix method) arglist ...) (M-dot-method prefix method arglist env k))
    (('function fname arglist body) (M-defFunction fname arglist body '() env k))
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
    ((cons x y) (begin (newline) (display statement) (error "Unrecogonized identifier")))
    ((? number? x) (M-num-literal x env k))
    (sym (M-symbol statement env k))
    (_ (Exception "Match failed")) )))


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
                          (k renv val) ))))))

(define (M-unary-oper op expr env k)
  (M-stat expr env 
          (lambda (e rst) (k e (op rst)) )))

(define (M-num-literal n env k)
  (k env (num n)))

(define (M-symbol sym env k)
  (cond
    [(equal? sym 'true) (k env (bool #t))]
    [(equal? sym 'false) (k env (bool #f))]
    [else (if (not (env-varDefined? env sym))
              (env-throw env (Exception+ (list "Use of undefined variable" sym)))
              (let ([var (env-getVar env sym)])
                (cond
                  [(tvoid? var) (env-throw env (Exception+ (list "Use of uninitialized value:" sym)))]
                  [else (k env var)] )))]))

(define (M-declare sym init env k)
  (if (env-varDefinedInClosure? env sym)
      (env-throw env (Exception+ (list "Redefinition of variable" sym)))
      (k (env-defineVar! env sym init) (tvoid)) ))

(define (M-declare-expr sym expr env k)
   (if (env-varDefinedInClosure? env sym)
       (env-throw env (Exception+ (list "Redefinition of variable" sym)))
       (M-stat expr env 
               (lambda (e rst) (M-declare sym rst e k) ))))


; Unlike C/Java assignment returns rvalue!
(define (M-assignment sym expr env k)
  (M-stat expr env 
          (lambda (env rst)
            (if (symbol? sym)
                (if (not (env-varDefined? env sym))
                    (env-throw env (Exception+ (list "Assigning to undefined variable" sym))) 
                    (let ([lval (env-getVar env sym)])
                      (if (value-lvalue? lval)
                          (let ([env (env-assign! env lval rst)])
                            (k env rst) )
                          (env-throw env (Exception+ (list "Cannot assign to rvalue" sym))) )))
                (M-stat sym env
                        (lambda (env lval)
                          (if (value-lvalue? lval)
                              (let ([env (env-assign! env lval rst)])
                                (k env rst) )
                              (env-throw env (Exception+ (list "Cannot assign to rvalue" sym))) ))) ))))

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

  ; Here were a very trick bug
  ; Since we changed 'env-pushLayer'
  ; from side-effect free to side-effect aware
  ; env is already changed after the execution of
  ; let statements. This is pretty dangerous indeed
  (let* ([env0 (env-cont-map env
                             (lambda (key cont)
                               (append-finalize cont)))]
         [env4 (env-pushLayer! env0)])
    (if (null? stmt)
        ((append-finalize k) env (tvoid)) 
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
(define (M-defFunction fname arg body attr env k)

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
        (if (env-varDefined? env fname)
            (env-throw env (Exception+
                            (list "Identifier for function" fname
                                  "is already used!" )))
            (let* ([closure (env-getCurrentClosure env)]
                   [Function (Function arglist body closure)]
                   [Function (if (not (null? attr)) (value-attr-add Function attr #t) Function)]
                   [n-env (env-defineConst! env fname Function)])
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

(define (M-getCallee fname env return)
    ((lambda (callee)
       (cond
         [(not (Function? callee))
          (env-throw env (Exception+ (list fname "is not a function!")))]
         [else (return callee)] ))
     (if (env-varDefined? env fname)
         (env-getVar env fname)
         (env-throw env (Exception+ (list "Undefined reference to function" fname))) )))

(define (M-call-on-value callee realArg this env k)
  (define isMethod (Function-method? callee))
  (define isStatic (Function-static? callee))
  
  (define currentClosure (env-getCurrentClosure env))

  (define k-save (env-cont-saveall env))

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
        (let ([farg (car formalArg)]
              [rarg (car realArg)])
          (if (pair? farg) ; We have a reference
              (if (value-lvalue? rarg)
                  ((lambda (newEnv)
                     (bindArguments (cdr formalArg) (cdr realArg) newEnv k))
                   (env-defineRef! env (cdr farg) rarg) )
                  (env-throw (env-replaceClosure env currentClosure)
                             (Exception "Cannot take reference of rvalues")))
              ((lambda (newEnv)
                 (bindArguments (cdr formalArg) (cdr realArg) newEnv k))
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
  
  (let ([formalArg (Function-arg callee)])
      (if (not (equal? (length formalArg) ; 2
                       (length realArg)))
          (env-throw env (Exception+ (list "Function called with wrong number of arguments\n"
                                           "Expecting" (length formalArg)
                                           "| Got" (length realArg))))
          (eval-arguments realArg env ; 3
          (lambda (evaledEnv realArgValue)
            (let* ([formalArg (if isMethod (cons 'this formalArg) formalArg )]
                   [realArgValue (if isMethod (cons this realArgValue) realArgValue)] ; "this" argument of oop
                   [calleePreClosure (env-replaceClosure evaledEnv (Function-closure callee))]
                   [calleeClosureEnv (env-pushClosure calleePreClosure)]) ; 4 5
              (bindArguments formalArg realArgValue calleeClosureEnv ; 6
               (lambda (bindedEnv v)
                 (introduceCall bindedEnv ; 7
                 (lambda (calleeEnv)
                   (exec (Function-body callee) calleeEnv
                   (lambda (terminateEnv value) ; -1, no return called internally
                     (env-return terminateEnv (tvoid)) ))))))))))))

(define (M-callFunction fname realArg env k)
  (M-getCallee fname env
               (lambda (callable)
                 (M-call-on-value realArg '() env k))))
                                                                 

(define (M-declareClass cname inheritance body env k)
  
  (define saved-closure (env-getCurrentClosure env))

  (define (M-body body class env k)
    (if (null? body)
        (k env class)
        (M-classdef (car body) class env
                    (lambda (env class)
                      (M-body (cdr body) class env k)))))

  ; Return Value:
  ; Env -> Top level of Env should be the class closure
  ; Class -> name : cname
  ;       -> itor : function evaluates to an instance closure chain
  ;       -> closure : closure chain of that class
  ;       -> base : base class name
  (define (M-inheritance inheritance env k)
    (match inheritance
      ('() (let* ([env (env-pushClosure env)]
                  [cls-closure (env-getCurrentClosure env)]
                  [cls-itor (box (closure-make '()))])
             (k env (Class cname cls-itor cls-closure '())) ))
      (('extends base)
       (if (not (env-varDefined? env cname))
           (env-throw env (Exception+ (list "Base class" cname "not defined.")))
           (let ([cls (env-getVar env cname)])
             (if (not (Class? cls))
                 (env-throw env (Exception+ (list "Base class identifier" cname
                                                  "does not refer to a class")))
                 (let* ([base-closure (Class-closure cls)]
                        [base-itor (Class-itor cls)]
                        [env (env-replaceClosur env base-closure)]
                        [env (env-pushClosure env)]
                        [cls-closure (env-getCurrentClosure env)]
                        [cls-itor (box (closure-make (base-itor)))])
                   (k env (Class cname cls-itor cls-closure base)) )))))
      (_ (error "Invalid Syntax")) ))

  (M-inheritance inheritance env 
                 (lambda (env class)
                   (M-body body class env 
                           (lambda (env class)
                            (let* ([env (env-replaceClosure env saved-closure)]
                                   [env (env-defineConst! env cname class)])
                              (k env (tvoid)) ))))))

(define (M-classdef stat class env k)
  
  (define (M-defineProperty id val class env k)
    (let* ([class-itor (Class-itor class)]
           [ub-class-itor (unbox class-itor)])
      (if (closure-varDefined? ub-class-itor id)
          (env-throw env (Exception+ (list "Multiple definition of" id
                                           "in class" (class-name class) )))
          (if (tvoid? val)
              (let* ([ub-class-itor (closure-defineVar ub-class-itor id (tvoid))])
                (begin (set-box! class-itor ub-class-itor)
                       (k env class))) 
              (M-stat val env
                      (lambda (env rst)
                        (begin (set-box! class-itor
                                         (closure-defineVar ub-class-itor id rst))
                               (k env class) )))))))

  (define (M-defineMethod fname arglist body class env k)
    (M-defFunction fname arglist body 'Method env
                   (lambda (env rst) (k env class)) ))

  (define (M-defineStaticMethod fname arglist body class env k)
    (M-defFunction fname arglist body 'Static-Method env
                   (lambda (env rst) (k env class)) ))

  (match stat
    (('var id) (M-defineProperty id (tvoid) class env k))
    (('var id val) (M-defineProperty id val class env k))
    (('function fname arglist body)
     (M-defineMethod fname arglist body class env k))
    (('static-function fname arglist body)
     (M-defineStaticMethod fname arglist body class env k))
    (_ (begin (display stat)
              (error "Invalid Class Syntax") ))))

(define (M-operatorNew cname env k)
  (if (not (env-varDefined? env cname))
      (env-throw env (Exception+ (list "Class " cname "not defined.")))
      (let ([cls (env-getVar env cname)])
        (if (not (Class? cls))
            (env-throw env (Exception+ (list "Identifier" cname
                                             "does not refer to a class")))
            (k env (Object cname (deepcopy (Class-itor cls)))) ))))

(define (M-dot-access prefix attr env k)

  (define (throw-not-found)
    (env-throw env (Exception+ (list "Object" prefix "of class"
                                     (Object-class obj)
                                     "does not have an property/method named"
                                     attr) )))
  (M-stat prefix env
          (lambda (env item)
            (cond
              [(Class? item)
               (let ([closure (unbox (Class-closure item))])
                 (if (not (closure-varDefined? closure attr))
                     (throw-not-found)
                     (k env item (closure-getVar closure attr))))]
              [(Object? item)
               (let ([obj-closure (unbox (Object-closure item))])
                 (if (closure-varDefined? obj-closure attr)
                     (k env item (closure-getVar obj-closure attr))
                     (M-stat (Object-class item) env
                             (lambda (env cls)
                               (if (Class? cls)
                                   (let ([closure (unbox (Class-closure cls))])
                                     (k env item (closure-getVar closure attr)) )
                                   (begin (displayln cls) (error "Unreachable code")) )))))]
              [env-throw (Excetption+ (list prefix "is not an object or class"))] ))))
                     
(define (M-dot-prop prefix attr env k)
  (M-dot-access prefix attr env
                (lambda (env obj field)
                      (k env field) )))

(define (M-dot-method prefix method arglist env k)
  (M-dot-access prefix method env
                (lambda (env item function)
                  (cond
                    [(Class? item)
                     (if (not (Function-static? function))
                         (env-throw env (Exception+ (list method "is not a static method")))
                         (M-call-on-value function arglist '() env k))]
                    [(Object? item)
                     (if (not (Function-method? function))
                         (env-throw env (Exception+ (list method "is not a class method")))
                         (M-call-on-value function arglist item env k))]
                    [else (error "Unreachable Code")] ))))
                

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
  ;(testall-helper 46 46))
  (testall-helper 1 87))

;(testall)