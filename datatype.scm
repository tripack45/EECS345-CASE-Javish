(load "common.scm")
(load "env.scm")


; Datatypes:
; We have 2 internal datatypes currently
; 'number' type, given by "number?" procedure
; 'boolean' type, given by "boolean?" procedure
; 'Exception' type, for throwing exceptions
; 'tvoid' type, return type of no return value cases
;              also used to mark uninitialize variable

; number type
(define (num x)
  (if (number? x)
      (value-make x 'number)
      (iException+ (list "Cannot form object type 'number' from" x)) ))

(define (num? value)
  (and (equal? (value-type value) 'number)
       (number? (value-v value)) ))

; boolean type
(define (bool x)
  (if (boolean? x)
      (value-make x 'boolean)
      (iException+ (list "Cannot form object type 'boolean' from" x)) ))

(define (bool? value)
  (and (equal? (value-type value) 'boolean)
       (boolean? (value-v value)) ))

(define (bool-true? x)
  (if (bool? x)
      (value-v x)
      (iException+ (list "Boolean value expected, got" (value-type x)) )))

; Exception type
(define (Exception msg)
  (if (string? msg)
      (value-make msg 'Exception)
      (iException+ (list "Cannot form object type 'Exception' from" msg)) ))

(define (Exception+ list)
  (let ([msg (form-string list)])
    (if (string? msg)
        (value-make msg 'Exception)
        (iException+ (list "Cannot form object type 'Exception' from list")) )))

(define (Exception? value)
  (and (equal? (value-type value) 'Exception)
       (string? (value-v value)) ))

; Function type
; Note function name is NOT a property of a function
; Mathematically only the MAPPING matters
; Thus argument, body, closure and return type
; But we don't have a return type :)
(define (Function arglist body closure)
  (value-make (dict-make+ (list 'arg 'body 'closure)
                          (list arglist body closure) )
               'Function) )

(define (Function? value)
  (equal? (value-type value) 'Function))

(define (Function-arg fun) (dict-get (value-v fun) 'arg))
(define (Function-body fun) (dict-get (value-v fun) 'body))
(define (Function-closure fun) (dict-get (value-v fun) 'closure))

; Data-type to string

(define (tostring value)
  (cond
    [(tvoid? value) "No value"]
    [(num? value) (number->string (value-v value))]
    [(bool? value) (if (value-v value)
                          "true"
                          "false")]
    [(Exception? value) (string-append "Exception: " (value-v value))]
    [else "Unknown datatype."] ))


; tvoid type, just one single value
(define (tvoid)
  (value-make '() 'tvoid))

(define (tvoid? v)
  (and (pair? v)
       (equal? (value-type v) 'tvoid)
       (null? (value-v v)) ))

; Overloading operators

(define (homogenous-op op input-type? ret-type)
  (lambda (v1 v2)
    (cond
      [(or (not (input-type? v1))
           (not (input-type? v2)) )
       (iException+ (list "No overload for" op "over type"
                          (value-type v1) "and" (value-type v2)))]
      [else (ret-type (op (value-v v1)
                          (value-v v2)))] )))

(define (unary-op op input-type? ret-type)
  (lambda (v)
    (cond
      [(not (input-type? v))
       (iException+ (list "No overload for unary" op "over type" (value-type v)))]
      [else (ret-type (op (value-v v)))] )))


(define (general-equiv v1 v2)
  (equal? v1 v2) )

(define (homogenous-equiv v1 v2)
  (cond
    [(or (tvoid? v1) (tvoid? v2))
     (iException "Cannot compare against non-exisistent values.")]
    [(not (equal? (value-type v1) (value-type v2)))
     (iException+ (list "Cannot compare" (value-type v1)
                        "against" (value-type v2) ))]
    [else (bool (equal? (value-v v1)
                        (value-v v2) ))]))

(define (homogenous-inequiv v1 v2)
  (let ([ret (homogenous-equiv v1 v2)])
    (if (iException? ret) ret
        (t/! ret) )))
            

; Short hand for "typed/+"
(define t/+ (homogenous-op + num? num))
(define t/- (homogenous-op - num? num)) 
(define t/* (homogenous-op * num? num))
(define t// (homogenous-op quotient num? num))
(define t/% (homogenous-op modulo num? num))

(define t/> (homogenous-op > num? bool))
(define t/< (homogenous-op < num? bool))
(define t/>= (homogenous-op >= num? bool))
(define t/<= (homogenous-op <= num? bool))

(define t/&& (homogenous-op land bool? bool))
(define t/|| (homogenous-op lor bool? bool))

(define t/== homogenous-equiv)
(define t/!= homogenous-inequiv)

(define t/neg (unary-op - num? num))
(define t/! (unary-op lnot bool? bool))

; Tests
;(define e1 (env-make+ '(a b c d)
;                      (list
;                       (num 1)
;                       (num 2)
;                       (num 3.0)
;                       (bool #t))))
;(newline)
;e1
;(newline)
;
;(value-torvalue (env-getVar e1 'a))
;
;(equal? (value-torvalue (env-getVar e1 'a))
;        (num 1))
;
;(define e2 (env-assign! e1
;                        (env-getVar e1 'b)
;                        (num 10)))
;
;(newline)
;
;e2
;
;(newline)
;
;(define e3 (env-defineVar! e2 'k (num 123)))
;
;
;(display "e3")
;(newline)
;e3
;(newline)
;
;(equal? (value-torvalue (env-getVar e2 'b)) (num 10))
;(equal? (value-torvalue (env-getVar e1 'b)) (num 10))
;(equal? (value-torvalue (env-getVar e2 'd)) (bool #t))
;
;(display "pop layer: \n")
;(env-popLayer! e3)
;(display "push layer: \n")
;(env-pushLayer! e3)
;(display "push closure \n")
;(env-pushClosure e3)
;(env-pushClosure e3)
;(env-pushClosure e3)
;(env-pushClosure e3)
;(env-pushClosure e3)
;(env-pushClosure e3)
;(env-pushClosure e3)