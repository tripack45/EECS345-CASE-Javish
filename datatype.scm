(load "common.scm")

(load "env.scm")

; Datatypes:
; We have 2 internal datatypes currently
; 'number' type, given by "number?" procedure
; 'boolean' type, given by "boolean?" procedure

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

; Exception type
(define (Exception msg)
  (if (string? msg)
      (value-make msg 'Exception)
      (iException+ (list "Cannot form object type 'Exception' from" msg)) ))

(define (Exception? value)
  (and (equal? (value-type value) 'Exception)
       (string? (value-v value)) ))


; Tests
;(define e1 (env-make+ '(a b c d)
;                      (list
;                       (num 1)
;                       (num 2)
;                       (num 3.0)
;                       (bool #t))))
;
;(equal? (env-getVar e1 'a) (num 1))
;
;(define e2 (env-assignToRef e1
;                            (env-getRef e1 'b)
;                            (num 10)))
;
;(equal? (env-getVar e2 'b) (num 10))
;(equal? (env-getVar e1 'b) (num 10))
;(equal? (env-getVar e2 'd) (bool #t))
;
;(define e3 (env-undefVar e2 'd))
;
;(env-varDefined? e2 'd)
;(not (env-varDefined? e3 'd))
  