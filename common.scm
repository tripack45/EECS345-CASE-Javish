;(define (filter+ pred l)
;  (define (filter-cps pred l k)
;    (cond
;      ((null? l) (k '()))
;      ((not (pred (car l))) (filter-cps pred (cdr l) k))
;      (else (filter-cps pred
;                        (cdr l)
;                        (lambda (rest) (k (cons (car l) rest)))))))
;
;  (filter-cps pred l (lambda (x) x)))
;
;(define (remove-first pred l)
;  (define (rm-first-cps pred l k)
;    (cond
;      ((null? l) (k '()))
;      ((pred (car l)) (k (cdr l)))
;      (else (rm-first-cps pred
;                          (cdr l)
;                          (lambda (rest) (k (cons (car l) rest))) ))))
;
;  (rm-first-cps pred l (lambda (x) x)) )
;
;(define (first+ pred l)
;  (if (pred (car l))
;      (car l)
;      (first pred (cdr l)) ))
;
;(define (error-make reason var)
;  (lambda () (string-append reason (symbol->string var))))

(define (land p q) (and p q))
(define (lor p q) (or p q))
(define (lnot p) (not p))

(define (id x) x)

; Poor man's printf
(define (form-string l)
  (cond
    [(null? l) ""]
    [(symbol? (car l))
     (string-append " '" (symbol->string (car l)) "' "
                    (form-string (cdr l)) )]
    [(number? (car l))
     (string-append " " (number->string (car l)) " "
                    (form-string (cdr l)) )]
    [(string? (car l))
     (string-append (car l) (form-string (cdr l)))]
    [else
     (begin (print l)
            (newline)
            (error "Cannot format string") )]))

; Returns a list of 2 lists
; The second list's first element is elem
; The second list is empty if elem is not exist

(define (split+ l crit)
  (define (split+& l crit k)
    (cond
      [(null? l) (k '() '())]
      [(crit (car l)) (k '() l)]
      [else (split+& (cdr l) crit
                     (lambda (first rest)
                       (k (cons (car l) first) rest) ))]))

  (split+& l crit (lambda (f r) (list f r))) )

(define (split l elem)
  (split+ l (lambda (e) (equal? e elem))))



; Internal Exception Type

(define (iException text)
  (box (lambda () text)) )

(define (iException? e)
  (and (box? e)
       (procedure? (unbox e)) ))

(define (iException+ list)
  (iException (form-string list)) )

(define (iException-str e)
  ((unbox e)) )
      
; Dictionary Type

(define (dict-make) '())

(define (dict-make+ keylist vallist)
  (if (null? keylist) '()
      (if (null? vallist)
          (dict-add (dict-make+ (cdr keylist) vallist)
                    (car keylist) #f)
          (dict-add (dict-make+ (cdr keylist) (cdr vallist))
                    (car keylist) (car vallist)) )))

(define (dict-pair-make key val)
  (list key val) )

(define (dict-cmpkey key)
  (lambda (pair) (equal? (car pair) key)) )

(define (dict-exisist? dict key)
  (match (split+ dict (dict-cmpkey key))
    ((fore back) (not (null? back))) ))

(define (dict-get dict key)
  (match (split+ dict (dict-cmpkey key))
    ((fore back)
     (if (null? back)
         (iException+ (list "Dictionary: retrieved key" key "does not exists."))
         (cadr (car back)) ))))

(define (dict-add dict key value)
  (if (dict-exisist? dict key)
      (iException+ (list "Dictionary: key" key "already exists."))
      (cons (dict-pair-make key value) dict) ))

; Performs deep copy of given dict
(define (dict-clone dict)
  (if (null? dict) '()
      (let ([first (car dict)]
            [rest (cdr dict)])
        (cons (list (car first) (box (unbox (cadr first))))
              (dict-clone (cdr dict)) ))))
  
; Update without side effect
(define (dict-update dict key newValue)
  (match (split+ dict (dict-cmpkey key))
    ((fore back)
     (if (null? back)
         (iException+ (list "Dictionary: updated key" key "does not exists."))
         (let ([pair (car back)])
           (append fore
                   (cons (dict-pair-make key newValue)
                         (cdr back) )))))))

(define (dict-update+ dict key f)
  (match (split+ dict (dict-cmpkey key))
    ((fore back)
     (if (null? back)
         (iException+ (list "Dictionary: updated key" key "does not exists."))
         (let ([pair (car back)])
           (append fore
                   (cons (dict-pair-make key (f (cadr pair)))
                         (cdr back) )))))))
; F expects to take in 2 values
; an key and a value
(define (dict-map dict f)
  (letrec ([fp (lambda (pair) (list (car pair)
                                    (apply f pair) ))])
    (map fp dict) ))

; Removes the item given by key
; if item does not exists, does nothing
(define (dict-remove dict key)
  (match (split+ dict (dict-cmpkey key))
    ((fore back)
     (if (null? back) dict
         (append fore (cdr back)) ))))


; Set Type

(define (set-make) '())

(define (set-make+ list)
  (define (set-make+-acc acc list)
    (cond
      [(null? list) acc]
      [(set-memberof? acc (car list))
       (set-make+-acc acc (cdr list))]
      [else (set-make+-acc (set-add acc (car list))
                           (cdr list) )]))

  (set-make+-acc (set-make) list))

(define (set-memberof? set elem)
  (cond
    [(null? set) #f]
    [(equal? (car set) elem) #t]
    [else (set-memberof? (cdr set) elem)] ))
   
(define (set-add set elem)
  (if (set-memberof? set elem)
      (iException+ (list "Set: element" elem "already exist!"))
      (cons elem set) ))

(define (set-remove set elem)
  (match (split set elem)
    ((fore back)
     (if (null? back) fore
         (append fore (cdr back)) ))))

; Utils

(define (memberOf? e list)
  (cond
    [(null? list) #f]
    [(equal? e (car list)) #t]
    [else (memberOf? e (cdr list))] ))

(define (unique? list)
  (cond
    [(null? list) #t]
    [(memberOf? (car list) (cdr list)) #f]
    [else (unique? (cdr list))] ))
    

