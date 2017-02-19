(define (filter+ pred l)
  (define (filter-cps pred l k)
    (cond
      ((null? l) (k '()))
      ((not (pred (car l))) (filter-cps pred (cdr l) k))
      (else (filter-cps pred
                        (cdr l)
                        (lambda (rest) (k (cons (car l) rest)))))))

  (filter-cps pred l (lambda (x) x)))

(define (remove-first pred l)
  (define (rm-first-cps pred l k)
    (cond
      ((null? l) (k '()))
      ((pred (car l)) (k (cdr l)))
      (else (rm-first-cps pred
                          (cdr l)
                          (lambda (rest) (k (cons (car l) rest))) ))))

  (rm-first-cps pred l (lambda (x) x)) )

(define (first+ pred l)
  (if (pred (car l))
      (car l)
      (first pred (cdr l)) ))

(define (error-make reason var)
  (lambda () (string-append reason (symbol->string var))))

(define (!= lhs rhs) (not (= lhs rhs)))
(define (land p q) (and p q))
(define (lor p q) (or p q))

(define (id x) x)

