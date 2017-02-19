;;===============================================================
;; The Lexical Analyzer

;=======================================
; get and unget the next symbol from the lexical analyzer
; A 1 symbol buffer is used so the last read symbol can be pushed
; back to the analyzer

(define last-symbol-saved #f)   ; is there a symbol buffered?
(define saved-symbol #f)        ; the symbol buffer

; gets the next symbol to be processed
;
(define get-next-symbol
  (lambda ()
    (if last-symbol-saved
       (begin
          (set! last-symbol-saved #f)
          saved-symbol)
       (begin
          (set! saved-symbol (lex))
          (set! last-symbol-saved #f)
          saved-symbol))))

; mark the last symbol sent as unread so that it can be read again
;
(define unget-next-symbol
  (lambda ()
    (begin
      (set! last-symbol-saved #t))))

;; A 1 character buffer is used so the last read character can be
;; pushed back

(define saved-last-char #f)    ; is the last character buffered?
(define last-read-char #f)     ; the character buffer

; read the next character from the file

(define readchar 
  (lambda (port)
    (if saved-last-char
        (begin
          (set! saved-last-char #f)
          last-read-char)
        (read-char port))))

; unread the last character from the file so it can be read again

(define unreadchar
  (lambda (lastchar port)
    (begin (set! last-read-char lastchar)
           (set! saved-last-char #t))))

; save the port to the input file

(define file-port '())

; open the input file

(define start-lex
  (lambda (filename)
     (set! file-port (open-input-file filename))))

; close the input file

(define end-lex
  (lambda ()
    (close-input-port file-port)))

; the current list of reserved words and operator characters

(define reserved-word-list '(if else return while break continue class extends new throw catch finally try static var true false function))
(define reserved-operator-list '(#\= #\< #\> #\! #\+ #\* #\/ #\- #\% #\& #\| #\!))

; return a lexeme with the next read symbol

(define return-id-lex 
  (lambda (id)
    (if (memq id reserved-word-list)
        (if (or (eq? id 'false) (eq? id 'true))
            (cons 'BOOLEAN id)
            (cons id '()))
        (cons 'ID id))))

(define return-num-lex
  (lambda (value)
    (cons 'NUMBER value)))

(define return-symbol-lex
  (lambda (symbol)
    (cons 'BINARY-OP symbol)))

;(define return-add-lex
;  (lambda (symbol)
;    (cons 'MATHOP symbol)))

(define return-left-paren-lex
   (lambda ()
     (cons 'LEFTPAREN '())))

(define return-right-paren-lex
  (lambda ()
    (cons 'RIGHTPAREN '())))

(define return-assign-lex
  (lambda (symbol)
     (cons 'ASSIGN symbol)))

(define return-null-lex
  (lambda (symbol)
    (cons 'UNKNOWN symbol)))

(define return-semicolon-lex
  (lambda ()
    (cons 'SEMICOLON '())))

(define return-leftbrace-lex
  (lambda ()
    (cons 'LEFTBRACE '())))

(define return-rightbrace-lex
  (lambda ()
    (cons 'RIGHTBRACE '())))

(define return-comma-lex
  (lambda ()
    (cons 'COMMA '())))

(define return-eof-lex
  (lambda ()
    (cons 'EOF '())))

(define return-period-lex
  (lambda ()
    (cons 'BINARY-OP 'dot)))

; The lexical analyer.  Keep reading characters until the next symbol is found.
; then return that symbol

(define lex
  (lambda ()
    (let ((nextchar (readchar file-port)))
      (cond ((eof-object? nextchar) (return-eof-lex))
            ((char-whitespace? nextchar) (lex))
            ((char-alphabetic? nextchar) (return-id-lex (string->symbol (id-lex file-port (make-string 1 nextchar)))))
            ((char-numeric? nextchar) (return-num-lex (num-lex file-port (addtointeger 0 nextchar))))
            ((memq nextchar reserved-operator-list) (return-symbol-lex (string->symbol (symbol-lex file-port (make-string 1 nextchar)))))
            ((char=? #\( nextchar) (return-left-paren-lex))
            ((char=? #\) nextchar) (return-right-paren-lex))
            ((char=? #\; nextchar) (return-semicolon-lex))
            ((char=? #\{ nextchar) (return-leftbrace-lex))
            ((char=? #\} nextchar) (return-rightbrace-lex))
            ((char=? #\, nextchar) (return-comma-lex))
            ((char=? #\. nextchar) (return-period-lex))
            (else (return-null-lex nextchar))))))


(define id-lex
   (lambda (fport idstring)
      (let ((nextchar (readchar fport)))
        (if (or (char-alphabetic? nextchar) (char-numeric? nextchar) (char=? #\_ nextchar))
            (id-lex fport (string-append idstring (make-string 1 nextchar)))
            (begin (unreadchar nextchar fport)
                   idstring)))))

(define addtointeger
  (lambda (val nextdigit)
     (+ (* val 10) (- (char->integer nextdigit) (char->integer #\0)))))

(define num-lex
  (lambda (fport value)
    (let ((nextchar (readchar fport)))
      (if (char-numeric? nextchar)
         (num-lex fport (addtointeger value nextchar))
         (begin (unreadchar nextchar fport)
                value)))))

(define symbol-lex
  (lambda (fport idstring)
    (let ((nextchar (readchar fport)))
      (if (memq nextchar reserved-operator-list)
         (symbol-lex fport (string-append idstring (make-string 1 nextchar)))
         (begin (unreadchar nextchar fport)
                idstring)))))
