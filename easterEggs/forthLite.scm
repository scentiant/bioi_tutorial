#!/usr/bin/guile -s
!#

;; Michael Sparks, 24 Dec 2020

;; Implement a subset of Forth/ HP-12C functionality in
;; Scheme, handling only the words +, -, * and /.

;; Error checking is not performed, and it is assumed
;; only well-formed Forth code is passed to the interpreter.
;; The stack is returned to the shell as a list of strings.

(define-macro (apply-word op stack)
  `(number->string
    (let ((opc (car (string->list ,op)))
	  (arg1 (string->number (cadr ,stack)))
  	  (arg2 (string->number (car ,stack))))
      (cond ((eqv? opc #\+) (+ arg1 arg2))
	    ((eqv? opc #\-) (- arg1 arg2))
	    ((eqv? opc #\*) (* arg1 arg2))
	    ((eqv? opc #\/) (/ arg1 arg2))
	    (else #f)))))

(define (parse-forth stack code)
  (cond ((null? code) (begin (write (reverse stack))
			     (newline)))
	((let ((op (car code)))
	   (or (string=? "+" op) (string=? "-" op)
	       (string=? "*" op) (string=? "/" op)))
	 (parse-forth
	  (cons (apply-word (car code) stack)
		(cddr stack))
	  (cdr code)))
	(else (parse-forth (cons (car code) stack)
			   (cdr code)))))

(parse-forth '() (cdr (command-line)))
