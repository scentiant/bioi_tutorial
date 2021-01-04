;; Michael E Sparks (10-16-20)
;; Scheme code for efficiently finding primes

#|
Sketch out in BASIC what we're after:

$ cat > prime.bas << EOF
> 10 REM Brute-force prime number finder
> 20 FOR N = 50 TO 2 STEP -1
> 30 P = 1
> 40 FOR M = 2 TO N - 1
> 50 Z = N MOD M
> 60 IF Z = 0 THEN
> 70 P = 0
> 80 END IF
> 90 NEXT M
> 100 IF P = 1 THEN
> 110 PRINT N
> 120 END IF
> 130 NEXT N
> 140 PRINT 2
> EOF
bwbasic
Bywater BASIC Interpreter/Shell, version 2.20 patch level 2
Copyright (c) 1993, Ted A. Campbell
Copyright (c) 1995-1997, Jon B. Volkoff

bwBASIC: load "prime.bas"
bwBASIC: run
47
43
41
37
31
29
23
19
17
13
11
7
5
3
2
bwBASIC: quit
|#

;; Always seed a random number generator!
(set! *random-state* (random-state-from-platform))

;; Scheme has a built-in for this (expt base exponent),
;; but we'll roll our own for illustrative purposes.
(define (power b p)
  (if (= p 0)
      1
      (* b (power b (- p 1)))))

(define (fermat-test n)
  (define (exp-then-mod base exp mod-rhs)
    (remainder (power base exp) mod-rhs))
  (let ((a (+ 1 (random (- n 1)))))
    (= (exp-then-mod a 1 n)
       (exp-then-mod a n n))))

(define (seems-prime? cand times-to-test)
  (cond ((= times-to-test 0) #t)
	((fermat-test cand)
	 (seems-prime? cand (- times-to-test 1)))
	(#t #f)))

(define (is-prime? cand)
  (define (test-it cand div)
    (cond ((or (= cand 2) (= div 1)) #t)
	  ((= (remainder cand div) 0) #f)
	  (else (test-it cand (- div 1)))))
  ;; Suppose cand % div = 0. Then, so too
  ;; does cand % (cand / div) = 0. However,
  ;; at least one of these <= sqrt(cand).
  ;; Thus, run-time of Omega(sqrt(cand))
  ;; rather than Omega(cand).
  (test-it cand (ceiling (sqrt cand))))

;; I would generally prefer to do the following recursively,
;; but am demonstrating here that Lisp can also accommodate an
;; iterative/ imperative approach (in contrast to pure functional
;; languages such as Haskell).
(define (list-primes)
  (display "Ceiling on primes? ")
  (let ((ceil (read)))
    (do ((p 2 (+ p 1)))
	((> p ceil))
      (if (and (seems-prime? p 5) (is-prime? p))
	  (format #t "~s\n" p)))))
