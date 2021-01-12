;; A few symbolic expressions to help explore
;; a system's precision capabilities.

;; Michael E. Sparks, 10-19-16

(define (approximate-system-epsilon)
  (define (approach-epsilon guess)
    (let ((baseline 1.0) (divisor 1.001))
      (cond ((= baseline (+ baseline (/ guess divisor)))
	     guess)
	    (else (approach-epsilon (/ guess divisor))))))
  (approach-epsilon 1.0))

(define within-tolerance?
  (lambda (testval reference tolerance)
    (not (> (abs (- testval reference)) tolerance))))

(define is-it-zero?
  (lambda (arg tol)
    (if (within-tolerance? arg 0.0 tol)
	"Yes, it is."
	"Nope, it ain't.")))

(define (ourtest x)
  (= (log x)
     (log (+ x (approximate-system-epsilon)))))
