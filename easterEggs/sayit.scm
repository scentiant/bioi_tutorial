;; Michael E Sparks, 11-17-16 (updated 10-16-20)

;; sayit.scm - Implementation of Conway's "Say It" sequence

;; Build up a list of dotted pairs--the car of each
;; such pair gives the length of the run, and the
;; cdr denotes the value of the run. The say-it sequence
;; is based on integer-valued symbols, though extension to
;; other classes could be accommodated by modifying the
;; "equals" predicate to suit. That's left as an exercise.
(define (markuprun type runlen rest)
  (cond ((null? rest) (cons (cons runlen type) '()))
        ((= type (car rest)) (markuprun type (1+ runlen) (cdr rest)))
        (#t (cons (cons runlen type)
                  (markuprun (car rest) 1 (cdr rest))))))

;; Converts a list of dotted pairs to a list of atomic elements.
(define (flatten src)
  (if (null? src)
      '()
      (append (list (caar src) (cdar src))
              (flatten (cdr src)))))

;; In Conway's say-it sequence, elements 2, 3, ..., N
;; depend only on the immediately preceding element.
(define (nextelt prevelt)
  (flatten (markuprun (car prevelt) 1 (cdr prevelt))))

;; Returns list of lists, each of which
;; is an element of the say-it sequence.
(define (sayitbuilder max)
  (let ((base (list 1)))
    (define (sayitaux prevelt currcnt)
      (if (>= currcnt max)
          '()
          (cons (nextelt prevelt)
                (sayitaux (nextelt prevelt) (1+ currcnt)))))
    (append (list base) (sayitaux base 1))))

;; Report elements of sequence in a print-friendly manner.
;; There's some subtle "for-each" vs "map" business going on
;; here, in particular w/r/t return values vs side effects;
;; grokking it's left as an exercise.
(define (sayitwriter seq)
  (let* ((dispn (lambda (n) (display n)))
         (procl (lambda (l) (map dispn l) (newline))))
    (for-each procl seq)))

;; Use the following expression as the principle "hook" for
;; calling code written in C.
(define sayit (lambda (x) (sayitwriter (sayitbuilder x))))

;; Shuttle launch sequence -> alternate behavior
(define (countdown x)
  (if (> x 0)
      (begin (display x) (newline) (sleep 1) (countdown (1- x)))
      (begin (display "Blastoff!") (newline))))
