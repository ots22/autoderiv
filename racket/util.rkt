#lang racket
(require rackunit)

(provide atom?)

(define (atom? x)
  (not (or (pair? x) (null? x))))

(module+ test
  (check-true  (atom? 'a))
  (check-true  (atom? 5))
  (check-true  (atom? #f))
  (check-true  (atom? #(1 2 3))) ; vectors are atoms by this definition
  (check-false (atom? '()))
  (check-false (atom? '(a)))
  (check-false (atom? '(a b)))
  (check-false (atom? '(a . b)))
  
  (check-exn exn:fail? (lambda () (atom?)))
  (check-exn exn:fail? (lambda () (atom? 'atom 'another))))
