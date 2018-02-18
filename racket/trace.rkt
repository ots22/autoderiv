#lang racket
(define current-trace (make-parameter '()))
(define parent-params (make-parameter '()))
(define (trace-append val)
  (current-trace (cons val (current-trace)))
  val)

(define (trace-append/label label val)
  (current-trace (cons (cons label val) (current-trace)))
  val)

(define-syntax (trace stx)
  (syntax-case stx ()
    [(_ fn expr)
     #'(parameterize ([current-trace '()]
                      [parent-params (current-parameterization)])
         (let ([result (trace-append/label fn expr)])
           (let ([subexpr-trace (current-trace)])
             (call-with-parameterization (parent-params)
                                         (lambda () (trace-append subexpr-trace))))
           result))]))

(provide trace current-trace)
