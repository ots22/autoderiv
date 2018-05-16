#lang racket
(require rackunit
         "util.rkt")

(provide trace-value
         trace-call
         trace-var
         current-trace
         parent-params
         node?
         node-label
         node-type
         node-value
         trace-append
         trace-append/label
         check-result-and-trace)

(define current-trace (make-parameter '()))
(define parent-params (make-parameter '()))

(define (node? x)
  (and (list? x)
       (>= (length x) 2)
       (or (eq? (cadr x) 'value)
           (eq? (cadr x) 'proc)
           (eq? (cadr x) 'var))))

(define (make-node value type . label)
  (append (list value type) label))

(define (node-value node) (first node))
(define (node-type node) (second node))
(define (node-label node) (third node))

(define (trace-append value)
  (current-trace (cons value (current-trace)))
  value)

(define (trace-append/label value . rest)
  (current-trace (cons (apply make-node value rest)
                       (current-trace)))
  value)

(define-syntax (trace-value stx)
  (syntax-case stx ()
    [(_ expr)
     #'(parameterize
           ([current-trace '()]
            [parent-params (current-parameterization)])
         (let ([result (trace-append/label expr 'value)])
           (let ([subexpr-trace (current-trace)])
             (call-with-parameterization (parent-params)
              (lambda () (trace-append subexpr-trace))))
           result))]))

(define-syntax (trace-call stx)
  (syntax-case stx ()
    [(_ fn args ...)
     #'(parameterize
           ([current-trace '()]
            [parent-params (current-parameterization)])
         (let* ([fn-once fn] ;; since fn might be an expression that evaluates to a function
                [result (trace-append/label (fn-once args ...) 'proc fn-once)])
           (let ([subexpr-trace (current-trace)])
             (call-with-parameterization (parent-params)
              (lambda () (trace-append subexpr-trace))))
           result))]))

(define-syntax (trace-var stx)
  (syntax-case stx ()
    [(_ expr)
     #'(parameterize
           ([current-trace '()]
            [parent-params (current-parameterization)])
         (let ([result (trace-append/label expr 'var 'expr)])
           (let ([subexpr-trace (current-trace)])
             (call-with-parameterization (parent-params)
              (lambda () (trace-append subexpr-trace))))
           result))]))

;; call function fn and check the return value and the current-trace against
;; the expected results passed as arguments
(define-syntax (check-result-and-trace stx)
  (syntax-case stx ()
    [(_ expected-value expected-trace expr)
     #`(parameterize ([current-trace '()])
         #,(syntax/loc stx (check-equal? expr expected-value))
         #,(syntax/loc stx (check-equal? (current-trace) expected-trace)))]))

(module+ test
  (check-result-and-trace 8
                          `(((8 proc ,+) ((5 value)) ((3 value))))
                          (trace-call + (trace-value 3) (trace-value 5)))

  (check-result-and-trace 7
                          `(((7 proc ,+) ((1 value)) ((6 proc ,*) ((2 value)) ((3 value)))))
                          (trace-call + (trace-call * (trace-value 3) (trace-value 2)) (trace-value 1))))
