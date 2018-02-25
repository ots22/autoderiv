#lang racket
(require rackunit
         "util.rkt")

(provide trace
         current-trace
         parent-params
         node?
         node-label
         node-value
         trace-append
         trace-append/label)

(define current-trace (make-parameter '()))
(define parent-params (make-parameter '()))

(define (node? x)
  (and (pair? x)
       (atom? (car x))
       (atom? (cdr x))))

(define/contract (make-node label value)
  (atom? atom? . -> . node?)
  (cons label value))

(define/contract (node-label node)
  (node? . -> . atom?)
  (car node))

(define/contract (node-value node)
  (node? . -> . atom?)
  (cdr node))

(define (trace-append val)
  (current-trace (cons val (current-trace)))
  val)

(define (trace-append/label label val)
  (current-trace (cons (make-node label val)
                       (current-trace)))
  val)

(define-syntax (trace stx)
  (syntax-case stx ()
    [(_ fn expr)
     #'(parameterize
           ([current-trace '()]
            [parent-params (current-parameterization)])
         (let ([result (trace-append/label fn expr)])
           (let ([subexpr-trace (current-trace)])
             (call-with-parameterization (parent-params)
              (lambda () (trace-append subexpr-trace))))
           result))]))

(module+ test
  (test-begin
   (parameterize ([current-trace '()])
     (let ([actual-value (trace + (+ (trace 'value 3)
                                     (trace 'value 5)))]
           [expected-value 8]
           [expected-trace `(((,+ . 8) ((value . 5))
                                       ((value . 3))))])
       (check-equal? actual-value expected-value)
       (check-equal? (current-trace) expected-trace))))

  (test-begin
   (parameterize ([current-trace '()])
     (let ([actual-value (trace + (+ (trace * (* (trace 'value 3)
                                                 (trace 'value 2)))
                                     (trace 'value 1)))]
           [expected-value 7]
           [expected-trace `(((,+ . 7) ((value . 1))
                                       ((,* . 6) ((value . 2))
                                                 ((value . 3)))))])
       (check-equal? actual-value expected-value)
       (check-equal? (current-trace) expected-trace)))))
