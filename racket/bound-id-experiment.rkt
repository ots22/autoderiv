#lang racket
(require "trace.rkt"
         "binding-tree.rkt")

(let-values ([(x)
              (parameterize
                  ([current-trace '()])
                (begin0
                  (trace + (+ (trace 'value (quote 1)) (trace 'value (quote 3))))
                  (annotation x (current-trace))))])
  (let-values ([(x)
                (parameterize
                    ([current-trace '()])
                  (begin0 (trace 'value (quote 5)) (annotation x (current-trace))))])
    (parameterize
        ([current-trace '()] [parent-params (current-parameterization)])
      (let ([result
             (trace-append/label
              +
              (+
               (begin (current-trace (cons (get-annotation x) (current-trace))) x)
               (begin (current-trace (cons (get-annotation x) (current-trace))) x)))])
        (let ([subexpr-trace (current-trace)])
          (call-with-parameterization (parent-params) (lambda () (trace-append subexpr-trace))))
        result))))