#lang racket
(require "trace.rkt")
(require "render-trace.rkt")
(require pict pict/tree-layout pict/code)

(define (fact n)
  (if (= n 0)
      (trace-value 1)
      (trace-call * (trace-value n)
                    (trace-call fact (trace-call - (- n 1))))))

(define (sin-iter x n)
  (if (= n 0)
      (trace-value x)
      (trace-call sin-iter (trace-call sin x) (trace-call - n 1))))

(trace-call sin-iter (trace-value 1.0) (trace-value 3))
(naive-layered (car (render-trace (current-trace))))
