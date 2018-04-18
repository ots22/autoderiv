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
      x
      (trace-call sin-iter (trace-call sin x) (- n 1))))

(trace-value (sin-iter (trace-value 1.0) 3))
(naive-layered (car (render-trace (current-trace))))

(trace-call sin (trace-call sin (trace-call sin 1.0)))
(naive-layered (car (render-trace (current-trace))))