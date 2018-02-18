#lang racket
(require "trace.rkt")
(require "render-trace.rkt")
(require pict pict/tree-layout pict/code)

(define (fact n)
  (if (= n 0)
      (trace 'value 1)
      (trace * (* (trace 'value n)
                  (trace fact (fact (trace - (- n 1))))))))

(define (sin-iter x n)
  (if (= n 0)
      (trace 'value x)
      (trace sin-iter (sin-iter (trace sin (sin x)) (trace - (- n 1))))))

(trace sin-iter (sin-iter (trace 'value 1.0) (trace 'value 3)))
(naive-layered (car (draw-graph (current-trace))))
