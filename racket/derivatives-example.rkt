#lang racket
(require "derivatives.rkt")
(((∂ 0) sin) 1.0)

(let ((t `((3 proc ,+) ((2 var y)) ((1 var x)))))
  (grad t))

;(current-trace)


(let ((t `((7 proc ,+) ((6 proc ,*) ((3 var z)) ((2 var y))) ((1 var x)))))
  (grad t))