#lang racket
(require "derivatives.rkt"
         "instrument.rkt"
         "trace.rkt"
         pict/tree-layout
         "render-trace.rkt")
;(((∂ 0) sin) 1.0)
;
;(let ((t `((3 proc ,+) ((2 var y)) ((1 var x)))))
;  (grad t))
;
;;(current-trace)
;
;
;(let ((t `((7 proc ,+) ((6 proc ,*) ((3 var z)) ((2 var y))) ((1 var x)))))
;  (grad t))


;
;(parameterize ((current-trace '()))
;  (let ((x 1)
;        (y 2)
;        (z 3))
;    (instrument
;     (+ x (* y z)))
;    (grad (car (current-trace)))))
;
;(parameterize ((current-trace '()))
;  (let ((x 1)
;        (y 2))
;    (instrument
;     (+ (* x y) (sin x)))
;    (grad (car (current-trace)))))
;
;(current-trace '())
;(let ((x 1))
;  (instrument
;   (letrec ((g (lambda (x n) (if (= n 0) x (g (+ x x) (- n 1))))))
;     (let ((res (g x 2)))
;       res))))
;(car (current-trace))


(current-trace '())
(let ((x 1))
  (instrument
   (let* ((y 5)
         (f (lambda (x) (sin x)))
         (z (f y)))
     (+ x z))))
(grad (car (current-trace)))
        