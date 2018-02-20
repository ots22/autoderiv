#lang racket
(require rackunit)

(provide ∂
         define-primal-derivative)

(define *derivatives* (make-hash))

(define ((∂ n) f) ((hash-ref *derivatives* f) n))

(define-syntax-rule (define-primal-derivative n f Df)
  (hash-set! *derivatives* f
             (lambda (n) Df)))

(define-primal-derivative n + (const 1))

(define-primal-derivative n *
  (lambda xs
    (let ([xs* (list-set xs n 1)])
      (apply * xs*))))

(define-primal-derivative n sin cos)

(module+ test
  (check-= (((∂ 2) +) 1 2 3)
           1 0.0)
  (check-= (((∂ 0) *) 0 1 2 3)
           6 0.0)
  (check-= (((∂ 0) sin) 0.0)
           1.0 0.0)
  (check-= (((∂ 0) sin) 1.0)
           (cos 1.0) 0.0))


