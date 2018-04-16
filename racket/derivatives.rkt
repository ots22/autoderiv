#lang racket
(require rackunit)

(provide ∂
         define-primal-derivative
         grad)

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

(require "trace.rkt")

(define (proc-node? a)
  (and (node? a) (eq? (node-type a) 'proc)))

(define (value-node? a)
  (and (node? a) (eq? (node-type a) 'value)))

(define (var-node? a)
  (and (node? a) (eq? (node-type a) 'var)))

(define (0->n n)
  (stream->list (in-range 0 n)))

(define (grad t)
  (letrec ([table (make-hasheq)]
           [derivative-from-trace
            (lambda (t seed)
              (cond
                [(null? t) '()]
                [(value-node? (car t)) '()]
                [(var-node? (car t))
                 (let* ([var (node-label (car t))]
                        [curr-value (hash-ref table var 0.0)])
                   (hash-set! table var (+ seed curr-value)))]
                [(proc-node? (car t))
                 (let* ([proc (node-label (car t))]
                        [args (map (compose node-value car) (cdr t))]
                        ;; the arguments are in reverse order in the tree
                        [indices (reverse (0->n (length args)))]
                        [partials (map (lambda (n) (apply ((∂ n) proc) args)) indices)])
                   (map (lambda (partial subtree) (derivative-from-trace subtree (* seed partial)))
                        partials (cdr t)))]
                [else (map derivative-from-trace t seed)]))])
    (derivative-from-trace t 1.0)
    table))




