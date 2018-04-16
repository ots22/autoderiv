#lang racket
(require rackunit
         syntax/id-table
         (for-syntax
           racket
           syntax/id-table))

(provide annotation
         get-annotation
         has-annotation?)

(define binding->trace (make-parameter (make-bound-id-table)))

(define-syntax (annotation stx)
  (syntax-case stx ()
    [(_ id val) #'(bound-id-table-set! (binding->trace) #'id val)]))

(define-syntax (get-annotation stx)
  (syntax-case stx ()
    [(_ id) #'(bound-id-table-ref (binding->trace) #'id)]))

(define-syntax (has-annotation? stx)
  (syntax-case stx ()
    [(_ id) #'(let ([found #t])
                (bound-id-table-ref (binding->trace) #'id (lambda () (set! found #f)))
                found)]))
