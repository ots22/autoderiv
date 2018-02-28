#lang racket
(require "binding-trace.rkt"
         (for-syntax racket/syntax))

;(let ((x (begin (annotation x 'b) 1)))
;  (let ((x (begin (annotation x 'a) 2)))
;    (get-annotation x)))

;; Macro generating:
;(let ((x 1))
;  (let ((x 2))
;    x))

(define myparameter (make-parameter '()))

(define-for-syntax (nested-helper stx)
  (displayln stx)
  (syntax-case stx ()
    [(my-let-values-form (((var) val)) body ...)
     (with-syntax* ([(body* ...) (map nested-helper (syntax->list #'(body ...)))])
                   (displayln "In let-values!")
                   (syntax/loc stx (letrec-values (((var) (begin (annotation var 'a) val)))
                                     (displayln (get-annotation var)) body* ...)))]
    [else #'else]))

(define-syntax (nested stx)
  (syntax-case stx ()
    [(_ stuff)  (nested-helper #'stuff)]))

(define-syntax (expand-nested stx)
  (syntax-case stx ()
    [(_ a) (let ([expanded (local-expand #'a 'expression '())])
             (nested-helper expanded))]))

(define-syntax (expand-nested2 stx)
  (syntax-case stx ()
    [(_ a) (with-syntax ([expanded (local-expand #'a 'expression '())])
             (nested-helper #'expanded))]))

(define-syntax (expand-nested3 stx)
  (syntax-case stx ()
    [(_ a) (with-syntax ([expanded (expand #'a)])
             (displayln #'expanded)
             (nested-helper #'expanded))]))

(expand-nested (letrec-values (((x) 2)) (letrec-values (((x) 3)) x)))



