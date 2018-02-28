#lang racket
(require "binding-trace.rkt"
         (for-syntax racket/syntax
                     syntax/kerncase))

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
  (kernel-syntax-case stx #f
    [(letrec-values (((var) val)) body ...)
     (with-syntax* ([(body* ...) (map nested-helper (syntax->list #'(body ...)))])
       (displayln "letrec")
       (syntax/loc stx (letrec-values (((var) (begin (annotation var 'a) val)))
                         (displayln (get-annotation var)) body* ...)))]

    [(let-values (((var) val)) body ...)
     (with-syntax* ([(body* ...) (map nested-helper (syntax->list #'(body ...)))])
       (displayln "let")
       (syntax/loc stx (let-values (((var) (begin (annotation var 'a) val)))
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

(expand-nested (let-values (((x) 2))
                 (let-values (((x) 3)) x)))

(expand-nested (letrec-values (((x) 2))
                 (letrec-values (((x) 3)) x)))

