#lang racket
(require rackunit
         "trace.rkt"
         "binding-trace.rkt"
         (for-syntax
           racket
           racket/syntax
           syntax/kerncase))

(provide instrument)

(define-for-syntax (instrument-expanded stx)
  (kernel-syntax-case stx #f
    [(#%plain-lambda formals body ... bodyn)
     (with-syntax ([bodyn* (instrument-expanded #'bodyn)])
       (syntax/loc stx
         (#%plain-lambda formals body ... bodyn*)))]

    [(case-lambda [formals body ... bodyn] ...)
     (with-syntax ([(bodyn* ...)
                    (map instrument-expanded (syntax->list #'(bodyn ...)))])
       (syntax/loc stx
         (case-lambda [formals body ... bodyn*] ...)))]
    
    [(if pred
         true-expr
         false-expr)
     (with-syntax ([true-expr* (instrument-expanded #'true-expr)]
                   [false-expr* (instrument-expanded #'false-expr)])
       (syntax/loc stx
         (if pred
             true-expr*
             false-expr*)))]
    
    [(begin expr ... exprn)
     (with-syntax ([exprn* (instrument-expanded #'exprn)])
       (syntax/loc stx (begin expr ... exprn*)))]

    [(begin0 expr0 expr ...)
     (with-syntax ([expr0* (instrument-expanded #'expr0)])
       (syntax/loc stx (begin expr0* expr ...)))]

    [(let-values ([(id) def-expr])
       body-expr ...)
     (with-syntax ([def-expr* (instrument-expanded #'def-expr)]
                   [(body-expr* ...) (map instrument-expanded (syntax->list #'(body-expr ...)))])
       (syntax/loc stx
         (letrec-values ([(def-expr-result def-expr-trace)
                          (parameterize ([current-trace '()])
                            (let ((def-expr-val def-expr*))
                              (values def-expr-val (current-trace))))])
           (let-values ([(id) def-expr-result])
             (annotation id def-expr-trace)
             body-expr* ...))))]

    [(letrec-values ([(id) def-expr])
       body-expr ...)
     (with-syntax* ([def-expr* (instrument-expanded #'def-expr)]
                    [def-expr**
                      #'(parameterize ([current-trace '()])
                          (begin0 def-expr*
                                  (annotation id (current-trace))
                                  ))]
                    [(body-expr* ...)
                     (map instrument-expanded
                          (syntax->list (syntax/loc stx (body-expr ...))))])
       (syntax/loc stx
         (letrec ([id def-expr**]) body-expr* ...)))]

    ;; Missing: set!

    [(quote form)
     (syntax/loc stx
       (trace 'value (quote form)))]

    [(quote-syntax expr) stx]
    
    [(quote-syntax expr #:local) stx]

    [(with-continuation-mark key-expr val-expr result-expr)
     (with-syntax ([result-expr* (instrument-expanded #'result-expr)])
       (syntax/loc stx (with-continuation-mark key-expr val-expr result-expr*)))]
    
    [(#%plain-app f args ...)
     (with-syntax ([(args* ...)
                    (map instrument-expanded
                         (syntax->list #'(args ...)))])
       (syntax/loc stx (trace f (f args* ...))))]

    [(%#expression expr)
     (instrument-expanded (syntax/loc stx expr))]

    ;; Retrieve the subtree for the given id and append to current-trace
    [id (syntax/loc stx
          (begin (current-trace (cons (get-annotation id) (current-trace)))
                 id))]))

(define-syntax (instrument stx)
  (syntax-case stx ()
    [(_ a) (let ([expanded (local-expand #'a 'expression '())])
             (instrument-expanded expanded))]))

;; Turn these into tests:
(instrument (let ([x (+ 1 3)]) (+ x x)))
(instrument 1)
(instrument (quote 1))
(instrument (+ 1 2 3))
(instrument (+ (if 1 2 3) 2))
(instrument (+ (and 1 2 3) (and 2 3 4)))
(instrument (begin0 1 2 3))
(instrument (let ((x (+ 1 2))) x))
(instrument (let-values (((x) (+ 1 2))) x))
(instrument (letrec ([y (+ 1 3)])
            (letrec ((x 5)) (+ x x))))
(instrument (let ([x (+ 1 3)])
            (let-values (((x) 5)) (+ x x))))
(instrument 1)
(instrument (let ((x 1)) 1))
(instrument (if #f (+ 1 2) (* (* (* 2 (if (+ (+ 2 1) 1) 2 3) 2) 2))))
(instrument ((if #t * +) 5 5))
;(w/helper '(a b c)) ;; => exception
(instrument (+ (if #f 3 4) 5))
