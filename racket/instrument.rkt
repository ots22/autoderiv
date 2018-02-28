#lang racket
(require racket/syntax
         syntax/parse
         rackunit
         "trace.rkt"
         "binding-trace.rkt"
         (for-syntax
          "trace.rkt"
          "binding-trace.rkt"
           racket
           racket/syntax
           syntax/parse
           syntax/kerncase
           syntax/strip-context))

(define-for-syntax (tape-helper stx)
  (kernel-syntax-case stx #f
    [(#%plain-lambda formals body ... bodyn)
     (with-syntax ([bodyn* (tape-helper #'bodyn)])
       (syntax/loc stx
         (#%plain-lambda formals body ... bodyn*)))]

    [(case-lambda [formals body ... bodyn] ...)
     (with-syntax ([(bodyn* ...)
                    (map tape-helper (syntax->list #'(bodyn ...)))])
       (syntax/loc stx
         (case-lambda [formals body ... bodyn*] ...)))]
    
    [(if pred
         true-expr
         false-expr)
     (with-syntax ([true-expr* (tape-helper #'true-expr)]
                   [false-expr* (tape-helper #'false-expr)])
       (syntax/loc stx
         (if pred
             true-expr*
             false-expr*)))]
    
    [(begin expr ... exprn)
     (with-syntax ([exprn* (tape-helper #'exprn)])
       (syntax/loc stx (begin expr ... exprn*)))]

    [(begin0 expr0 expr ...)
     (with-syntax ([expr0* (tape-helper #'expr0)])
       (syntax/loc stx (begin expr0* expr ...)))]

    [(let-values ([(id) def-expr])
       body-expr ...)
     (with-syntax ([def-expr* (tape-helper #'def-expr)]
                   [(body-expr* ...) (map tape-helper (syntax->list #'(body-expr ...)))])
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
     (with-syntax* ([def-expr* (tape-helper #'def-expr)]
                    [def-expr**
                      #'(parameterize ([current-trace '()])
                          (begin0 def-expr*
                                  (annotation id (current-trace))
                                  ))]
                    [(body-expr* ...)
                     (map tape-helper
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
     (with-syntax ([result-expr* (tape-helper #'result-expr)])
       (syntax/loc stx (with-continuation-mark key-expr val-expr result-expr*)))]
    
    [(#%plain-app f args ...)
     (with-syntax ([(args* ...)
                    (map tape-helper
                         (syntax->list #'(args ...)))])
       (syntax/loc stx (trace f (f args* ...))))]

    [(%#expression expr)
     (tape-helper (syntax/loc stx expr))]

    ;; Retrieve the subtree for the given id and append to current-trace
    [id (syntax/loc stx
          (begin (current-trace (cons (get-annotation id) (current-trace)))
                 id))]))

(define-syntax (w/helper stx)
  (syntax-case stx ()
    [(_ a) (let ([expanded (local-expand #'a 'expression '())])
             (tape-helper expanded))]))

;(w/helper (let ([x (+ 1 3)]) (+ x x)))
;(w/helper 1)
;(w/helper (quote 1))
;(w/helper (+ 1 2 3))
;(w/helper (+ (if 1 2 3) 2))
;(w/helper (+ (and 1 2 3) (and 2 3 4)))
;(define a (w/helper (+ (and 1 2 3) (and 2 3 4))))
;(w/helper (begin0 1 2 3))

;(w/helper (let ((x (+ 1 2))) x))
;(w/helper (let-values (((x) (+ 1 2))) x))

;(let-values (((x) (+ 1 2))) x)
;; this is fine:
;(w/helper (letrec ([y (+ 1 3)])
;            (letrec ((x 5)) (+ x x))))

(w/helper (let ([x (+ 1 3)])
            (let-values (((x) 5)) (+ x x))))
;(no-expand (let-values (((x) (+ 1 3)))
;             (let-values (((x) 5))
;               (+ x x))))

;(and 1 2)
;(w/helper 1)
;(w/helper (let ((x 1)) 1))
;(w/helper (if #f (+ 1 2) (* (* (* 2 (if (+ (+ 2 1) 1) 2 3) 2) 2))))
;(w/helper ((if #t * +) 5 5))
;(w/helper '(a b c))
;(module+ test (check-values (w/helper (+ 1 2))))
;(w/helper (+ (if #f 3 4) 5))
