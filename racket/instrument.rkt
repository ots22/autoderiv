#lang racket
(require racket/syntax
         syntax/parse
         rackunit
         "trace.rkt"
         (for-syntax
           racket
           racket/syntax
           syntax/parse
           syntax/kerncase))

;; print each intermediate function evaluation
(define (add-to-tape form)
  (displayln form)
  form)

;; tape-helper works on expanded forms
;(define-for-syntax (tape-helper stx)
;  (syntax-parse stx #:literals (#%plain-app quote)
;    [(#%plain-app f args ...)
;     (with-syntax ([(transformed-args ...)
;                    (map tape-helper (syntax->list #'(args ...)))])
;       #'(trace f (f transformed-args ...)))]
;
;    [(quote arg) #'(trace 'value arg)]
;    
;    [(args ...) (with-syntax ([(transformed-args ...)
;                               (map tape-helper (syntax->list #'(args ...)))])
;                  (displayln (syntax-local-context))
;                  #'(trace 'other (transformed-args ...)))]
;
;    [arg #'arg] ;; anything else
;    ))

(define-for-syntax (tape-helper stx)
  (kernel-syntax-case stx #f
    [(#%plain-app f args ...)
     (with-syntax ([(args* ...)
                    (map tape-helper (syntax->list #'(args ...)))])
       (syntax/loc stx (trace f (f args* ...))))]

    [(quote form)
     (syntax/loc stx
       (trace 'value (quote form)))]

    [(if pred
         true-expr
         false-expr)
     (with-syntax ([true-expr* (tape-helper #'true-expr)]
                   [false-expr* (tape-helper #'false-expr)])
       (syntax/loc stx
         (if pred
             true-expr*
             false-expr*)))]

    [(%#expression expr)
     (tape-helper #'expr)]

    [(begin expr ... exprn)
     (with-syntax ([exprn* (tape-helper #'exprn)])
       (syntax/loc stx (begin expr ... exprn*)))]

    [(begin0 expr0 expr ...)
     (with-syntax ([expr0* (tape-helper #'expr0)])
       (syntax/loc stx (begin expr0* expr ...)))]

    [(quote-syntax expr) stx]

    [(let-values ([(id ...) expr0] ...)
       expr1 ...)
     (with-syntax ([(expr0* ...)
                    (map tape-helper (syntax->list #'(expr0 ...)))])
       (syntax/loc stx
         (let-values ([(id ...) expr0] ...)
           expr1 ...)))]

    ;[(let-values ([(id ...) expr0] ...)
    ;   expr1 ...) stx]

    [(#%plain-lambda formals body ... bodyn)
     (with-syntax ([bodyn* (tape-helper #'bodyn)])
       (syntax/loc stx
         (#%plain-lambda formals body ... body*)))]

    ;;[(case-lambda [] ) ???]

    ;;[(letrec-values ([(id ...) expr0] ...)
    ;;   expr1 ...+)
    ;; ???]
    
    [(with-continuation-mark key-expr val-expr result-expr)
     (with-syntax ([result-expr* (tape-helper #'result-expr)])
       (syntax/loc stx (with-continuation-mark key-expr val-expr result-expr*)))]
    ))

;(tape-helper #'(#%plain-app + (#%plain-app + 1 1) 2))

(define-syntax (w/helper stx)
  (syntax-case stx ()
    [(_ a) (let ([expanded (local-expand #'a 'expression '())])
             (tape-helper expanded))]))


;(w/helper (+ 1 2 3))
;(w/helper (+ (if 1 2 3) 2))
;(w/helper (+ (and 1 2 3) (and 2 3 4)))
;(define a (w/helper (+ (and 1 2 3) (and 2 3 4))))
;(w/helper (begin0 1 2 3))

;(w/helper (let ((x (+ 1 2))) x))
(w/helper (let-values (((x) (+ 1 2))) x))

;(and 1 2)
;(w/helper 1)
;(w/helper (let ((x 1)) 1))
;(w/helper (if #f (+ 1 2) (* (* (* 2 (if (+ (+ 2 1) 1) 2 3) 2) 2))))
;(w/helper ((if #t * +) 5 5))
;(w/helper '(a b c))
;(module+ test (check-values (w/helper (+ 1 2))))
;(w/helper (+ (if #f 3 4) 5))
