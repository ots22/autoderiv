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

     ;; Note: don't handle multiple values
    ;[(let-values ([(id) def-expr] ...)
    ;   body-expr ...)
    ; (with-syntax ([(def-expr* ...)
    ;                (map tape-helper (syntax->list #'(def-expr ...)))]
    ;               [(body-expr* ...)
    ;                (map tape-helper
    ;                     (syntax->list (syntax/loc stx (body-expr ...))))])
    ;   (syntax/loc stx
    ;     (#%plain-app (#%plain-lambda (id ...) body-expr* ...)
    ;                  def-expr* ...)))]
    [(letrec-values ([(id) def-expr])
       body-expr ...)
     (with-syntax* ([def-expr* (tape-helper #'def-expr)]
                    [def-expr**
                      (syntax/loc stx (parameterize ([current-trace '()])
                                        (begin0 def-expr*
                                                (annotation id (current-trace))
                                                )))]
                    [(body-expr* ...)
                     (map tape-helper
                          (syntax->list (syntax/loc stx (body-expr ...))))])
     ; (with-syntax ([def-expr* #`(begin0 def-expr
     ;                                   (annotation id (current-trace)))]
     ;              [(body-expr* ...) (map tape-helper (syntax->list #'(body-expr ...)))])
       (syntax/loc stx
         (letrec ([id def-expr**]) body-expr* ...)))
     ]

    ;; Missing: letrec

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

    ;; retrieve the subtree for the given id
    [id (syntax/loc stx
          (begin (current-trace (cons (get-annotation id) (current-trace)))
                 id))]))

(define-syntax (w/helper stx)
  (syntax-case stx ()
    [(_ a) (let ([expanded (local-expand #'a 'expression '())])
             (tape-helper expanded))]))

(define-syntax (no-expand stx)
  (syntax-case stx ()
    [(_ a) (tape-helper #'a)]))

;(w/helper (let ([x (+ 1 3)]) (+ x x)))
(w/helper 1)
(w/helper (quote 1))
(w/helper (+ 1 2 3))
(w/helper (+ (if 1 2 3) 2))
(w/helper (+ (and 1 2 3) (and 2 3 4)))
;(define a (w/helper (+ (and 1 2 3) (and 2 3 4))))
(w/helper (begin0 1 2 3))

;(w/helper (let ((x (+ 1 2))) x))
;(w/helper (let-values (((x) (+ 1 2))) x))

(let-values (((x) (+ 1 2))) x)
;; this is fine:
(w/helper (letrec ([y (+ 1 3)])
            (letrec ((x 5)) (+ x x))))
;; this produces "identifier's binding is ambiguous", but works when manually expanding - probably need to think carefully about context etc
(w/helper (letrec ([x (+ 1 3)])
            (letrec ((x 5)) (+ x x))))
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
