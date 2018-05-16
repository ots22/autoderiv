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

    ;; TODO: multiple bindings in let-values
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

    ;; TODO: multiple bindings in letrec-values
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
       (trace-value (quote form)))]

    [(quote-syntax expr) stx]
    
    [(quote-syntax expr #:local) stx]

    [(with-continuation-mark key-expr val-expr result-expr)
     (with-syntax ([result-expr* (instrument-expanded #'result-expr)])
       (syntax/loc stx (with-continuation-mark key-expr val-expr result-expr*)))]
    
    [(#%plain-app f args ...)
     (with-syntax ([(args* ...)
                    (map instrument-expanded
                         (syntax->list #'(args ...)))])
       (syntax/loc stx (trace-call f args* ...)))]

    [(%#expression expr)
     (instrument-expanded (syntax/loc stx expr))]

    ;; Retrieve the subtree for the given id and append to current-trace
    [id (syntax/loc stx
          (begin (if (has-annotation? id)
                  (current-trace (append (get-annotation id) (current-trace)))
                  (trace-var id))
                 id))]))

(define-syntax (instrument stx)
  (syntax-case stx ()
    [(_ a) (let ([expanded (local-expand #'a 'expression '())])
             (instrument-expanded expanded))]))

(define-syntax (value-and-trace stx)
  (syntax-case stx ()
    [(_ body) #'(parameterize ((current-trace '()))
                (println body)
                (println (current-trace)))]))

(module+ test
  (check-result-and-trace 8
                         `(((8 proc ,+) ((4 proc ,+) ((3 value)) ((1 value))) ((4 proc ,+) ((3 value)) ((1 value)))))
   (instrument (let ([x (+ 1 3)]) (+ x x))))


  (check-result-and-trace 1 '(((1 value)))
   (instrument 1))

  (check-result-and-trace 1 '(((1 value)))
   (instrument (quote 1)))

  (check-result-and-trace 6 `(((6 proc ,+) ((3 value)) ((2 value)) ((1 value))))
   (instrument (+ 1 2 3)))

  (check-result-and-trace 4 `(((4 proc ,+) ((2 value)) ((2 value))))
   (instrument (+ (if 1 2 3) 2)))

  (check-result-and-trace 7 `(((7 proc ,+) ((4 value)) ((3 value))))
   (instrument (+ (and 1 2 3) (and 2 3 4))))

  (check-result-and-trace 1 `(((1 value)))
   (instrument (begin0 1 2 3)))

  (check-result-and-trace 3 `(((3 proc ,+) ((2 value)) ((1 value))))
   (instrument (let ((x (+ 1 2))) x)))

  (check-result-and-trace 3 `(((3 proc ,+) ((2 value)) ((1 value))))
   (instrument (let-values (((x) (+ 1 2))) x)))

  ;;; multiple let bindings -- not yet implemented
  ;;(check-result-and-trace 10 `(((10 proc ,+) ((5 value)) ((5 value))))
  ;; (instrument (letrec ([y (+ 1 3)] [z 3])
  ;;               (letrec ((x 5)) (+ x x)))))
  ;;
  ;;(check-result-and-trace 7 `(((7 proc ,+) ((3 value)) ((4 proc ,+) ((3 value)) ((1 value)))))
  ;; (instrument (let ([y (+ 1 3)] [z 3]) (+ y z))))

  (check-result-and-trace 7 `(((7 proc ,+) ((6 proc ,*) ((3 var z)) ((2 var y))) ((1 var x))))
   (let ((x 1) (y 2) (z 3))
     (instrument
      (+ x (* y z)))))

  (check-result-and-trace 10 `(((10 proc ,+) ((5 value)) ((5 value))))
   (instrument (let ([x (+ 1 3)])
                 (let-values (((x) 5)) (+ x x)))))

  (check-result-and-trace 1 '(((1 value)))
   (instrument (let ((x 1)) 1)))

  (check-result-and-trace 16 `(((16 proc ,*) ((16 proc ,*) ((2 value)) ((8 proc ,*) ((2 value)) ((2 value)) ((2 value))))))
   (instrument (if #f (+ 1 2) (* (* (* 2 (if (+ (+ 2 1) 1) 2 3) 2) 2)))))

  (check-result-and-trace 25 `(((25 proc ,*) ((5 value)) ((5 value))))
   (instrument ((if #t * +) 5 5)))

  (check-result-and-trace 3 `(((3 proc ,+) ((2 var y)) ((1 var x))))
   (let ((x 1) (y 2)) (instrument (+ x y))))

  )
