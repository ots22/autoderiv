#lang racket
(require racket/syntax
         syntax/parse
         rackunit
         "trace.rkt"
         (for-syntax
           racket
           racket/syntax
           syntax/parse))

;; print each intermediate function evaluation
(define (add-to-tape form)
  (displayln form)
  form)

;; tape-helper works on expanded forms
(define-for-syntax (tape-helper stx)
  (syntax-parse stx #:literals (#%plain-app quote)
    [(#%plain-app f args ...)
     (with-syntax ([(transformed-args ...)
                    (map tape-helper (syntax->list #'(args ...)))])
       #'(trace f (f transformed-args ...)))]

    [(quote arg) #'(trace 'value arg)]
    
    [(args ...) (with-syntax ([(transformed-args ...)
                               (map tape-helper (syntax->list #'(args ...)))])
                  (displayln (syntax-local-context))
                  #'(trace 'other (transformed-args ...)))]

    [arg #'arg] ;; anything else
    
    ;[arg #'arg] ;; ignore anything else
    ;; literals
    ;[(_ (arg0 rest ...)) #'((tape-helper arg0) (tape-helper ...)] ;; descend into lists
    ;[(_ args ...) #'(args ...)] ;; ignore everything else
    ))

;(tape-helper #'(#%plain-app + (#%plain-app + 1 1) 2))

(define-syntax (w/helper stx)
  (syntax-case stx ()
    [(_ a) (let ([expanded (local-expand #'a 'expression '())])
             (tape-helper expanded))]))



(w/helper (+ (if 1 2 3) 2))
(w/helper 1)
;(w/helper (let ((x 1)) 1))
;(w/helper (if #f (+ 1 2) (* (* (* 2 (if (+ (+ 2 1) 1) 2 3) 2) 2))))
;(w/helper ((if #t * +) 5 5))
;(w/helper '(a b c))
;(module+ test (check-values (w/helper (+ 1 2))))
;(w/helper (+ (if #f 3 4) 5))
