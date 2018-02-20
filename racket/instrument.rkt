#lang racket
(require racket/syntax
         syntax/parse
         (for-syntax
           racket
           racket/syntax
           syntax/parse))

;; print each intermediate function evaluation
(define (add-to-tape form)
  (displayln form)
  form)

;; tape-helper works on expanded forms
(define-syntax (tape-helper/orig stx)
  (syntax-parse stx #:literals (#%plain-app quote)
                [(_) #'()]
                [(_ (args ...)) #'(tape-helper/orig args ...)] ;; descend into lists
                [(_ #%plain-app f args ...) #'(add-to-tape (apply f (tape-helper/orig args ...)))]
                [(_ quote arg) #'(add-to-tape (quote arg))]
                [(_ arg1 rest ...+) #'(list (tape-helper/orig arg1) (tape-helper/orig rest ...))] ;; two or more
                [(_ arg1) #'(add-to-tape arg1)]))
;; fixed for quote, but this way will need to deal with every special form (e.g still fails for if)
;; want to pass through everything (except quote?) - seems hard to make a general statement
;; there are some special functions for handling this sort of thing in
;; one of the syntax modules (stx(?) something like kernel forms)
;; xxxxFIXED currently fails on 'quote':  (tape-helper (quote 5)) => (tape-helper quote 5)

;; another attempt (doesn't work)
(define-syntax (tape-helper stx)
  (displayln "in-helper")
  (syntax-parse stx #:literals (#%plain-app)
                [(_) #'()]
                [(_ (#%plain-app f args ...)) #'((add-to-tape (apply f (tape-helper args ...))))]
                [(_ (arg1 args ...)) #'(arg1 (tape-helper args ...))]
                ;;[(_ (args ...)) #'(tape-helper args ...)] ;; descend into lists
                [(_ arg1 rest ...+) #'(list (tape-helper arg1) (tape-helper rest ...))] ;; two or more
                [(_ arg1) #'(add-to-tape arg1)]))

(define-syntax (w/helper stx)
  (syntax-case stx ()
    [(_ a) (let ([expanded (local-expand #'a 'expression '())])
             #`(tape-helper/orig #,expanded))]))

(w/helper (+ 1 2))
;(w/helper (+ (if #f 3 4) 5))
