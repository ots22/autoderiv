#lang racket
(require pict
         pict/tree-layout
         pict/code
         rackunit
         "util.rkt"
         "trace.rkt")

(provide render-trace)

(define (procname p)
  (cond [(object-name p) => identity]
        [else p]))

(define (node-pict node)
  (let ((label (node-label node))
        (value (node-value node)))
    (if (procedure? label)
        (code #,(procname label) (code:comment #,(format "=> ~a" value)))
        (code #,value))))

(define (render-trace tree)
  (cond
   ;; Nothing in the tree, nothing to do
   [(null? tree)
    '()] 
   ;; At a labelled subtree if the head of it is a node.
   ;; Make edges from it to the result of a recursive call.
   [(node? (car tree))
    (let ((edges (map tree-edge (render-trace (cdr tree))))) 
      (apply tree-layout #:pict (node-pict (car tree)) edges))]
   ;; Otherwise, it is a list of sub-trees
   [else (map render-trace tree)]))

(define (render-example)
  (let ([eg-tree `(((,+ . 5)
                    ((value . 2)) ((value . 3))))])
    (scale-to-fit
     (naive-layered (car (render-trace eg-tree)))
     300 200)))

(module+ test
  (check-not-exn render-example))
