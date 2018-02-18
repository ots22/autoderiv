#lang racket
(require pict pict/tree-layout pict/code)

(define eg-tree `(((,+ . 8) ((value . 3)) ((value . 5)))))
(define eg-tree2 `(((,+ . 7) ((value . 1)) ((,* . 6) ((value . 2)) ((value . 3))))))

(define (atom? x)
  (not (or (pair? x) (null? x))))

(define (node? x)
  (and (pair? x)
       (atom? (car x))
       (atom? (cdr x))))

(define (node-label node) (car node))
(define (node-value node) (cdr node))

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

(provide render-trace)

(render-trace eg-tree2)

(scale-to-fit
 (naive-layered (car (render-trace eg-tree2)))
 300 200)