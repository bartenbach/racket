#lang racket

(require racket/trace)
;; binary search tree
;; '()
;; '(x () ())
;; '(4 (3 () ()) (5 () ()))
;;  (root (left tree) (right tree))

;; functions get left subtree, get right subtree, get leaf, etc
(define (left-subtree tree) (car (cdr tree)))
(define (right-subtree tree) (car (cdr (cdr tree))))
(define (isLeaf tree x))

(define (bst-insert tree x)
  (cond
    ([null? tree] (list x null null))
    ([< x (car tree)] (list (car tree) (bst-insert (car (cdr tree)) x) (car (cdr (cdr tree))))) 
    ([> x (car tree)] (list (car tree) (car (cdr tree)) (bst-insert (car (cdr (cdr tree))) x)))
    (else tree)))

(trace-define (bst-delete tree x)
  (cond
    ([null? (car tree)] null)
    ([empty? (car tree)] null)
    ([isLeaf? x] tree without x)
    ([hasOneChild? x] relink x.parent to x.child)
    ([hasTwoChildren? x] delete and relink to smallest node in right tree)
    ([not(= x (car tree))]
  
(trace-define (empty-tree? tree)
  (cond
    ([null? (car tree)] true)
    (else false)))

(define (bst-contains? tree x)
  (cond
    ([empty? tree] false)
    ([null? (car tree)] false)
    ([= x (car tree)] true)
    ([< x (car tree)] (bst-contains? (left-subtree tree) x))
    ([> x (car tree)] (bst-contains? (right-subtree tree) x))))

(define (bst-height tree)
  (cond
    ([null? tree] 0)
    ([empty? (car tree)] 0)
    (else (+ 1 (max (bst-height (left-subtree tree)) (bst-height (right-subtree tree)))))))

(define (bst-size tree)
  (cond
    ([null? tree] 0)
    ([empty? (car tree)] 0)
    (else (+ 1 (bst-size (left-subtree tree)) (bst-size (right-subtree tree))))))

(define a (bst-insert null 5))
(define b (bst-insert null null))
(define c (bst-insert (bst-insert (bst-insert null 5) 8) 2))
(define d (bst-insert (bst-insert (bst-insert (bst-insert (bst-insert null 5) 8) 2) 6) 7))