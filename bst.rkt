#lang racket

(require racket/trace)
(require rackunit)
;; binary search tree
;; '()
;; '(x () ())
;; '(4 (3 () ()) (5 () ()))
;;  (root (left tree) (right tree))

; returns the left subtree of a given tree
(define (left-subtree tree)
  (car (cdr tree)))

; returns the right subtree of a given tree
(define (right-subtree tree)
  (car (cdr (cdr tree))))

; returns the subtree of a tree starting with node 'x' and all children
(define (get-subtree tree x)
  (cond
    ([and (null? (left-subtree tree)) (null? (right-subtree tree))] null)
    ([= x (car (left-subtree tree))] x)
    ([= x (car (right-subtree tree))] x)
    (else (get-subtree (left-subtree tree) x) (get-subtree (right-subtree tree) x))))

; Returns true if x in tree is a leaf - otherwise false
(trace-define (isLeaf? tree x)
  (cond
    ; the tree is empty
    ([or (empty? tree) (empty? (car tree))] #f)
    ; the root is equal to x and both subtrees are null
    ([and (= x (car tree)) (null? (left-subtree tree)) (null? (right-subtree tree))] #t)
    ; the root is not equal to x but both subtrees are null
    ([and (null? (left-subtree tree)) (null? (right-subtree tree))] #f)
    ; subtrees are not null, recursively check subtrees
    ([< x (car tree)] (isLeaf? (left-subtree tree) x))
    ([> x (car tree)] (isLeaf? (right-subtree tree) x))
    (else #f)))
    ;(else (isLeaf? (left-subtree tree) x) (isLeaf? (right-subtree tree) x))))
    ;([and (null? (left-subtree (get-subtree tree x))) (null? (right-subtree (get-subtree tree x)))] #t)
    ;(else #f)))


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
    ([isLeaf? tree x] tree) ; return tree without x
    ;([hasOneChild? x] relink x.parent to x.child)
    ;([hasTwoChildren? x] delete and relink to smallest node in right tree)
    ([not(= x (car tree))])))
  
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
    ([empty? (car tree)] -1)
    ([and (empty? (left-subtree tree)) (empty? (right-subtree tree))] 0)
    (else (+ 1 (max (bst-height (left-subtree tree)) (bst-height (right-subtree tree)))))))

(define (bst-size tree)
  (cond
    ([null? tree] 0)
    ([empty? (car tree)] 0)
    (else (+ 1 (bst-size (left-subtree tree)) (bst-size (right-subtree tree))))))

; tree definitions
(define a (bst-insert null 5))
(define b (bst-insert null null))
(define c (bst-insert (bst-insert (bst-insert null 5) 8) 2))
(define d (bst-insert (bst-insert (bst-insert (bst-insert (bst-insert null 5) 8) 2) 6) 7))

; unit tests
(check-equal? '(5 () ())
              (bst-insert null 5))
(check-equal? '(() () ())
              (bst-insert null null))
(check-equal? '(5
                (2 () ()) (8 () ()))
              (bst-insert (bst-insert (bst-insert null 5) 8) 2))
(check-equal? '(5
                (2 () ()) (8
                       (6 ()
                          (7 () ())) ()))
              (bst-insert (bst-insert (bst-insert (bst-insert (bst-insert null 5) 8) 2) 6) 7))
(check-equal? (empty-tree? b) #t "Empty tree should return true")
(check-equal? (empty-tree? c) #f "Non-empty tree should return false")
(check-equal? (bst-size a) 1 "One node size test")
(check-equal? (bst-size b) 0 "Empty tree size test")
(check-equal? (bst-size c) 3 "Three node size test")
(check-equal? (isLeaf? a 5) #t "Root with empty tree true case")
(check-equal? (isLeaf? a 10) #f "Root with empty tree false case")
(check-equal? (isLeaf? b 5) #f "Completely empty tree")
(check-equal? (isLeaf? c 5) #f "Tree with subtrees root should not be leaf")
(check-equal? (isLeaf? c 8) #t "Is a leaf")
(check-equal? (isLeaf? c 10) #f "Not a leaf")
(check-equal? (isLeaf? d 5) #f "Not a leaf")
(check-equal? (isLeaf? d 2) #t "Not a leaf")
(check-equal? (isLeaf? d 8) #f "Not a leaf")
(check-equal? (isLeaf? d 6) #f "Not a leaf")
(check-equal? (isLeaf? d 7) #t "Is a leaf")

(check-equal? (bst-height a) 0 "Height of a tree with only one node should be zero")
(check-equal? (bst-height b) -1 "Height of a tree with zero nodes should be -1")
(check-equal? (bst-height c) 1 "Height of node with two children should be 1")

