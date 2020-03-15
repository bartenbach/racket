#lang racket

(require racket/trace)
(require rackunit)
;; binary search tree
;; '()
;; '(x () ())
;; '(4 (3 () ()) (5 () ()))
;;  (root (left tree) (right tree))

; Returns the left subtree of a given tree
(define (left-subtree tree)
  (car (cdr tree)))

; Returns the right subtree of a given tree
(define (right-subtree tree)
  (car (cdr (cdr tree))))

; Returns true if x in the tree is a leaf - otherwise false
(define (isLeaf? tree x)
  (cond
    ; the tree is empty
    ([or (empty? tree) (empty? (car tree))] #f)
    ; the root is equal to x and both subtrees are null
    ([and (= x (car tree)) (null? (left-subtree tree)) (null? (right-subtree tree))] #t)
    ; the root is not equal to x but both subtrees are null
    ([and (null? (left-subtree tree)) (null? (right-subtree tree))] #f)
    ; subtrees are not null, tail recursively check subtrees
    ([< x (car tree)] (isLeaf? (left-subtree tree) x))
    ([> x (car tree)] (isLeaf? (right-subtree tree) x))
    (else #f)))

; Returns the largest node in a binary search tree.
(define (max-node tree)
  (cond
    ([null? (right-subtree tree)] (car tree))
    (else (max-node (right-subtree tree)))))

; Returns true iff the provided binary search tree (or subset) has only one child.
(define (hasOneChild? x)
  (cond
    ([xor (null? (left-subtree x)) (null? (right-subtree x))] #t)
    (else #f)))

; Returns true if the provided binary search tree (or subset) has two child nodes.
(define (hasTwoChildren? x)
  (cond
    ([and (not(null? (left-subtree x))) (not(null? (right-subtree x)))] #t)
    (else #f)))

; Provides an interface to create binary search tree objects and insert new nodes into existing binary search trees.
(define (bst-insert tree x)
  (cond
    ([null? tree] (list x null null))
    ([< x (car tree)] (list (car tree) (bst-insert (car (cdr tree)) x) (car (cdr (cdr tree))))) 
    ([> x (car tree)] (list (car tree) (car (cdr tree)) (bst-insert (car (cdr (cdr tree))) x)))
    (else tree)))

; Provides the interface for deleting a node 'x' from a given binary search tree.
(define (bst-delete tree x)
  (cond
    ([not(bst-contains? tree x)] tree) ; this is identical to Java's behavior with ArrayList.remove()
    ([and (empty? (left-subtree tree)) (empty? (right-subtree tree)) (= x (car tree))] '(() () ())) ; just a root node
    (else (bst-delete-recursive tree x))))

; The 'private' function called by bst-delete that recursively calls itself to delete a node from a binary search tree.
(define (bst-delete-recursive tree x)
  (cond
    ([null? tree] null)
    ([isLeaf? tree x] (leaf-deletion tree x))
    ([< x (car tree)] (cons (car tree) (cons ((bst-delete-recursive (left-subtree tree) x) (right-subtree tree)))))
    ([> x (car tree)] (cons (car tree) (cons (left-subtree tree) (bst-delete-recursive (right-subtree tree) x))))
    ([and (= x (car tree)) (hasOneChild? tree)] (relink-child tree x))
    ([and (= x (car tree)) (hasTwoChildren? tree)] (cons (relink-children tree x) null))))

; Relinks a binary search tree with only one child without 'x'.
(define (relink-child tree x)
  (cond
    ([null? (right-subtree tree)] (cons (left-subtree tree) null))
    (else (cons (right-subtree tree) null))))

; Provides functionality to relink a binary search tree that has two children.
; It does so by returning a binary search tree with the largest node in the right subtree replacing
; the node we are deleting.  The left subtree is then linked as it was previously.  The right subtree
; is linked again as the right subtree but minus the value we promoted to the new 'root' of the provided tree.
(define (relink-children tree x)
    (cons (max-node (right-subtree tree))
           (cons (left-subtree tree)
                 (cons (bst-delete (right-subtree tree) (max-node (right-subtree tree))) null))))

; Returns a completely empty binary search tree.
(define (empty-bst)
  (cons null (cons null (cons null '()))))

; Returns a binary search tree with a given leaf 'x' removed deleted.
(define (leaf-deletion tree x)
  (cond
    ([< x (car tree)] (cons (car tree) (cons (empty-bst) (cdr (cdr tree)))))
    (else (cons (car tree) (cons (car (cdr tree)) (cons (empty-bst) null))))))

; Returns true if a given binary search tree is empty, otherwise false.
(define (empty-tree? tree)
  (cond
    ([null? (car tree)] true)
    (else false)))

; TODO - unfinished...would be nice though.
(define (bst-print tree)
  (print "  ")
  (print (car tree))
  (newline)
  (print (right-subtree tree))
  (print " ")
  (print (left-subtree tree)))

; Returns true if provided tree contains value 'x', otherwise false
(define (bst-contains? tree x)
  (cond
    ([empty? tree] #f)
    ([null? (car tree)] #f)
    ([= x (car tree)] #t)
    ([< x (car tree)] (bst-contains? (left-subtree tree) x))
    ([> x (car tree)] (bst-contains? (right-subtree tree) x))))

; Returns the height of a given binary search tree.
(define (bst-height tree)
  (cond
    ([empty? (car tree)] -1)
    ([and (empty? (left-subtree tree)) (empty? (right-subtree tree))] 0)
    (else (+ 1 (max (bst-height (left-subtree tree)) (bst-height (right-subtree tree)))))))

; Returns the number of nodes in a given binary search tree.
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
(define e (bst-insert (bst-insert (bst-insert (bst-insert (bst-insert null 5) 8) 2) 6) 9))
(define f (bst-insert (bst-insert (bst-insert (bst-insert (bst-insert null 5) 9) 2) 6) 7))

; unit tests
(check-equal? '(5 () ()) a)
(check-equal? '(() () ()) b)
(check-equal? '(5(2 () ()) (8 () ())) c)
(check-equal? '(5(2 () ()) (8(6 ()(7 () ())) ())) d)
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
(check-equal? (bst-contains? a 5) #t "Tree a contains 5")
(check-equal? (bst-delete a 5) '(() () ()) "Deleting 5 from a should yield an empty tree")
(check-equal? (bst-delete b 10) b "Deleting nothing should do nothing") ; this is questionable behavior, but mimics Java
(check-equal? (bst-delete c 8) '(5 (2 () ()) (() () ())) "Testing leaf deletion on tree c")
(check-equal? (bst-delete c 2) '(5 (() () ()) (8 () ())) "Testing leaf deletion on tree c")
(check-equal? (bst-delete d 8) '(5 (2 () ()) (6 () (7 () ()))) "Testing one child deletion on d")
(check-equal? (bst-delete e 8) '(5 (2 () ()) (9 (6 () ()) (() () ()))) "Testing two child deletion on e")
(check-equal? (bst-delete f 2) '(5 (() () ()) (9 (6 () (7 () ())) ())) "Testing one child deletion on f")
(check-equal? (bst-delete f 9) '(5 (2 () ()) (6 () (7 () ()))) "Testing two child deletion on f")
; TODO - this could use a more rigorous suite of test cases performing many different deletion scenarios
(check-equal? (max-node a) 5 "Testing largest node in a tree is 5")
(check-equal? (max-node c) 8 "Testing largest node in c tree is 8")
(check-equal? (max-node e) 9 "Testing largest node in e tree is 9")
(check-equal? (empty-bst) '(() () ()) "Testing empty binary search tree generates correctly")