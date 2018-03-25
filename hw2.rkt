#lang racket

(require racket/trace)
; ********************************************************
; CS 201 HW #1  DUE Wednesday, 9/27/2017 at 11:59 pm
;                via the submit system on the Zoo

; ********************************************************
; Name: Olivia Roth
; Email address: olivia.roth@yale.edu
; ********************************************************

; This file may be opened in DrRacket.
; Lines beginning with semicolons are comments.

; If you are asked to write a procedure, please make sure it has 
; the specified name, and the specified number and order of arguments.  
; The names of the formal arguments need not be the same
; as in the problem specification.

; For each problem, the intended inputs to your procedures
; are specified (for example, "positive integers") 
; and your procedures need not do anything reasonable 
; for other possible inputs.

; You may write auxiliary procedures in addition to
; the requested one(s) -- for each of your auxiliary 
; procedures, please include a comment explaining
; what it does, and giving an example or two.

; You may also use procedures you have written 
; elsewhere in this assignment or previous assignments.
; They only need to be defined once somewhere within this file.

; Please use the predicate equal? to test equality
; of values that may not be numbers.  To test equality
; of numbers, you can use =.

; Also, most of these procedures involve car/cdr recursion.
; In that case, your code needs to implement the recursion, not
; simply invoke a racket procedure that allows you to avoid the problem.

; ********************************************************
; ** problem 0 ** (1 easy point) 
; Replace the number 0 in the definition below to indicate
; the number of hours you spent doing this assignment
; Decimal numbers (eg, 6.237) are fine.  Exclude time
; spent reading.

(define hours 4.2)

; ********************************************************
; ** problem 1 ** (9 points)
; Write a procedure

; (depth tree)

; which takes a nest list tree as input and return an integer 
; indicating the maximum level of the tree.

; Examples

; (depth '()) => 0
; (depth '(1 2 3)) => 1
; (depth '(a (b (c (d))))) => 4
; (depth '((((((0))))))) => 6

; ********************************************************

(define (depth tree)
  (cond
    ((empty? tree) 0)
    ((list? tree)(max (+ 1 (depth (first tree)))(depth (rest tree))))
    (else 0)))
     
       
; ********************************************************
; ** problem 2 ** (10 points)
; Write a procedure

; (count-odd tree)

; that takes a nest list tree and returns another the number of 
; elements which are odd

; Examples

; (count-odd '(1 2 3)) => 2
; (count-odd '(1 (2 (3)))) => 2
; (count-odd '()) => 0
; (count-odd '((((((8 8 8))))))) => 0
; (count-odd '((((((9 9 9))))))) => 3


; ********************************************************
(define (count-odd lst)
  (cond ((null? lst) 0)
	((number? lst)
         (if (odd? lst)
             1
             0))
	((list? lst)
	 (+ (count-odd (car lst))
	    (count-odd (cdr lst))))))

; ********************************************************
; ** problem 3 ** (10 points)
; Write a procedure

; (count-pred pred tree)

; that takes a nest list tree and returns another the number of 
; elements which satisfy the given predicate


; Examples

; (count-pred odd? '(1 2 3)) => 2
; (count-pred even? '(1 2 3)) => 1
; (count-pred integer? '(1 (2 (3)))) => 3
; (count-pred string? '()) => 0
; (count-pred even? '((((((8 8 8))))))) => 3
; (count-pred (lambda (x) (> x 5)) '((((((9 9 9))))))) => 3

; ********************************************************
(define (count-pred pred lst)
  (cond ((null? lst) 0)
	((list? lst)
	 (+ (count-pred pred (car lst))
	    (count-pred pred (cdr lst))))
        ((pred lst) 1)
        (else 0)))

; ********************************************************
; ** problem 4 ** (10 points)
; Write a procedure

; (find-type type tree)

; that takes a data type and a nested list tree and returns #t if
; at least one of the leaf nodes is of the given type

; Examples:

; (find-type 'integer '((1 2 3))) => #t
; (find-type 'flonum '((1 2 3))) => #f
; (find-type 'flonum '((1 2 1.3))) => #t
; (find-type 'symbol '((1 2 3))) => #f
; (find-type 'symbol '((a b c))) => #t
; (find-type 'string '((a ("b") c))) => #t
; (find-type 'character '((a ("b") #\c))) => #t

;; boolean types are legal
; ********************************************************
(define (find-type type tree)
  (cond ((equal? 'integer type)
         (if (equal? (count-pred exact-integer? tree) 0)
             #f
             #t))
        ((equal? 'flonum type)
         (if (equal? (count-pred flonum? tree) 0)
             #f
             #t))
        ((equal? 'symbol type)
         (if (equal? (count-pred symbol? tree) 0)
             #f
             #t))
        ((equal? 'string type)
         (if (equal? (count-pred string? tree) 0)
             #f
             #t))
        ((equal? 'character type)
         (if (equal? (count-pred char? tree) 0)
             #f
             #t))
        ((equal? 'boolean type)
          (if (equal? (count-pred boolean? tree) 0)
             #f
             #t))
        ( else #f )))

; ********************************************************
; ** problem 5 ** (10 points)
; Write a procedure

; (types tree)

; that takes a nested list tree and returns a list of the data
; types in the tree in alphbetical order

; Examples

; (types '(1 1 1 1)) => '(integer)
; (types '(1 ((a)))) => '(integer symbol)
; (types '(1 (("a")))) => '(integer string)
; (types '(1 ((#\a)))) => '(character integer)
; (types '(1.0 ((#\a)))) => '(character flonum)
;; boolean types are legal

; ********************************************************
(define (types tree)
  (cond ((null? tree) '())
	((list? tree)
         (append
          (if (find-type 'boolean tree)'(boolean) '())
          (if (find-type 'character tree)'(character) '())
          (if (find-type 'flonum tree)'(flonum) '())
          (if (find-type 'integer tree)'(integer) '())
          (if (find-type 'string tree)'(string) '())
          (if (find-type 'symbol tree)'(symbol) '())))))

; ********************************************************
; ** problem 6 ** (10 points)
; Write a procedure

; (cull pred lst)

; that takes a predicate pred and a list lst and returns a new list
; of two lists: the first list comprises the elements that satisfy
; pred, and the second list comprises elements that do not.

; Examples:

; (cull even? '(1 2 3 4 5 6)) => '((2 4 6) (1 3 5))
; (cull symbol? '(1 2 a b 3 3 d e)) => '((a b d e) (1 2 3 3))
; (cull even? '(2 4 6)) => '((2 4 6) ())
; (cull odd? '(2 4 6)) => '(() (2 4 6))
; (cull odd? '()) => '(() ())

; ********************************************************
;
(define (cull pred lst)
  (append (cons(cHelpY pred lst (list))'()) (cons (cHelpN pred lst (list)) '())))

(define (cHelpY pred tree yes)
  ;this returns a list of items that satify the predicate
  ;this method takes a predicate (pred), the list passed into the orignial method (tree), and an empty list (yes)
  ;the method recursively calls itself until tree is not a list, but an element. if the element satisfies the predicate,
  ;it is made into a one element list and returned. if it does not satisy the predicate, an empty list is returned.
  ;these results are then appended to one another.
  ;if the tree is empty, the method returns the list yes.

  ;example:
  ;Given: even? '(1 2 3 4 5 6) '()
  ;appends: (() (2) () (4) () (6) ()) to return (2 4 6)
  ;Given: symbol? '(1 2 a 3 3) '()
  ;appends (() () (a) () () ()) to return (a)
  (cond ((empty? tree) yes)
	((list? tree) (append (cHelpY pred (car tree) yes)(cHelpY pred (cdr tree) yes)))
        ((pred tree)(list tree))
        ((not (pred tree)) '())))

(define (cHelpN pred tree no)
  ;this returns a list of items that don't satify the predicate
  ;this method takes a predicate (pred), the list passed into the orignial method (tree), and an empty list (no)
  ;the method recursively calls itself until tree is not a list, but an element. if the element does not satisfy the predicate,
  ;it is made into a one element list and returned. If 'tree' satisfies the predicate, an empty list is returned.
  ;these results are then appended to one another.
  ;if the tree is empty, the method returns the list no.

  ;example:
  ;Given: even? '(1 2 3 4 5 6) '()
  ;appends: ((1) () (3) () (5) () ()) to return (1 3 5)
  ;Given: symbol? '(1 2 a 3 3) '()
  ;appends ((1) (2) () (3) (3) ()) to return (1 2 3 3)
  
  (cond ((empty? tree) no)
	((list? tree) (append (cHelpN pred (car tree) no)(cHelpN pred (cdr tree) no)))
        ((not(pred tree))(list tree))
        ((pred tree)'())))

; ********************************************************
; ** problem 7 ** (10 points)

(define (tree-min tree)
  (cond ((empty? tree) '())
        ((list? (first tree))(our-min (tree-min(first tree))(tree-min (rest tree))))
        (else (our-min (first tree) (tree-min (rest tree))))))

(define (our-min t1 t2)
  ;this method takes two elements, t1 and t2. They can be number or lists.
  ;if t1 is a list, the method returns t2.
  ;if t2 is a list, the method returns t1.
  ;if neither item is a list (so they are both numbers), the method returns the smaller value.

  ;examples:
  
  ;given: 9 '(10 8)
  ;returns: 9
  
  ;given: 9 8
  ;returns: 8
  
  ;given: 2 8
  ;returns: 2
  
  ;given: '(76 7) 32
  ;returns: 32
  (if (list? t1)
      t2
      (if (list? t2)
          t1
          (if (< t1 t2)
              t1
              t2))))
; ********************************************************
; ** problem 8 ** (10 points)
; Write the procedure

; (count-leaves tre)

; count-leaves takes a nest list tree as an argument and returns
; an integer which is the number of leaves in the tree.

; Examples:

; (count-leaves '(1 2 3)) => 3
; (count-leaves '()) => 0
; (count-leaves '(1 (2 (3 (4))))) => 4
; (count-leaves '(((((((7)))))))) => 1

; ********************************************************
(define (count-leaves lst)
  (cond ((null? lst) 0)
	((number? lst) 1)
	((list? lst)
	 (+ (count-leaves (car lst))
	    (count-leaves (cdr lst))))))

; ********************************************************
; ** problem 9 ** (10 points)
; Write a procedure

; (map-tree proc tree)

; which takes two arguments, a procedure proc and a nested list tree,
; and returns a copy of tree with each leaf node replaced by
; the result of applying proc to that leaf.

; Examples:

; (map-tree even? '(1 2 3 4)) => '(#f #t #f #t)
; (map-tree even? '(1 (2 (3 (4))))) => '(#f (#t (#f (#t))))
; (map-tree (lambda (x) (+ x 1)) '(1 (2 (3 (4 5 6))))) => '(2 (3 (4 (5 6 7))))
; (map-tree odd? '()) => '()

; ********************************************************
(define (map-tree proc lst)
  (cond ((null? lst) '())
	((list? lst)
	 (cons (map-tree proc (car lst))
	    (map-tree proc (cdr lst))))
        (else (proc lst))))


; ********************************************************
; ** problem 10 (10 points)
; Write a procedure

;; (quadratic-roots a b c)

;; which calculates the roots of a quadratic equation

;;  a x^2 + b x + c

;; using the quadratic formula

;; see https://en.wikipedia.org/wiki/Quadratic_formula

;; The answer can be either a single root or two complex roots

;; The discriminant is (b^2 - 4 a c)
;; if the discriminant is 0, there is only one root
;; otherwise there are two.

;; Examples:

;; (quadratic-roots 1 -14 49) => 7
;; (quadratic-roots 1 4 -5) => '(1 -5)
;; (quadratic-roots 1 6 9) => -3
;; (quadratic-roots 1 4 4) => -2 
;; (quadratic-roots 1 0 -4)=>  '(2 -2)
;; (quadratic-roots 1 0 -4)=>  '(0+2i 0-2i)


; ********************************************************
(define (quadratic-roots a b c)
  (if (= a 0)
      "quadratic formula of these numbers cannot be taken. divide by zero error."
      (if (= (- (* b b) (* 4 a c)) 0)
          (/(- (- 0 b)(sqrt (- (* b b) (* 4 a c))))(* 2 a))
          (append (list(/(+ (- 0 b)(sqrt (- (* b b) (* 4 a c))))(* 2 a)))(list(/(- (- 0 b)(sqrt (- (* b b) (* 4 a c))))(* 2 a)))))))

; ********************************************************
; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))
	
(test 'hours hours (lambda (x) (> x 0)))

(test 'depth (depth '((((((0)))))(8 9)))  6)
(test 'depth (depth '((1 2)(2 3)(3 4)))  2)
(test 'depth (depth '((1 2)(2 (3 3 3) 3)(3 () 4)))  3)
(test 'depth (depth '((1 2)(2 3)(3 () 4)))  2)
(test 'depth (depth '())  0)
(test 'depth (depth '(1 2 3)) 1)
(test 'depth (depth '(a (b (c (d))))) 4)
(test 'depth (depth '((((((0))))))) 6)
	
(test 'count-odd (count-odd '(1 2 3)) 2)
(test 'count-odd (count-odd '(1 2 3)) 2)
(test 'count-odd (count-odd '(1 (21 (31)))) 3)
(test 'count-odd (count-odd '(1 2 5)) 2)
(test 'count-odd (count-odd '()) 0)
(test 'count-odd (count-odd '((((((8 8 8))))))) 0)
(test 'count-odd (count-odd '((((((9 9 9))))))) 3)

(test 'count-pred (count-pred odd? '(1 2 3)) 2)
(test 'count-pred (count-pred even? '(1 2 3)) 1)
(test 'count-pred (count-pred integer? '(1 (2 (3)))) 3)
(test 'count-pred (count-pred string? '()) 0)
(test 'count-pred (count-pred even? '((((((8 8 8))))))) 3)
(test 'count-pred (count-pred (lambda (x) (> x 5)) '((((((9 9 9))))))) 3)
(test 'count-pred (count-pred (lambda (x) (> x 5)) '((((11)(((9 9 9))))(3 1 21)))) 5)
(test 'count-pred (count-pred odd? '((((11)(((9 9 9))))(3 1 21)))) 7)

(test 'find-type (find-type 'integer '((1 2 3))) #t)
(test 'find-type (find-type 'flonum '((1 2 3))) #f)
(test 'find-type (find-type 'flonum '((1 2 0.3 3))) #t)
(test 'find-type (find-type 'symbol '((1 2 3))) #f)
(test 'find-type (find-type 'symbol '((a b c))) #t)
(test 'find-type (find-type 'string '((a ("b") c))) #t)
(test 'find-type (find-type 'character '((a ("b") #\c))) #t)
(test 'find-type (find-type 'boolean '((a ("b") #\c)())) #f)
(test 'find-type (find-type 'character '((a ("b") #\c () (#t)))) #t)
(test 'find-type (find-type 'character '((a ("b") "c"))) #f)

(test 'types (types '(1 1 1 1)) '(integer))
(test 'types (types '(1 "a" "a" 1)) '(integer string))
(test 'types (types '(1.0 1 1 1 #t)) '(boolean flonum integer))
(test 'types (types '(1 ((a)))) '(integer symbol))
(test 'types (types '(1 (("a")))) '(integer string))
(test 'types (types '(1 ((#\a)))) '(character integer))
(test 'types (types '(1.0 (("a")))) '(flonum string))
(test 'types (types '(1.0 (("a"))(#t 1))) '(boolean flonum integer string));difference btwn a flonum and a integer

(test 'cull (cull even? '(1 2 3 4 5 6)) '((2 4 6) (1 3 5)))
(test 'cull (cull symbol? '(1 2 a b 3 3 d e)) '((a b d e) (1 2 3 3)))
(test 'cull (cull boolean? '(1 2 a b 3 3 d e)) '(()(1 2 a b 3 3 d e)))
(test 'cull (cull even? '(2 4 6)) '((2 4 6) ()))
(test 'cull (cull boolean? '(1 2 a b 3 3 d e #t #f)) '((#t #f)(1 2 a b 3 3 d e)))
(test 'cull (cull odd? '(2 4 6)) '(() (2 4 6)))
(test 'cull (cull odd? '()) '(() ()))

(test 'tree-min (tree-min '(1 2 3)) 1)
(test 'tree-min (tree-min '(1 (2 (-3)))) -3)
(test 'tree-min (tree-min '()) '())
(test 'tree-min (tree-min '(((((1)(((((7)))))))(-1)))) -1)
(test 'tree-min (tree-min '(((((1)(((((7)))))))()))) 1)
(test 'tree-min (tree-min '(((((((((7)))))))))) 7)
(test 'tree-min (tree-min '((((((((()))))))))) '())

(test 'count-leaves (count-leaves '(1 2 3)) 3)
(test 'count-leaves (count-leaves '()) 0)
(test 'count-leaves (count-leaves '(1 (2 (3 (4))))) 4)
(test 'count-leaves (count-leaves '(((((((7)))))))) 1)
(test 'count-leaves (count-leaves '(((((1)(((((7)))))))(-1)))) 3)
(test 'count-leaves (count-leaves '((((()((((()))))))()))) 0)

(test 'map-tree (map-tree even? '(1 2 3 4)) '(#f #t #f #t))
(test 'map-tree (map-tree even? '(((((1)(((((7)))))))(-1)))) '(((((#f)(((((#f)))))))(#f))))
(test 'map-tree (map-tree (lambda (x) (+ x 1)) '(((((1)(((((7)))()))))(-1)))) '(((((2)(((((8)))()))))(0))))
(test 'map-tree (map-tree even? '(1 (2 (3 (4))))) '(#f (#t (#f (#t)))))
(test 'map-tree (map-tree (lambda (x) (+ x 1)) '(1 (2 (3 (4 5 6))))) '(2 (3 (4 (5 6 7)))))
(test 'map-tree (map-tree odd? '()) '())

(test 'quadratic-roots (quadratic-roots 1 -14 49) 7)
(test 'quadratic-roots (quadratic-roots 1 4 -5) '(1 -5))
(test 'quadratic-roots (quadratic-roots 1 6 9) -3)
(test 'quadratic-roots (quadratic-roots 1 4 4) -2)
;(test 'quadratic-roots (quadratic-roots 0 4 4) "quadratic formula of these numbers cannot be taken. divide by zero error.")
(test 'quadratic-roots (quadratic-roots 1 0 -4) '(2 -2))
(test 'quadratic-roots (quadratic-roots 1 0 4) '(0+2i 0-2i))


; ========================================================
; **** end of hw #2
; ========================================================