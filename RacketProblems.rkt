#lang racket
(provide (all-defined-out))

; #1: divisible-by-x?
(define (divisible-by-x? n)
  (lambda (x)
    (= (remainder x n) 0)
    ))


; #2: function-9
(define (function-9 x)
  (x 9))


; #3: my-map
(define (my-map func list)
  (cond [(empty? list) null]
        [else (cons (func (first list)) (my-map func (rest list)))]
        ))


; #4: pair-up
(define (pair-up list1 list2)
  (cond
    [(empty? list1) null]
    [(empty? list2) null]
    [(cons (append (list (first list1)) (list (first list2))) (pair-up (rest list1) (rest list2)))]
    ))


; #5: classify
(define (trueList func list)
  (cond
    [(empty? list) null]
    [(func (first list)) (cons (first list) (trueList func (rest list)))]
    [else (trueList func (rest list))]
    ))

(define (falseList func list)
  (cond
    [(empty? list) null]
    [(not (func (first list))) (cons (first list) (falseList func (rest list)))]
    [else (falseList func (rest list))]
    ))

(define (classify func list)  
  (cond
    [(empty? list) null]
    [else (cons (trueList func list) (cons (falseList func list) null))]
    ))


; #6: is-member?
(define (is-member? val list)
  (cond
    [(empty? list) #f]
    [(equal? val (first list)) #t]
    [else (is-member? val (rest list))]
    ))


; #7: my-sorted?
(define (my-sorted? func list)
  (cond
    [(= (length list) 1) #t]
    [(func (first list) (first (rest list))) (my-sorted? func (rest list))]
    [else #f]
    ))


; #8: my-flatten
(define (my-flatten list)
  (cond
    [(empty? list) null]
    [(list? (first list)) (append (my-flatten (first list)) (my-flatten (rest list)))]
    [else (cons (first list) (my-flatten (rest list)))]
    ))


; #9: upper-threshold
(define (upper-threshold list threshold)
  (cond
    [(empty? list) null]
    [(< (first list) threshold) (cons (first list) (upper-threshold (rest list) threshold))]
    [else (upper-threshold (rest list) threshold)]
    ))


; #10: my-list-ref
(define (my-list-ref list ref)
  (cond
    [(< ref 0) (error "Reference cannot be less than 0")]
    [(>= ref (length list)) (error "Index out of bounds")]
    [(= ref 0) (first list)]
    [else (my-list-ref (rest list) (- ref 1))]
    ))

; OPTIONAL BONUS
; #11: deep-reverse
; NOTE: This function uses the 'my-map'(#3) function implemented above.
(define (my-reverse list)
  (cond
    [(null? list) null]
    [else (append (my-reverse (rest list)) (cons (first list) null))]
    ))
  
(define (deep-reverse list)
  (cond
    [(list? list) (my-reverse (my-map deep-reverse list))]
    [else list]
    ))


; TEST CASES
(writeln 'divisible-by-x?)
((divisible-by-x? 3) 12) 
((divisible-by-x? 7) 20)
(define div-by-5 (divisible-by-x? 5)) 
(div-by-5 15)
(writeln null)

(writeln 'function-9)
(function-9 sqrt)
(function-9 add1) 
(function-9 (lambda (x) (+ x 7)))
(writeln null)

(writeln 'my-map)
(my-map sqrt '(9 25 81 49)) 
(my-map add1 '(6 4 8 3)) 
(my-map (lambda (n) (* n n)) '(5 7)) 
(my-map even? '(2 5 7 12))
(writeln null)

(writeln 'pair-up)
(pair-up '(1 2 3 4) '(a b c d))
(pair-up '(1 2 3) '(4 9 5 7))
(pair-up '(3 5 6) '("one" 6.18 #t "two"))
(writeln null)

(writeln 'classify)
(classify even? '(7 2 3 5 8))
(classify integer? '(3.0 -5.2 8 16 99.7))
(classify real? '())
(writeln null)

(writeln 'is-member?)
(is-member? 6 '(4 8 6 2 1))
(is-member? 7 '(4 8 6 2 1))
(is-member? "foo" '(4 5 #f "foo" a))
(is-member? '(3 4) '(4 5 #f "foo" (3 4)))
(writeln null)

(writeln 'my-sorted?)
(my-sorted? < '(2 5 6 9 11 34))
(my-sorted? < '(7 25 4 15 11 34))
(my-sorted? string<? '("alpha" "beta" "gamma"))
(my-sorted? string<? '("john" "zack" "bob"))
(writeln null)

(writeln 'my-flatten)
(my-flatten '(1))
(my-flatten '((1 2) 3))
(my-flatten '(((4 3) 6)((7 2 9)(5 1))))
(writeln null)

(writeln 'upper-threshold)
(upper-threshold '(3 6.2 7 2 9 5.3 1) 6)
(upper-threshold '(1 2 3 4 5) 4)
(upper-threshold '(4 8 5 6 7) 6.1)
(upper-threshold '(8 3 5 7) 2)
(writeln null)

(writeln 'my-list-ref)
(my-list-ref '(4 7 9) 0)
(my-list-ref '(4 7 9) 1)
;(my-list-ref '(4 7 9) 3)  ;displays "Index out of bounds" error.
(writeln null)

(writeln 'deep-reverse)
(deep-reverse '(((4 3) 6)((7 2 9)(5 1))))
(deep-reverse '((1 2) 3))
(deep-reverse '((4 5)))
(deep-reverse '(3 6 9 12))
(writeln null)
