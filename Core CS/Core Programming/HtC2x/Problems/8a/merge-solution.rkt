;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname merge-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; merge-starter.rkt

;Problem:
;
;Design the function merge. It consumes two lists of numbers, which it assumes are 
;each sorted in ascending order. It produces a single list of all the numbers, 
;also sorted in ascending order. 
;
;Your solution should explicitly show the cross product of type comments table, 
;filled in with the values in each case. Your final function should have a cond 
;with 3 cases. You can do this simplification using the cross product table by 
;recognizing that there are subtly equal answers. 
;
;Hint: Think carefully about the values of both lists. You might see a way to 
;change a cell content so that 2 cells have the same value.

;; ================
;; Data definitions:

;; ListOfNumber is one of:
;; - empty
;; - (cons Number ListOfNumber)
;; interp. a list of numbers and assumes that they are sorted in ascending order

(define LON1 empty)
(define LON2 (cons 1 empty))
(define LON3 (cons 1 (cons 2 empty)))
(define LON4 (cons 1 (cons 2 (cons 4 empty))))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;-------------------------------------------------------------------------------------------
;|                  lonb >  |  empty                    | (cons Number ListOfNumber)        |        
;|  lona                    |                           |                                   |
;|   V                      |                           |                                   |
;|--------------------------|---------------------------|-----------------------------------|
;|  empty                   |         empty             | empty? lona -> (list lonb)        |
;|                          |                           |  (append  lonb lona)              |
;|--------------------------|---------------------------|-----------------------------------|
;|(cons Number ListOfNumber)|  empty? lonb ->(list lona)| (if (<= (first lona) (first lonb))|
;|                          |   (append lona lonb)      |                                   |
;|-------------------------------------------------------------------------------------------

;; ================
;; Functions:

;; ListOfNumber ListOfNumber -> ListOfNumber
;; consumes two lists of numbers and produce a new list of number with all the numbers, sorted in ascending order
(check-expect (merge-sorted--list empty empty) empty)
(check-expect (merge-sorted--list empty (list 1)) (list 1))
(check-expect (merge-sorted--list (list 3) empty) (list 3))
(check-expect (merge-sorted--list (list 3) (list 1)) (list 1 3))
(check-expect (merge-sorted--list (list 2 3) (list 1)) (list 1 2 3))
(check-expect (merge-sorted--list (list 2 3 4) (list 1 5)) (list 1 2 3 4 5))
(check-expect (merge-sorted--list (list 4 5 6) (list 1 2)) (list 1 2 4 5 6))

;(define (merge-sorted--list lona lonb) empty);stub

(define (merge-sorted--list lona lonb)
  (cond [(and (empty? lona) (empty? lonb)) empty]
        [(or (empty? lona) (empty? lonb)) (append lona lonb)]
        [else
         (if (<= (first lona) (first lonb))
             (cons (first lona) (merge-sorted--list (rest lona) lonb))
             (cons (first lonb) (merge-sorted--list lona (rest lonb))))]))


