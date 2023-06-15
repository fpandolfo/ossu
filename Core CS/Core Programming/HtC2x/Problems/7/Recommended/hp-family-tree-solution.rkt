;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname hp-family-tree-solution) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

;; hp-family-tree-starter.rkt

;In this problem set you will represent information about descendant family 
;trees from Harry Potter and design functions that operate on those trees.
;
;To make your task much easier we suggest two things:
;  - you only need a DESCENDANT family tree
;  - read through this entire problem set carefully to see what information 
;    the functions below are going to need. Design your data definitions to
;    only represent that information.
;  - you can find all the information you need by looking at the individual 
;    character pages like the one we point you to for Arthur Weasley.


;PROBLEM 1:
;
;Design a data definition that represents a family tree from the Harry Potter 
;wiki, which contains all necessary information for the other problems.  You 
;will use this data definition throughout the rest of the homework.


;PROBLEM 2: 
;
;Define a constant named ARTHUR that represents the descendant family tree for 
;Arthur Weasley. You can find all the infomation you need by starting 
;at: http://harrypotter.wikia.com/wiki/Arthur_Weasley.
;
;You must include all of Arthur's children and these grandchildren: Lily, 
;Victoire, Albus, James.
;
;
;Note that on the Potter wiki you will find a lot of information. But for some 
;people some of the information may be missing. Enter that information with a 
;special value of "" (the empty string) meaning it is not present. Don't forget
;this special value when writing your interp.


;; Data definitions:

(define-struct wiz (name wand patronus kids))
;; Wizard is (make-wiz String String String ListOfWizard)
;; interp. a wizard in a descendant family tree:
;;                       name is the first name
;;                       wand is the wood their primary wand is made of ("" if unknown)
;;                       patronus is a string ("" if unknown)
;;                       kids is their immediate children

;; ListOfWizard is one of:
;; empty
;; (cons Wizard ListOfWizard)
;; interp. a wizard descendant family tree

(define ARTHUR
  (make-wiz "ARTHUR" "" "WEASEL" (list (make-wiz "WILLIAM" "" ""
                                                 (list (make-wiz "VICTOIRE" "" "" empty)))
                                       (make-wiz "CHARLES" "ASH" "" empty)
                                       (make-wiz "PERCY" "" "" empty)
                                       (make-wiz "FRED" "" "MAGPIE" empty)
                                       (make-wiz "GEORGE" "" "MAGPIE" empty)
                                       (make-wiz "RONALD" "ASH" "JACK RUSSELL TERRIER" empty)
                                       (make-wiz "GINEVRA" "YEW" "HORSE"
                                                 (list (make-wiz "JAMES" "" "" empty)
                                                       (make-wiz "ALBUS" "CHERRY" "" empty)
                                                       (make-wiz "LILY" "" "" empty))))))
#;
(define (fn-for--wiz wiz)
  (... (wiz-name wiz)
       (wiz-wand wiz)
       (wiz-patronus wiz)
       (fn-for--low (wiz-kids wiz))))
#;
(define (fn-for--low low)
  (cond [(empty? low) (...)]
        [else
         (... (fn-for--wiz (first low))
              (fn-for--low (rest low)))]))

;PROBLEM 3:
;
;Design a function that produces a pair list (i.e. list of two-element lists)
;of every person in the tree and his or her patronus. For example, assuming 
;that HARRY is a tree representing Harry Potter and that he has no children
;(even though we know he does) the result would be: (list (list "Harry" "Stag")).
;
;You must use ARTHUR as one of your examples.


;; Wizard -> ListOfPair
;; ListOfWizard -> ListOfPair
;; produce a new list with every wizard and their patron
(check-expect (produce-low empty) empty)
(check-expect (produce-pair (make-wiz "RONALD" "" "JACK RUSSEL TERRIER" empty)) (list (list "RONALD" "JACK RUSSEL TERRIER")))
(check-expect (produce-pair (make-wiz "RONALD" "" "JACK RUSSEL TERRIER"
                                      (list (make-wiz "GINEVRA" "YEW" "HORSE" empty)))) (list (list "RONALD" "JACK RUSSEL TERRIER") (list "GINEVRA" "HORSE")))

(check-expect (produce-pair ARTHUR)
              (list (list "ARTHUR" "WEASEL")
                    (list "WILLIAM" "")
                    (list "VICTOIRE" "")
                    (list "CHARLES" "")
                    (list "PERCY" "")
                    (list "FRED" "MAGPIE")
                    (list "GEORGE" "MAGPIE")
                    (list "RONALD" "JACK RUSSELL TERRIER")
                    (list "GINEVRA" "HORSE")
                    (list "JAMES" "")
                    (list "ALBUS" "")
                    (list "LILY" "")))

;(define (produce-pair w low) (list (list "RONALD" "JACK RUSSEL TERRIER")));stub

(define (produce-pair wiz)
  (cons (list (wiz-name wiz)
              (wiz-patronus wiz))
        (produce-low (wiz-kids wiz))))

(define (produce-low low)
  (cond [(empty? low) empty]
        [else
         (append (produce-pair (first low))
                 (produce-low (rest low)))]))

;PROBLEM 4:
;
;Design a function that produces the names of every person in a given tree 
;whose wands are made of a given material. 
;
;You must use ARTHUR as one of your examples.

;; String Wizard -> ListOfString
;; ListOfWizard String -> ListOfString
;; returns every person in a tree whose wands are made of the given material
(check-expect (wands--wizard "TESTE" (make-wiz "Fernando" "steel" "" empty)) empty)
(check-expect (wands--low "TESTE" empty) empty)
(check-expect (wands--wizard "YEW" (make-wiz "GINEVRA" "YEW" "HORSE" empty)) (list "GINEVRA"))
(check-expect (wands--wizard "YEW" (make-wiz "GINEVRA" "YEW" "HORSE" (list (make-wiz "JAMES" "YEW" "" empty)))) (list "GINEVRA" "JAMES"))
(check-expect (wands--wizard "ASH" ARTHUR) (list "CHARLES" "RONALD"))

;(define (wands--wizard w wiz) empty)
;(define (wands--low w low) empty)

(define (wands--wizard w wiz)
  (if (string=? w (wiz-wand wiz))
      (cons (wiz-name wiz)
            (wands--low w (wiz-kids wiz)))
      (wands--low w (wiz-kids wiz))))

(define (wands--low w low)
  (cond [(empty? low) empty]
        [else
         (append (wands--wizard w (first low))
                 (wands--low w (rest low)))]))