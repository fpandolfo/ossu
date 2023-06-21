;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname fold-dir-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; fold-dir-starter.rkt

;; =================
;; Data definitions:

(define-struct dir (name sub-dirs images))
;; Dir is (make-dir String ListOfDir ListOfImage)
;; interp. An directory in the organizer, with a name, a list
;;         of sub-dirs and a list of images.
#;
(define (fn-for-dir dir)
  (... (dir-name dir)
       (fn-for-lod (dir-sub-dirs dir))
       (fn-for-loi (dir-images dir))))

;; ListOfDir is one of:
;;  - empty
;;  - (cons Dir ListOfDir)
;; interp. A list of directories, this represents the sub-directories of
;;         a directory.
#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-dir (first lod))
              (fn-for-lod (rest lod)))]))

;; ListOfImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; interp. a list of images, this represents the sub-images of a directory.
;; NOTE: Image is a primitive type, but ListOfImage is not.
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (rest loi))]))

(define I1 (square 10 "solid" "red"))
(define I2 (square 12 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) empty))

;; =================
;; Functions:


;PROBLEM A:
;
;Design an abstract fold function for Dir called fold-dir. 


;; (String Y Z -> X) (X Y -> Y) (Z Image -> Z) Y Z Dir -> X
;; abstract fold function for Dir
(define (fold-dir fn1 fn2 fn3 b1 b2 dir)
  (local [(define (fn-for-dir dir)                 ; Dir -> X
            (fn1 (dir-name dir)                    
                 (fn-for-lod (dir-sub-dirs dir))   
                 (fn-for-loi (dir-images dir))))   

          (define (fn-for-lod lod)                 ; (listof Dir) -> Y
            (cond [(empty? lod) b1]                
                  [else
                   (fn2 (fn-for-dir (first lod))   ; Dir -> X
                        (fn-for-lod (rest lod)))]))

          (define (fn-for-loi loi)                 ; (listof Image) -> Z
            (cond [(empty? loi) b2]                
                  [else
                   (fn3 (first loi)                
                        (fn-for-loi (rest loi)))]))]            
    (fn-for-dir dir)))


;PROBLEM B:
;
;Design a function that consumes a Dir and produces the number of 
;images in the directory and its sub-directories. 
;Use the fold-dir abstract function.

;; Dir -> Natural
;; count the total number of images in Dir and its subdirs
(check-expect (count-images D6) (+ 0 1 1 1))

(define (count-images dir)
  (local [(define (fn1 name rlod rloi) (+ rlod rloi))
          (define (fn2 rdir rlod) (+ rdir rlod))
          (define (fn3 img rloi) (+ 1 rloi))]
    (fold-dir fn1 fn2 fn3 0 0 dir)))


;PROBLEM C:
;
;Design a function that consumes a Dir and a String. The function looks in
;dir and all its sub-directories for a directory with the given name. If it
;finds such a directory it should produce true, if not it should produce false. 
;Use the fold-dir abstract function.

;; String Dir -> Boolean
;; search for a dir with name
;; if found produce true
;; if not found produce false
(check-expect (find-dir D6 "D1") false)
(check-expect (find-dir D6 "D8") false)
(check-expect (find-dir D6 "D6") true)
(check-expect (find-dir D6 "D4") true)

(define (find-dir dir s)
  (local [(define (fn1 name rdirs rimgs) (or (string=? name s)
                                             rdirs
                                             rimgs))
          (define (fn2 rdir rdirs) (or rdir rdirs))
          (define (fn3 img rloi) false)]
    (fold-dir fn1 fn2 fn3 false false dir)))





