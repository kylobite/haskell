; SICP

; Expressions

(+ 1 2)
;; 3

(* 1 2 3 4)
;; 24

(+ (- 4 2) (* 2 5))
;; 12

(+ (- 4 2)
   (* 2 5))
;; 12

; Naming the Environment

(define two 2)
;; 2

(= two 2)
;; #t | #t = true, #f = false

(define pi     3.14159265)
(define radius 5)
(define circum (* 2 pi radius))
;; 31.4159265