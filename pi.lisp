
;;
;; Preamble: Lisp prerequisits
;;

;; These two lines sets the number of binary digits used to represent a float
;; in Lisp. This is necessary because you'll be working with tiny numbers
;; TL;DR ignore these two lines
(setf (EXT:LONG-FLOAT-DIGITS) 35000)
(setf *read-default-float-format* 'long-float)

;; This method rounds a number to a certain precision
;; It takes two arguments: the number to round and the number of digits to
;; round in decimals
;;
;; Example: (roundToPrecision 10.0044 3) -> 10.004
(defun roundToPrecision (number precision)
  (let
    ((p (expt 10 precision)))
    (/ (round (* number p)) p)
  )
)

;;
;; Exercise
;;

;; Exercise
;; Your task is to implement the Gauss-Legendre algorithm for calculating pi
;; and extract 10.000 (ten thousand) digits

(defun calNextA(a b)
 ( / (+ a b) 2 )
 )

 (defun calNextB(a b)
( sqrt ( * a b))
 )

(defun calNextP (p)
(* 2 p)
)


;; tn+1 = tn - pn(an - an+1)^2;
(defun calNextT(tn p a b)
( - tn (* (expt (- a (calNextA a b) ) 2) p) )
)


(defvar a0 1L0)
(defvar b0   ( / 1L0 (sqrt 2L0)))
(defvar t0 (/ 1L0 4L0))
(defvar p0 1L0)




(defun calPi (a b p t0 prePi)
(let (
  (aNext (calNextA a b))
  (bNext (calNextB a b))
  (pNext (calNextP p))
  (tNext (calNextT t0 p a b ))
  )
  (let (
    (pi (roundToPrecision (/ (expt (+ aNext bNext)2) (* 4 tNext)) 10000))
    )
;;Body of second let
    (if
       (= prePi pi)
       pi

       (calPi (calNextA a b) (calNextB a b) (calNextP p) (calNextT t0 p a b) pi)
    )
  )
)
)



(write (coerce (calPi a0 b0 p0 t0 0L0) 'long-float))

;;(write (calNextT 1 2 3 1))


;; Gauss-Legendre algorithm on Wikipedia
;; https://en.wikipedia.org/wiki/Gauss%E2%80%93Legendre_algorithm
