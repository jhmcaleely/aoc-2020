;; https://adventofcode.com/2020/day/1

;; Before you leave, the Elves in accounting just need you to fix your
;; expense report (your puzzle input); apparently, something isn't
;; quite adding up.

;; Specifically, they need you to find the two entries that sum to
;; 2020 and then multiply those two numbers together.

;; For example, suppose your expense report contained the following:

;; 1721
;; 979
;; 366
;; 299
;; 675
;; 1456
;;
;; In this list, the two entries that sum to 2020 are 1721 and
;; 299. Multiplying them together produces 1721 * 299 = 514579, so the
;; correct answer is 514579.

;; Of course, your expense report is much larger. Find the two entries
;; that sum to 2020; what do you get if you multiply them together?

(defparameter *test* '(1721 979 366 299 675 1456))
(defparameter *target-sum* 2020)
(defparameter *failing-test* '(1721 979 366 298 675 1456))
(defparameter *i* nil)

(defun target-sum-p (a b)
  (= (+ a b) *target-sum*))

(defun compute-product (a b)
  (* a b))

(defun find-pair (x)
  (let ((head (first x))
	(tail (rest x)))
    (when tail
      (let ((match (find-if (lambda (a) (target-sum-p head a)) tail)))
	(if match
	    (cons head match)
	    (find-pair tail))))))

(defun compute-answer (l)
  (let ((result (find-pair l)))
    (* (car result) (cdr result))))


(defun read-input ()
  (with-open-file (input "input.txt" :direction :input)
    (do ((l (read-line input) (read-line input nil 'eof)))
	((eql l 'eof) nil)
      (setq *i* (cons (parse-integer l) *i*)))))

(read-input)

*i*


; 787776
(print (compute-answer *i*))
