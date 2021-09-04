;; https://adventofcode.com/2020/day/1
;;
;; Before you leave, the Elves in accounting just need you to fix your
;; expense report (your puzzle input); apparently, something isn't
;; quite adding up.
;;
;; Specifically, they need you to find the two entries that sum to
;; 2020 and then multiply those two numbers together.
;;
;; For example, suppose your expense report contained the following:
;;
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
;;
;; Of course, your expense report is much larger. Find the two entries
;; that sum to 2020; what do you get if you multiply them together?


;; Recursively search pairs in a list that match the supplied predicate
(defun find-pair-if (list predicate-p)
  (let ((head (first list))
	(tail (rest list)))
    (when tail
      (let ((match (find-if (lambda (a) (funcall predicate-p head a)) tail)))
	(if match
	    (cons head match)
	    (find-pair-if tail predicate-p))))))


(defun compute-answer (input)
  (let ((match (find-pair-if input
			     (lambda (a b)
			       (= (+ a b) 2020)))))
    (* (car match) (cdr match))))


;; Read each line as an integer
(defun read-numbers-as-list (filename)
  (with-open-file (input filename :direction :input)
    (loop for line = (read-line input nil)
	  while line
	  collect (parse-integer line))))


;; First evaluate the example
(when (/= 514579
	  (compute-answer '(1721 979 366 299 675 1456)))
  (error "Test case failed"))


;; 787776
(print (compute-answer (read-numbers-as-list "input.txt")))
