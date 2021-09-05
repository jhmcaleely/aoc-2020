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
	    (list head match)
	    (find-pair-if tail predicate-p))))))


;; Read each line as an integer
(defun read-numbers-as-list (filename)
  (with-open-file (input filename :direction :input)
    (loop for line = (read-line input nil)
	  while line
	  collect (parse-integer line))))


(defun output-product (part test-product find-sum)
  (let
      ((target-sum 2020)
       (test-input '(1721 979 366 299 675 1456))
       (sample-input (read-numbers-as-list "input.txt"))
       (part-label (format nil "Day 1, part ~a:" part)))

    (labels
	((find-product (input)
	   (reduce #'* (funcall find-sum target-sum input))))
					; Evaluate test case
      (when (/= test-product
		(find-product test-input))
	(error "~a product not matched" part-label))
					; Output answer
      (format t "~a ~a~%" part-label
	      (find-product sample-input)))))


(defun find-pair-sum (n list)
  (find-pair-if list (lambda (a b) (= (+ a b) n))))


(output-product 1 514579 #'find-pair-sum)


;; The Elves in accounting are thankful for your help one of them even
;; offers you a starfish coin they had left over from a past
;; vacation. They offer you a second one if you can find three numbers
;; in your expense report that meet the same criteria.
;;
;; Using the above example again, the three entries that sum to 2020
;; are 979, 366, and 675. Multiplying them together produces the
;; answer, 241861950.
;;
;; In your expense report, what is the product of the three entries
;; that sum to 2020?


(defun find-triple-sum (n list)
  (let* ((one (first list))
	 (remainder (- n one))
	 (tail (rest list)))
    (when (and tail (< 0 remainder))
      (let ((match (find-pair-sum remainder tail)))
	(if match
	    (list one (first match) (second match))
	    (find-triple-sum n tail))))))


(output-product 2 241861950 #'find-triple-sum)
