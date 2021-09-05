;; Advent of Code Day 2, Part 1
;;
;; To try to debug the problem, they have created a list (your puzzle
;; input) of passwords (according to the corrupted database) and the
;; corporate policy when that password was set.
;;
;; For example, suppose you have the following list:
;;
;; 1-3 a: abcde
;; 1-3 b: cdefg
;; 2-9 c: ccccccccc
;;
;; Each line gives the password policy and then the password. The
;; password policy indicates the lowest and highest number of times a
;; given letter must appear for the password to be valid. For example,
;; 1-3 a means that the password must contain a at least 1 time and at
;; most 3 times.
;;
;; In the above example, 2 passwords are valid. The middle password,
;; cdefg, is not; it contains no instances of b, but needs at least
;; 1. The first and third passwords are valid: they contain one a or
;; nine c, both within the limits of their respective policies.
;;
;; How many passwords are valid according to their policies?

(defun parse-line (line)
  (let
      ((min (parse-integer line
			   :end (position #\- line)))
       (max (parse-integer line
			   :start (+ 1 (position #\- line))
			   :end (position #\Space line)))
       (rule-char (char line
			(+ 1 (position #\Space line))))
       (password (subseq line (+ 2 (position #\: line)))))
    (format nil "~a ~a: ~a ~a" min max rule-char password)
    (list min max rule-char password)))

(defun valid-password (password-record)
  (let* ((lowest (first password-record))
	(highest (second password-record))
	(character (third password-record))
	(password (fourth password-record))
	(char-count (count character password)))
    (and
     (<= lowest char-count)
     (>= highest char-count))))

(defun read-password-records (filename)
  (with-open-file (input filename :direction :input)
    (loop for line = (read-line input nil)
	  while line
	  collect (parse-line line))))

(= 2 (count t (map 'list
		   #'valid-password (read-password-records "02.test-input.txt"))))

(format t "Day 2, part 1: ~a~%"
	(count t (map 'list #'valid-password (read-password-records "02.input.txt"))))
