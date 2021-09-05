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


(defun parse-record (line)
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


(defun read-password-records (filename)
  (with-open-file (input filename :direction :input)
    (loop for line = (read-line input nil)
	  while line
	  collect (parse-record line))))


(defun output-count (part test-count valid-record)
  (let
      ((test-input (read-password-records "02.test-input.txt"))
       (sample-input (read-password-records "02.input.txt"))
       (part-label (format nil "Day 2, part ~a:" part)))

    (labels
	((count-valid (input)
	   (count t (map 'list valid-record input))))

      (when (/= test-count
		(count-valid test-input))
	(error "~a password count not matched" part-label))

      (format t "~a ~a~%" part-label
	      (count-valid sample-input)))))


(defun valid-password1 (password-record)
  (let* ((lowest (first password-record))
	(highest (second password-record))
	(character (third password-record))
	(password (fourth password-record))
	(char-count (count character password)))
    (and
     (<= lowest char-count)
     (>= highest char-count))))


(output-count 1 2 #'valid-password1)


;; While it appears you validated the passwords correctly, they don't
;; seem to be what the Official Toboggan Corporate Authentication
;; System is expecting.
;;
;; The shopkeeper suddenly realizes that he just accidentally explained
;; the password policy rules from his old job at the sled rental place
;; down the street! The Official Toboggan Corporate Policy actually
;; works a little differently.
;;
;; Each policy actually describes two positions in the password, where
;; 1 means the first character, 2 means the second character, and so
;; on. (Be careful; Toboggan Corporate Policies have no concept of
;; "index zero"!) Exactly one of these positions must contain the given
;; letter. Other occurrences of the letter are irrelevant for the
;; purposes of policy enforcement.
;;
;; Given the same example list from above:
;;
;; 1-3 a: abcde is valid: position 1 contains a and position 3 does
;; not.
;; 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains
;; b.
;; 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain
;; c.
;;
;; How many passwords are valid according to the new interpretation of
;; the policies?


(defun char-at-position (password position)
  (let ((index (- position 1)))
    (char password index)))


(defun valid-password2 (password-record)
  (let* ((position1 (first password-record))
	 (position2 (second password-record))
	 (character (third password-record))
	 (password (fourth password-record)))
    (not (eq
	  (eql character (char-at-position password position1))
	  (eql character (char-at-position password position2))))))


(output-count 2 1 #'valid-password2)
