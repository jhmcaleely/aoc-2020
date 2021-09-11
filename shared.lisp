;; For advent-of-code 2020

(defun read-line-records (filename)
  "return a file as a list of strings. Each string is one line from the file"
  (with-open-file (input filename :direction :input)
    (loop for line = (read-line input nil)
	  while line
	  collect line)))
