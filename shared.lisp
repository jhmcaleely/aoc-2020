;; For advent-of-code 2020

(defun read-line-records (filename)
  "return a file as a list of strings. Each string is one line from the file"
  (with-open-file (input filename :direction :input)
    (loop for line = (read-line input nil)
	  while line
	  collect line)))


(defun read-multiline-string-records (filename)
  (with-open-file (input filename :direction :input)
    (do
     ((line (read-line input nil) (read-line input nil))
      (record nil)
      (records nil))
     ((not line) (push record records))
      (if (equalp line "")
	  (progn
	    (setf records (push record records))
	    (setf record nil))
	  (if record
	      (setf record (concatenate 'string record " " line))
	      (setf record line))))))
