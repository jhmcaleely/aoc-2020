;; For advent-of-code 2020


(defun read-parsed-line-records (filename parser)
  "return a file as a list of records. Each record is the result of calling parser on a line"
  (with-open-file (input filename :direction :input)
    (loop for line = (read-line input nil)
	  while line
	  collect (funcall parser line))))


(defun read-line-records (filename)
  "return a file as a list of strings. Each string is one line from the file"
  (read-parsed-line-records filename #'identity))


(defun read-multiline-string-records (filename)
  "read records that are separated by blank lines. result is a list of strings with spaces separating what was on each line"
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


(defun split-sequence (item sequence)
  "spit a sequence into a list of subsequences that were separated by item"
  (do
   ((next-item (position item sequence)
	       (position item sequence :start (1+ next-item)))
    (last-item 0 (1+ next-item))
    (subseqs nil))

   ((not next-item)

    (nreverse
     (push
      (subseq sequence last-item next-item)
      subseqs)))

    (setf
     subseqs (push
	      (subseq sequence last-item next-item)
	      subseqs))))
