(load "shared.lisp")

(dotimes (i 9)
  (load (format nil "~2,'0d.lisp" (1+ i))))

(self-test)

(report-solutions)
