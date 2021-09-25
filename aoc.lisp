(load "shared.lisp")

(dotimes (i 8)
  (load (format nil "~2,'0d.lisp" (1+ i))))

(self-test)

(report-solutions)
