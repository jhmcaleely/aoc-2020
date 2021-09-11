(load "shared.lisp")

(dotimes (i 6)
  (load (format nil "~2,'0d.lisp" (1+ i))))
