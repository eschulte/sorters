(in-package :software-evolution)
(use-package :cl-ppcre)
(use-package :curry-compose-reader-macros)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defun to-stats (list)
  (mapcar (lambda (it)
            `((:fitness . ,(fitness it))
              (:edits   . ,(length (edits it)))))
          list))

(defun stats-at-fit (fit-num)
  (to-stats (restore (format nil "pops/~d.store" fit-num))))

(defvar *stats* (with-open-file (in "pop-fitness")
                  (loop :for line = (read-line in nil nil) :while line
                     :collect (stats-at-fit (parse-integer line)))))
