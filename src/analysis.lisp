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

;;; Summary of a *massive* run on real
(require :cl-fad)

(defvar infinity
  #+sbcl
  SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #-(or sbcl)
  (error "must specify a positive infinity value"))

#+real
(advise-thread-pool-size 48)

(defvar *files* (cl-fad:list-directory "results/merge-pops-2013-01-28"))

(defvar *summary* nil)

#+first-time
(setf *summary*
      (pmapcar (lambda (path)
                 (let* ((pop (restore path))
                        (neutral (remove infinity pop :key #'fitness))
                        (sorted (sort neutral #'< :key #'fitness)))
                   `((:evals      . ,(parse-integer (pathname-name path)))
                     (:num-neut   . ,(length neutral))
                     (:mean-fit   . ,(mean (mapcar #'fitness neutral)))
                     (:best-fit   . ,(fitness (car sorted)))
                     (:best-edits . ,(edits (car sorted))))))
               *files*))

(prog1 :done (setf *summary* (restore "results/merge-pops-2013-01-28.store")))

(length *summary*)

(mapcar #'car (car *summary*))
;; (:EVALS :NUM-NEUT :MEAN-FIT :BEST-FIT :BEST-EDITS)

(with-open-file (out "/tmp/merge-pops-2013-01-28.summary" :direction :output)
  (mapc
   (lambda (it)
     (format out "~{~a~^ ~}~%"
             (mapcar {aget _ it}
                     '(:EVALS :NUM-NEUT :MEAN-FIT :BEST-FIT))))
   *summary*)
  :done)
