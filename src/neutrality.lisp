;;; neutral.lisp --- test sorters for neutrality across representations
(require :software-evolution)
(require :curry-compose-reader-macros)
(in-package :software-evolution)
(use-package :curry-compose-reader-macros)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar *fitness-predicate* #'<
  "Descending order because we want to minimize run time.")

(defvar *test* "../../bin/test-file.sh"
  "The standard sorter test script.")

(defvar *orig* (from-file (make-instance 'cil) "sorters/merge_file_c.c")
  "The original program.")

(defvar *work-dir* "sh-runner/work/"
  "Needed because SBCL chokes after too many shell outs.")

(defmethod neutralp ((variant cil))
  (with-temp-file (file)
    (ignore-errors
      (phenome variant :bin file)
      (format t "~a ~a" *test* file)
      (multiple-value-bind (stdout stderr exit) (shell "~a ~a" *test* file)
        (declare (ignorable stderr stdout))
        (zerop exit)))))

(defun test (variant)
  (incf *fitness-evals*) (neutralp variant))
(un-memoize 'test)
(memoize #'test :key [#'genome #'first])

(defun save-pop (ind-format)
  (loop :for individual :in *population* :as i :upfrom 0 :do
     (string-to-file (genome individual) (format nil ind-format i))))

(defun neutral-walk (neutralp &key (pop-size 100) (max-steps 256) &aux next)
  "Generate successive neutral generations of *POPULATION*.
The function NEUTRALP should take a variant and test its neutrality."
  (loop :for step :below max-steps :do
     (loop :for new = (let ((it (copy (random-elt *population*))))
                        (mutate it) it) :until (= (length next) pop-size) :do
        (format t "~S -> ~S~%"
                (edits new) (when (funcall neutralp new)
                              (setf (fitness new) 1) (push new next) 1)))
     (setf *population* next) (setf next nil)
     (save-pop (format nil "steps/~d/ind-~~d.c" step))))
