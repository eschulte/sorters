(load "src/energy.lisp")
(in-package :perf-opt)

;; robust read and process of a dirty data stream too large to fit into memory
(defvar *ind-stats*
  (with-open-file (in "results/energy-1/ind.stats")
    (loop :for line = (ignore-errors (read in nil :eof)) :until (eq line :eof)
       :when line :collect
       (ignore-errors
         (cons (car line)
               (mapcar [{aget _ (cdr line)} #'car] *energy-model*))))))

(with-open-file (out "/tmp/ind.data" :direction :output)
  (with-open-file (in "results/energy-1/ind.stats")
    (loop :for line = (ignore-errors (read in nil :eof)) :until (eq line :eof)
       :do (ignore-errors
             (mapc {format out "~a ~{~a~^ ~}~%" (car line)}
                   (remove-if-not [{> _ 2} #'length]
                                  (mapcar [{aget _ (cdr line)} #'car]
                                          *energy-model*)))))))
