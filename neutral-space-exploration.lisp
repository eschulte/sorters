(require :software-evolution)
(in-package :software-evolution)

;; compile insertion.c to assembly
(shell "gcc -S insertion.c")

;; general configuration settings
(setq *pos-test-num* 10)
(setq *neg-test-num* 0)
(setq *test-script* "./test.sh")
(setq *genome-averaging-keys* '(:pos))
(setq original (asm-from-file "insertion.s"))

(unless (= 10 (fitness original))
  (error "Failed sanity check: ~s!=10" (fitness original)))

;; apply good path samples
(apply-path original :pos (samples-from-tracer-file "trace"))


;;; experimentation
(defmacro repeatedly (n &body body)
  `(loop :for _ :upto ,n :collect ,@body))

(defun ant-stats (ant)
  "Return statistics about point ANT in the genotype space."
  (let ((fitness (fitness ant)) neighbors)
    (dotimes (_ 10)
      (let ((neighbor (copy ant)))
        (mutate neighbor)
        (push `((:fitness . ,(fitness neighbor))
                (:history . ,(history neighbor))) neighbors)))
    `((:size      . ,(length (genome ant)))
      (:fitness   . ,fitness)
      (:history   . ,(history ant))
      (:neighbors . ,neighbors))))

(defun do-random-walk (dir &key (walks 1000) (steps 100))
  "Run a series of random walks saving results to DIR."
  (dotimes (n walks)
    (store (let ((ant (asm-from-file "insertion.s")))
             (repeatedly steps (prog1 (ant-stats ant) (mutate ant))))
     (merge-pathnames (format nil "rand-walk-~S.store" n) dir))))

(defun do-neutral-walk (dir &key (popsize 100) (steps 1000))
  "Run a series of neutral walk saving results to DIR."
  (let ((pop (repeatedly popsize (let ((ant (asm-from-file "insertion.s")))
                                   (mutate ant) ant))))
    (dotimes (n steps)
      (store (mapcar #'ant-stats pop)
             (merge-pathnames (format nil "neut-pop-~S.store" n) dir))
      (setf pop (mapcar (lambda (ant)
                          (mutate ant)
                          (if (= 10 (fitness ant)) ant (random-elt pop)))
                        pop)))))


;; analysis
(defun aget (key lst) (cdr (assoc key lst)))
(defun getter (key) (lambda (it) (aget key it)))
(defun transpose (matrix) (apply #'map 'list #'list matrix))

(defun step-neighbor-stats (step)
  "Given a step, return (mut-rb ave-neighbor-fitness)."
  (let ((neutral 0) (average 0))
    (mapc (lambda (fitness)
            (when (= 10 fitness) (incf neutral))
            (incf average fitness))
          (mapcar (lambda (n) (cdr (assoc :fitness n))) step))
    (list neutral (/ average 10))))

(defun walk-stats (walk)
  "Return per-step alists of size, fitness, `step-neighbor-stats'. "
  (mapcar (lambda (step)
            (setf (cdr (assoc :neighbors step))
                  (step-neighbor-stats (cdr (assoc :neighbors step))))
            (delete-if (lambda (it) (equalp it :history)) step :key #'car))
          (reverse (copy-tree walk))))

(defun by-step (walks)
  "Given a list of walks, return stats organized by step."
  (flet ((mean-and-stdev (lst) (cons (mean lst) (variance lst))))
    (mapcar
     (lambda (steps)
       `((:fitness   . ,(mean-and-stdev (mapcar (getter :fitness)   steps)))
         (:size      . ,(mean-and-stdev (mapcar (getter :size)      steps)))
         (:mut-rb . ,(mean-and-stdev
                      (mapcar #'first (mapcar (getter :neighbors) steps))))))
     (transpose walks))))

(defun to-file (steps file)
  "Dump steps to file as tab separated text."
  (with-open-file (out file :direction :output)
    (loop :for step :in *stats* :do
       (format out "~&~a ~a ~a ~a ~a ~a"
               (car (aget :fitness step))
               (cdr (aget :fitness step))
               (car (aget :size step))
               (cdr (aget :size step))
               (car (aget :mut-rb step))
               (cdr (aget :mut-rb step))))))

#+run-analysis
(progn
  (defvar *walks*
    (loop :for i :from 0 :upto 999 :collect
       (walk-stats (restore (format nil "results/rand-walks/walk_~a.store" i))))
    "The raw walk data read directly from what is stored to disk.")

  (defvar *stats*
    (by-step *walks*)
    "Statistics describing the walk data organized by step.")

  (to-file *stats* "results/rand-walks/stats.txt"))
