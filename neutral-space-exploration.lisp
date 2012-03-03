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

(defmethod size ((ant software))
  (length (genome ant)))

(defun ant-stats (ant)
  "Return statistics about point ANT in the genotype space."
  (let ((fitness (fitness ant)) neighbors)
    (dotimes (_ 10)
      (let ((neighbor (copy ant)))
        (mutate neighbor)
        (push `((:fitness . ,(fitness neighbor))
                (:history . ,(history neighbor))) neighbors)))
    `((:size      . ,(size ant))
      (:fitness   . ,fitness)
      (:history   . ,(history ant))
      (:neighbors . ,neighbors))))

(defvar *pop* nil
  "Population variable, made global to allow peeking in on runs.")

(defun do-random-walk (dir &key (walks 1000) (steps 100))
  "Run a series of random walks saving results to DIR."
  (dotimes (n walks)
    (store (let ((ant (asm-from-file "insertion.s")))
             (repeatedly steps (prog1 (ant-stats ant) (mutate ant))))
     (merge-pathnames (format nil "rand-walk-~S.store" n) dir))))

(defun do-neutral-walk (dir &key (popsize 100) (steps 1000))
  "Expand a population in the neutral space saving results to DIR."
  (setf *pop* (repeatedly popsize (let ((ant (asm-from-file "insertion.s")))
                                    (mutate ant) ant)))
  (dotimes (n steps)
    (store (mapcar #'ant-stats *pop*)
           (merge-pathnames (format nil "neut-pop-~S.store" n) dir))
    (setf *pop*
          (mapcar (lambda (ant)
                    (mutate ant)
                    (if (= 10 (fitness ant)) ant (copy (random-elt *pop*))))
                  *pop*))))

(defun do-biased-walk (dir &key
                             (popsize 100) (steps 1000)
                             (test #'<) (key #'size) (tournysize 2))
  "Evolve a population in the neutral space biased by TEST and KEY."
  (setf *pop* (repeatedly popsize (let ((ant (asm-from-file "insertion.s")))
                                    (mutate ant) ant)))
  (dotimes (n steps)
    (store (mapcar #'ant-stats *pop*)
           (merge-pathnames (format nil "neut-pop-~S.store" n) dir))
    (setf *pop*
          (repeatedly popsize
                      (first (sort (repeatedly tournysize (random-elt *pop*))
                                   :test test :key key)))
          (mapcar (lambda (ant)
                    (mutate ant)
                    (if (= 10 (fitness ant)) ant (copy (random-elt *pop*))))
                  *pop*))))

#+run-neutral-walk
(do-neutral-walk "results/neut-walk/")


;; analysis
(defun aget (key lst) (cdr (assoc key lst)))
(defun getter (key) (lambda (it) (aget key it)))
(defun transpose (matrix) (apply #'map 'list #'list matrix))

(defun step-mut-rb (step)
  "Return the mutational robustness of a step from its neighbors."
  (/ (count 10 (mapcar (getter :fitness) (aget :neighbors step))) 10))

(defun walk-stats (walk)
  "Return per-step alists of size, fitness, `step-neighbor-stats'. "
  (mapcar (lambda (step)
            `((:mut-rb   . ,(step-mut-rb step))
              (:genotype . ,(sxhash (aget :history step)))
              (:fitness  . ,(aget :fitness step))
              (:size     . ,(aget :size step))))
          (reverse (copy-tree walk))))

(defun by-step (walks)
  "Given a list of walks, return stats organized by step."
  (flet ((mean-and-stdev (lst) (cons (mean lst) (variance lst))))
    (mapcar
     (lambda (steps)
       `((:neutral   . ,(/ (count 10 (mapcar (getter :fitness) steps)) 1000))
         (:fitness   . ,(mean-and-stdev (mapcar (getter :fitness) steps)))
         (:size      . ,(mean-and-stdev (mapcar (getter :size)    steps)))
         (:mut-rb    . ,(mean-and-stdev (mapcar (getter :mut-rb)  steps)))))
     (transpose walks))))

(defun to-file (steps file)
  "Dump steps to file as tab separated text."
  (with-open-file (out file :direction :output :if-exists :supersede)
    (loop :for step :in *stats* :do
       (format out "~&~f ~f ~f ~f ~f ~f ~f"
               (aget :neutral step)
               (car (aget :fitness step))
               (cdr (aget :fitness step))
               (car (aget :size step))
               (cdr (aget :size step))
               (car (aget :mut-rb step))
               (cdr (aget :mut-rb step))))))

(defvar *walks* nil
  "The raw walk data read directly from what is stored to disk.")

(defvar *stats* nil
  "Statistics describing the walk data organized by step.")

#+nil
(progn
  (setf *walks*
        (loop :for i :from 0 :upto 999 :collect
           (walk-stats
            (restore
             (format nil "results/rand-walks/rand-walk-~a.store" i)))))

  (setf *stats* (by-step *walks*))

  (to-file *stats* "results/rand-walks/stats.txt"))
