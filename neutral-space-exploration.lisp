(require :software-evolution)
(in-package :software-evolution)

;; compile insertion.c to assembly
(shell "gcc -S insertion.c")

;; general configuration settings
(setq *pos-test-num* 10)
(setq *neg-test-num* 0)
(setq *test-script* "test.sh")
(setq *genome-averaging-keys* '(:pos))
(setq original (asm-from-file "insertion.s"))

;; apply good path samples
(apply-path original :pos (samples-from-tracer-file "trace"))

;; walk away from the original program in genome space
(defvar *walks* nil)

(setf *walks* nil)

(progn
  (dotimes (n 1000)
    (push (let ((ant (asm-from-file "insertion.s")) walk)
            (dotimes (_ 100)
              (let ((fitness (fitness ant)) neighbors)
                (dotimes (_ 10)
                  (let ((neighbor (copy ant)))
                    (mutate neighbor)
                    (push `((:fitness . ,(fitness neighbor))
                            (:history . ,(history neighbor))) neighbors)))
                (push `((:size      . ,(length (genome ant)))
                        (:fitness   . ,fitness)
                        (:history   . ,(history ant))
                        (:neighbors . ,neighbors)) walk)
                (mutate ant)))
            walk) *walks*)
    (store (car *walks*) (format nil "walk_~S.store" n)))
  (store *walks* #P"walks.store"))
