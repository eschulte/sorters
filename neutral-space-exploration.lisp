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
