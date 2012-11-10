#lang typed/racket

(: cap-forward (Byte -> Boolean))
(define (cap-forward prev)
  (let: ([stdin (current-input-port)]
         [space : Byte 32]
         [eof   : Byte 10]
         [low-a : Byte 97])
    (let: ([curr : (U Byte EOF) (read-byte stdin)])
      (if (byte? curr)
          (begin (display (if (and (= space prev)
                                   (>= prev low-a))
                              (- curr 32)
                              curr))
                 (cap-forward curr))
          #f))))

(cap-forward (ann 32 Byte))
