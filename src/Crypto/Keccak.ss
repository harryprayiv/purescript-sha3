;;; Crypto.Keccak — Chez Scheme FFI
;;;
;;; Round constants for Keccak-f[1600] as native 64-bit integers.
;;; On the JS backend these had to be split into { hi, lo } records.
;;; Here they're just integer literals. Beautiful.

(library (Crypto.Keccak foreign)
  (export roundConstants orInt)
  (import (chezscheme))

  ;; The 24 round constants (RC) from FIPS 202 §3.2.5.
  ;; Each is a 64-bit value expressed directly.
  (define roundConstants
    (list->vector
      (list
        #x0000000000000001
        #x0000000000008082
        #x800000000000808A
        #x8000000080008000
        #x000000000000808B
        #x0000000080000001
        #x8000000080008081
        #x8000000000008009
        #x000000000000008A
        #x0000000000000088
        #x0000000080008009
        #x000000008000000A
        #x000000008000808B
        #x800000000000008B
        #x8000000000008089
        #x8000000000008003
        #x8000000000008002
        #x8000000000000080
        #x000000000000800A
        #x800000008000000A
        #x8000000080008081
        #x8000000000008080
        #x0000000080000001
        #x8000000080008008)))

  ;; Bitwise OR for plain Ints (used in padding, not lane operations).
  (define orInt
    (lambda (a)
      (lambda (b)
        (logior a b))))

) ;; end library