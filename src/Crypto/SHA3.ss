(library (Crypto.SHA3 foreign)
  (export stringToUtf8Bv arrayToByteArray byteArrayToArray
          bytesToHex hexToByteArray eqByteArray byteArrayLength)
  (import (chezscheme)
          (purescm pstring)
          (prefix (srfi :214) fv:)
          (purescm bytevector))

  ;; String -> ByteArray  (PureScript String is a pstring)
  (define stringToUtf8Bv
    (lambda (ps)
      (bv-from-utf8 (pstring->string ps))))

  ;; Array Int -> ByteArray
  (define arrayToByteArray
    (lambda (fv)
      (bv-from-flexvector fv)))

  ;; ByteArray -> Array Int
  (define byteArrayToArray
    (lambda (bv)
      (bv-to-flexvector bv)))

  ;; ByteArray -> String  (returns pstring)
  (define bytesToHex
    (lambda (bv)
      (string->pstring (bv-to-hex bv))))

  ;; String -> ByteArray  (hex decode)
  (define hexToByteArray
    (lambda (ps)
      (bv-from-hex (pstring->string ps))))

  ;; ByteArray -> ByteArray -> Boolean
  (define eqByteArray
    (lambda (a)
      (lambda (b)
        (bytevector=? a b))))

  ;; ByteArray -> Int
  (define byteArrayLength
    (lambda (bv)
      (bytevector-length bv)))

) ;; end library