;;; Crypto.SHA3 — Chez Scheme FFI
;;;
;;; String-to-UTF8, hex encoding/decoding.
;;; No more Node.js Buffer — we're running on bare metal now.

(library (Crypto.SHA3 foreign)
  (export stringToUtf8 bytesToHex hexToBytes_)
  (import (chezscheme))

  ;; Convert a PureScript String to an array of UTF-8 bytes.
  ;; Chez Scheme strings are Unicode; we encode to UTF-8 via bytevector.
  (define stringToUtf8
    (lambda (str)
      (let* ([bv (string->utf8 str)]
             [len (bytevector-length bv)])
        (let loop ([i 0] [acc '()])
          (if (= i len)
              (list->vector (reverse acc))
              (loop (+ i 1)
                    (cons (bytevector-u8-ref bv i) acc)))))))

  ;; Encode an array of byte values as a lowercase hex string.
  (define bytesToHex
    (lambda (arr)
      (let* ([len (vector-length arr)]
             [hex-chars "0123456789abcdef"])
        (let loop ([i 0] [acc '()])
          (if (= i len)
              (apply string-append (reverse acc))
              (let ([b (vector-ref arr i)])
                (loop (+ i 1)
                      (cons (string
                              (string-ref hex-chars (ash b -4))
                              (string-ref hex-chars (logand b #xF)))
                            acc))))))))

  ;; Decode a hex string to an array of bytes, returning (Just arr) or Nothing.
  ;; Takes the Just and Nothing constructors as arguments (purescm pattern).
  (define hexToBytes_
    (lambda (just)
      (lambda (nothing)
        (lambda (str)
          (let ([len (string-length str)])
            (if (or (odd? len)
                    (not (hex-string? str)))
                nothing
                ((just)
                 (let loop ([i 0] [acc '()])
                   (if (= i len)
                       (list->vector (reverse acc))
                       (loop (+ i 2)
                             (cons (+ (* (hex-digit (string-ref str i)) 16)
                                      (hex-digit (string-ref str (+ i 1))))
                                   acc)))))))))))

  ;; Helper: is every char in the string a hex digit?
  (define hex-string?
    (lambda (str)
      (let ([len (string-length str)])
        (let loop ([i 0])
          (if (= i len)
              #t
              (let ([c (string-ref str i)])
                (if (or (and (char>=? c #\0) (char<=? c #\9))
                        (and (char>=? c #\a) (char<=? c #\f))
                        (and (char>=? c #\A) (char<=? c #\F)))
                    (loop (+ i 1))
                    #f)))))))

  ;; Helper: convert a hex char to its numeric value.
  (define hex-digit
    (lambda (c)
      (cond
        [(and (char>=? c #\0) (char<=? c #\9))
         (- (char->integer c) (char->integer #\0))]
        [(and (char>=? c #\a) (char<=? c #\f))
         (+ 10 (- (char->integer c) (char->integer #\a)))]
        [(and (char>=? c #\A) (char<=? c #\F))
         (+ 10 (- (char->integer c) (char->integer #\A)))]
        [else 0])))

) ;; end library