;;; Test.Main — Chez Scheme FFI

(library (Test.Main foreign)
  (export argv)
  (import (chezscheme)
          (purescm pstring)
          (srfi :214))

  (define argv
    (list->flexvector
      (map string->pstring (command-line))))

) ;; end library