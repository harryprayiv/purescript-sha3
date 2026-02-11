(library (Test.Main foreign)
  (export hasBenchFlag)
  (import (chezscheme))

  (define hasBenchFlag
    (and (getenv "BENCH") #t))

) ;; end library