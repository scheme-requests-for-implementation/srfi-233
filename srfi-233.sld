(define-library (srfi-233)
    (import (scheme base)
            (scheme case-lambda)
            (scheme write))
    (export
        make-ini-file-generator
        make-ini-file-accumulator)
    (include "ini-files-impl.scm"))
