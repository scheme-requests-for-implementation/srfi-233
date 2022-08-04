(define-library
    (ini-files)
    (import (scheme base)
            (scheme write))
    (export
        make-ini-file-generator
        make-ini-file-accumulator)
    (include "ini-files-impl.scm"))
