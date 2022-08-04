(cond-expand
  (guile
   (import (scheme base)
           (ini-files)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (ini-files)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (ini-files)
           (srfi 64))))


(test-begin "ini-files")

(test-group
 "make-ini-file-accumulator"

 (define result
   (let* ((port (open-output-string))
          (acc (make-ini-file-accumulator port)))

     ;; write leading section-less data
     (acc '("" "key1" "value1"))

     ;; write comment
     (acc "test comment")

     ;; write new section
     (acc '("section" "key2" "value2"))

     (get-output-string port)))

 (test-equal
     "key1=value1\n; test comment\n[section]\nkey2=value2\n"
   result))

(test-group
 "make-ini-file-generator"

 (define (read-to-list generator)
   (let loop ((lst '()))
     (define entry-lst (generator))
     (cond
      ((eof-object? entry-lst)
       (reverse lst))
      (else (loop (cons entry-lst lst))))))

 (define source "key1 = value1\n
; comment\n
\n
[section]\n
 key2 = value2\n
[section2]\n
key3\n
[key]4\n
\n
\n")

 (define result (read-to-list (make-ini-file-generator (open-input-string source))))

 (test-equal
     '(("" "key1" "value1")
       ("section" "key2" "value2")
       ("section2" "key3" "")
       ("section2" "[key]4" ""))
   result))

(test-end)
