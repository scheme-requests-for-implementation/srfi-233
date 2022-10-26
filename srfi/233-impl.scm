(define make-ini-file-accumulator
  (case-lambda
    ((port)
     (make-accumulator port #\= #\;))
    ((port key-value-sep)
     (make-accumulator port key-value-sep #\;))
    ((port key-value-sep comment-delim)
     (make-accumulator port key-value-sep comment-delim))))

(define (make-accumulator port key-value-sep comment-delim)
  (define current-section '||)

  (define (write-comment str)
    (display comment-delim port)
    (display " " port)
    (display str port)
    (newline port))

  (define (write-data section key value)
    (unless (equal? section current-section)
      (set! current-section section)
      (display "[" port)
      (display section port)
      (display "]" port)
      (newline port))
    (display key port)
    (display key-value-sep port)
    (display value port)
    (newline port))

  (define (data-triple? arg)
    (and (list? arg)
         (= 3 (length arg))
         (symbol? (list-ref arg 0))
         (symbol? (list-ref arg 1))
         (string? (list-ref arg 2))))

  (lambda (arg)
    (cond
     ((eof-object? arg) (eof-object))
     ((string? arg) (write-comment arg))
     ((data-triple? arg) (apply write-data arg))
     (else (error "Unexpected input")))))

(define make-ini-file-generator
  (case-lambda
    ((port)
     (make-generator port #\= #\;))
    ((port key-value-sep)
     (make-generator port key-value-sep #\;))
    ((port key-value-sep comment-delim)
     (make-generator port key-value-sep comment-delim))))

(define (make-generator port key-value-sep comment-delim)

  ;; remove space from both ends of string
  (define (trim str)
    (define (space? char) (char=? #\space char))
    (let loop1 ((end (string-length str)))
      (if (and (> end 0) (space? (string-ref str (- end 1))))
        (loop1 (- end 1))
        (let loop2 ((start 0))
          (if (and (< start end) (space? (string-ref str start)))
            (loop2 (+ start 1))
            (substring str start end))))))

  ;; return #t if the line is a comment,
  ;; #f otherwise
  (define (comment line)
    (define len (string-length line))
    (let loop ((i 0))
      (cond
       ((>= i len) #f)
       ((equal? (string-ref line i) #\space) (loop (+ 1 i)))
       ((equal? (string-ref line i) comment-delim) #t)
       (else #f))))

  ;; return section name as a symbol if the line is a section declaration,
  ;; #f otherwise
  (define (section line)
    (define len (string-length line))
    (cond
      ((= len 0) #f)
      ((not (char=? (string-ref line 0) #\[)) #f)
      (else (let loop ((i 0))
              (cond
                ((>= i len) #f)
                ((and (= i (- len 1))
                      (char=? (string-ref line i) #\]))
                 (string->symbol (substring line 1 (- len 1))))
                (else (loop (+ i 1))))))))

  ;; return pair of key and value
  ;;
  ;; if line has a separator char,
  ;; key is a (whitespace trimmed) symbol up to first separator char, value is a (whitespace trimmed) string
  ;;
  ;; if line doesn't have a separator char
  ;; key is a #f, value is the entire line
  ;;
  ;; if separator char is encounted multiple times, first one is used to distinguish key from value
  (define (key-value line)
    (define len (string-length line))
    (let loop ((i 0))
      (cond
        ;; line parsed without encountering a separator
        ((>= i len)
         (cons #f
               line))
        ;; encountered a (first leftmost) separator
        ((char=? key-value-sep (string-ref line i))
         (cons (string->symbol (trim (substring line 0 i)))
               (if (>= (+ i 1) len)
                 ""
                 (trim (substring line (+ i 1) len)))))
        (else
          (loop (+ i 1))))))

  (define current-section '||)
  (define eof #f)

  (lambda ()
    (or (and eof (eof-object))
        (let loop ()
          (define line (read-line port))
          (if (eof-object? line)
            (begin
              (set! eof #t)
              (eof-object))
            (let ((trimmed-line (trim line)))
              (cond
                ((= 0 (string-length trimmed-line))
                 (loop))
                ((comment trimmed-line)
                 (loop))
                ((section trimmed-line) => (lambda (section)
                                             (set! current-section section)
                                             (loop)))
                ((key-value trimmed-line) => (lambda (key-value-pair)
                                               (list current-section
                                                     (car key-value-pair)
                                                     (cdr key-value-pair)))))))))))
