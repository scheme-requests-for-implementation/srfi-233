(define (make-ini-file-accumulator port)
  (define current-section "")

  (define (write-comment str)
    (display "; " port)
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
    (display "=" port)
    (display value port)
    (newline port))

  (define (close)
    (close-output-port port)
    (eof-object))

  (define (data-triple? arg)
    (and (list? arg)
         (= 3 (length arg))
         (let loop ((arg arg))
           (cond
            ((null? arg) #t)
            ((not (string? (car arg))) #f)
            (else (loop (cdr arg)))))))

  (lambda (arg)
    (cond
     ((eof-object? arg) (close))
     ((string? arg) (write-comment arg))
     ((data-triple? arg) (apply write-data arg))
     (else (error "Unexpected input")))))

(define (make-ini-file-generator port)

  (define (trim-head line)
    (let loop ((chars (string->list line)))
      (cond
       ((null? chars) "")
       ((equal? #\space (car chars)) (loop (cdr chars)))
       (else (list->string chars)))))

  (define (trim-tail line)
    (let loop ((chars (string->list line))
               (chars/rev '())
               (spaces '()))
      (cond
       ((null? chars)
        (list->string (reverse chars/rev)))
       ((equal? #\space (car chars))
        (loop (cdr chars)
              chars/rev
              (cons #\space spaces)))
       (else (loop (cdr chars)
                   (append (list (car chars)) spaces chars/rev)
                   '())))))

  (define (trim line)
    (trim-tail (trim-head line)))

  (define (comment line)
    (let loop ((chars (string->list line)))
      (cond
       ((null? chars) #f)
       ((equal? (car chars) #\space) (loop (cdr chars)))
       ((equal? (car chars) #\;) #t)
       (else #f))))

  (define (section line)
    (define chars (string->list line))
    (define first (car chars))
    (if (equal? first #\[)
        (let loop ((chars (cdr chars))
                   (chars/rev '()))
          (cond
           ((null? chars) #f)
           ((and (null? (cdr chars))
                 (equal? (car chars) #\]))
            (list->string (reverse chars/rev)))
           (else (loop (cdr chars)
                       (cons (car chars) chars/rev)))))
        #f))

  (define (key-value line)
    (let loop ((chars (string->list line))
               (key-parsed #f)
               (key/rev '())
               (value/rev '()))
      (cond
       ((null? chars)
        (cons (trim-tail (list->string (reverse key/rev)))
              (trim-head (list->string (reverse value/rev)))))
       ((and (equal? (car chars) #\=)
             (not key-parsed))
        (loop (cdr chars)
              #t
              key/rev
              value/rev))
       (else (loop (cdr chars)
                   key-parsed
                   (if key-parsed
                       key/rev
                       (cons (car chars) key/rev))
                   (if key-parsed
                       (cons (car chars) value/rev)
                       value/rev))))))

  (define current-section "")
  (define eof #f)

  (lambda ()
    (call/cc
     (lambda (k)
       (when eof
         (k (eof-object)))
       (let loop ()
         (define line (read-line port))
         (when (eof-object? line)
           (begin
             (set! eof #t)
             (close-input-port port)
             (k (eof-object))))
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
