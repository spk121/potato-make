(define-module (make main)
  #:use-module (ice-9 getopt-long)
  #:export (main)
  )

(define EXIT_SUCCESS 0)
(define EXIT_NOT_UP_TO_DATE 1)
(define EXIT_FAILURE 2)

(define option-spec
  '((environment-overrides (single-char #\e))
    (makefile (single-char #\f) (value #t))
    (ignore-errors (single-char #\i))
    (keep-going (single-char #\k))
    (dry-run (single-char #\n))
    (print-data-base (single-char #\p))
    (question (single-char #\q))
    (no-builtin-rules (single-char #\r))
    (stop (single-char #\S))
    (silent (single-char #\s))
    (touch (single-char #\t))))

(define (main args)
  (let ((options (getopt-long args option-spec)))
    (write options)
    (newline)
    EXIT_SUCCESS))
    
