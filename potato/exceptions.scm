(define-module (potato exceptions)
  #:use-module (ice-9 exceptions)
  #:export (bad-key-type
            bad-value-type
            bad-proc-output
            invalid-macro
            not-a-regular-file
            not-a-procedure
            no-read-access-to-file
            ))

(define (make-bad-key-type origin irritants)
  (make-exception
   (make-programming-error)
   (make-exception-with-origin origin)
   (make-exception-with-message "Wrong type for key. Expecting string or procedure.")
   (make-exception-with-irritants irritants)))

(define (bad-key-type origin irritant)
  (raise-exception (make-bad-key-type origin irritant)))

(define (make-bad-value-type origin irritants)
  (make-exception
   (make-programming-error)
   (make-exception-with-origin origin)
   (make-exception-with-message "Wrong type for value. Expecting string or procedure.")
   (make-exception-with-irritants irritants)))

(define (bad-value-type origin irritant)
  (raise-exception (make-bad-value-type origin irritant)))

(define (make-bad-proc-output origin irritants)
  (make-exception
   (make-programming-error)
   (make-exception-with-origin origin)
   (make-exception-with-message "Procedure does not evaluate to a string.")
   (make-exception-with-irritants irritants)))

(define (bad-proc-output origin irritant)
  (raise-exception (make-bad-proc-output origin irritant)))

(define (make-invalid-macro origin irritants)
  (make-exception
   (make-programming-error)
   (make-exception-with-origin origin)
   (make-exception-with-message "Invalid macro format")
   (make-exception-with-irritants irritants)))

(define (invalid-macro origin irritant)
  (raise-exception (make-invalid-macro origin irritant)))

(define (make-not-a-regular-file origin irritants)
  (make-exception
   (make-programming-error)
   (make-exception-with-origin origin)
   (make-exception-with-message "Not a regular file")
   (make-exception-with-irritants irritants)))

(define (not-a-regular-file origin irritant)
  (raise-exception (make-not-a-regular-file origin irritant)))

(define (make-not-a-procedure origin irritants)
  (make-exception
   (make-programming-error)
   (make-exception-with-origin origin)
   (make-exception-with-message "Not a procedure")
   (make-exception-with-irritants irritants)))

(define (not-a-procedure origin irritant)
  (raise-exception (make-not-a-procedure origin irritant)))

(define (make-no-read-access-to-file origin irritants)
  (make-exception
   (make-programming-error)
   (make-exception-with-origin origin)
   (make-exception-with-message "Do not have permissions to read file")
   (make-exception-with-irritants irritants)))

(define (no-read-access-to-file origin irritant)
  (raise-exception (make-no-read-access-to-file origin irritant)))
