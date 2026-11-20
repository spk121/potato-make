;;; exceptions.scm -- part of the Potato Make test suite.

(use-modules (potato exceptions)
             (ice-9 exceptions)
             (srfi srfi-64)
             (tests common))

(test-begin "exceptions")

(define (caught thunk)
  "Call THUNK and return the exception object that it raises, or #f."
  (with-exception-handler
      (lambda (exn) exn)
    (lambda () (thunk) #f)
    #:unwind? #t))

(define raisers
  `((bad-key-type           ,bad-key-type)
    (bad-value-type         ,bad-value-type)
    (bad-proc-output        ,bad-proc-output)
    (invalid-macro          ,invalid-macro)
    (not-a-regular-file     ,not-a-regular-file)
    (not-a-procedure        ,not-a-procedure)
    (no-read-access-to-file ,no-read-access-to-file)
    (invalid-argument-type  ,invalid-argument-type)))

(for-each
 (lambda (entry)
   (let* ((name (symbol->string (car entry)))
          (raiser (cadr entry))
          (exn (caught (lambda () (raiser "origin-proc" '(irritant))))))
     (test-assert (string-append name " raises a programming error.")
       (programming-error? exn))
     (test-equal (string-append name " records its origin.")
       "origin-proc"
       (and (exception-with-origin? exn)
            (exception-origin exn)))
     (test-equal (string-append name " records its irritants.")
       '(irritant)
       (and (exception-with-irritants? exn)
            (exception-irritants exn)))
     (test-assert (string-append name " has a message.")
       (and (exception-with-message? exn)
            (string? (exception-message exn))
            (not (string-null? (exception-message exn)))))))
 raisers)

(test-exit "exceptions")
