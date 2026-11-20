;;; common.scm -- helpers shared by the test scripts.
;;;
;;; The test scripts are run through 'pre-inst-env', which puts the
;;; top of the source tree on Guile's load path.

(define-module (tests common)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 ftw)
  #:export (test-exit
            reset-commands!
            commands
            last-command
            stub-system-pass
            stub-system-fail
            call-in-temp-dir
            in-temp-dir
            touch
            output-of
            call-catching-exit))

(define (test-exit name)
  "End the test group NAME and exit.  The exit status is zero only
if there were no failures and no unexpected passes, which is how the
Automake test harness learns the result."
  (let ((runner (test-runner-current)))
    (test-end name)
    (exit (and (zero? (test-runner-fail-count runner))
               (zero? (test-runner-xpass-count runner))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stand-ins for the 'system' procedure, to be installed with
;; install-alternate-system-driver.  They record the command strings
;; instead of running them.

(define %commands '())

(define (reset-commands!)
  (set! %commands '()))

(define (commands)
  "Every command received since the last reset, oldest first."
  (reverse %commands))

(define (last-command)
  "The most recent command received, or #f."
  (if (null? %commands) #f (car %commands)))

(define (stub-system-pass cmd)
  (set! %commands (cons cmd %commands))
  0)

(define (stub-system-fail cmd)
  (set! %commands (cons cmd %commands))
  1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filesystem helpers.

(define (delete-recursively dir)
  (file-system-fold (const #t)
                    (lambda (name stat result) (delete-file name))
                    (const #t)
                    (lambda (name stat result) (rmdir name))
                    (const #t)
                    (lambda (name stat errno result) #f)
                    #t
                    dir
                    lstat))

(define (call-in-temp-dir thunk)
  "Call THUNK with a fresh, empty temporary directory as the current
directory.  The directory is removed afterward."
  (let* ((tmp (or (getenv "TMPDIR") "/tmp"))
         (dir (mkdtemp (string-append tmp "/potato-make-test-XXXXXX")))
         (old (getcwd)))
    (dynamic-wind
      (lambda () (chdir dir))
      thunk
      (lambda ()
        (chdir old)
        (delete-recursively dir)))))

(define-syntax-rule (in-temp-dir body ...)
  (call-in-temp-dir (lambda () body ...)))

(define* (touch name #:optional (mtime #f) (contents ""))
  "Create or overwrite the file NAME.  If MTIME is given, it is the
modification time, in seconds since the epoch."
  (call-with-output-file name
    (lambda (port) (display contents port)))
  (when mtime
    (utime name mtime mtime)))

(define-syntax-rule (output-of body ...)
  "Evaluate BODY and return, as a string, what it wrote to the
current output port."
  (with-output-to-string (lambda () body ...)))

(define (call-catching-exit thunk)
  "Call THUNK, which is expected to call 'exit'.  Return the exit
status as an integer, or #f if THUNK returned without exiting."
  (catch 'quit
    (lambda () (thunk) #f)
    (lambda (key . args)
      (cond
       ((null? args) 0)
       ((eq? (car args) #t) 0)
       ((eq? (car args) #f) 1)
       (else (car args))))))
