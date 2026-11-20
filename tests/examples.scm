;;; examples.scm -- part of the Potato Make test suite.
;;;
;;; Runs the scripts in the examples directory the way a user would,
;;; as separate Guile processes.

(use-modules (srfi srfi-64)
             (ice-9 ftw)
             (ice-9 popen)
             (ice-9 rdelim)
             (tests common))

(test-begin "examples")

(define guile (or (getenv "GUILE") "guile"))
(define top-srcdir (or (getenv "abs_top_srcdir") (getcwd)))
(define examples-dir (string-append top-srcdir "/examples"))

(define (run-example script . args)
  "Run the example SCRIPT with ARGS.  Return its exit status and its
output."
  (let* ((port (apply open-pipe* OPEN_READ guile "--no-auto-compile" "-s"
                      script "--ascii" args))
         (output (read-string port))
         (status (status:exit-val (close-pipe port))))
    (list status output)))

(define (command-available? name)
  (zero? (system (string-append "command -v " name " >/dev/null 2>&1"))))

;;;;;;;;;;;;;;;;
;; hello.scm

(define hello (string-append examples-dir "/hello.scm"))

(test-assert "hello.scm prints its greeting."
  (let ((result (run-example hello)))
    (and (eqv? 0 (car result))
         (string-contains (cadr result) "Hello from potato-make!"))))

(test-assert "hello.scm accepts its target by name."
  (let ((result (run-example hello "hello")))
    (and (eqv? 0 (car result))
         (string-contains (cadr result) "Hello from potato-make!"))))

(test-assert "hello.scm --verbosity=0 prints only the greeting."
  (let ((result (run-example hello "--verbosity=0")))
    (and (eqv? 0 (car result))
         (string=? (cadr result) "Hello from potato-make!\n"))))

;;;;;;;;;;;;;;;;
;; src/makefile.scm

(define src-dir (string-append examples-dir "/src"))
(define src-files
  '("makefile.scm" "main.c" "foo.c" "foo.h" "alpha bravo.c" "alpha bravo.h"))

(define-syntax-rule (in-copy-of-src body ...)
  ;; The source tree may be read-only, so build in a copy.
  (in-temp-dir
   (for-each (lambda (file)
               (copy-file (string-append src-dir "/" file) file))
             src-files)
   body ...))

(define have-cc? (command-available? "cc"))

(unless have-cc? (test-skip 3))

(test-assert "src/makefile.scm builds a program from sources, one of
which has a space in its name."
  (in-copy-of-src
   (let ((result (run-example "makefile.scm")))
     (and (eqv? 0 (car result))
          (file-exists? "alpha bravo.o")
          (file-exists? "hello")
          (zero? (system "./hello >/dev/null"))))))

(test-assert "src/makefile.scm does nothing when everything is up to date."
  (in-copy-of-src
   (run-example "makefile.scm")
   (let ((result (run-example "makefile.scm")))
     (and (eqv? 0 (car result))
          (not (string-contains (cadr result) "cc "))))))

(test-assert "src/makefile.scm clean removes the build products."
  (in-copy-of-src
   (run-example "makefile.scm")
   (let ((result (run-example "makefile.scm" "clean")))
     (and (eqv? 0 (car result))
          (not (file-exists? "hello"))
          (not (file-exists? "main.o"))
          (not (file-exists? "alpha bravo.o"))))))

(test-exit "examples")
