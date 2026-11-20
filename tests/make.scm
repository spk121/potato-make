;;; make.scm -- part of the Potato Make test suite.
;;;
;;; Tests of command-line parsing, 'initialize', 'execute' and the
;;; extension mechanism.

(use-modules (potato make)
             (srfi srfi-1)
             (srfi srfi-64)
             (tests common))

(test-begin "make")

(define quiet '("test" "--verbosity=0"))

(install-alternate-system-driver stub-system-pass)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --version and --help

(define package-version (getenv "POTATO_MAKE_VERSION"))

(unless package-version
  (test-skip "The library version matches the package version."))
(test-equal "The library version matches the package version."
  package-version
  (@@ (potato make) %version))

(test-assert "--version prints the version and exits successfully."
  (let* ((status #f)
         (out (output-of
               (set! status
                 (call-catching-exit
                  (lambda () (initialize '("myscript" "--version"))))))))
    (and (eqv? status 0)
         (string-contains out "myscript")
         (string-contains
          out
          (string-append "potato make " (@@ (potato make) %version))))))

(test-assert "--help describes the options and exits successfully."
  (let* ((status #f)
         (out (output-of
               (set! status
                 (call-catching-exit
                  (lambda () (initialize '("myscript" "-h"))))))))
    (and (eqv? status 0)
         (string-contains out "myscript")
         (every (lambda (opt) (string-contains out opt))
                '("-h, --help" "-v, --version" "--verbosity"
                  "-e, --environment" "-E, --elevate-environment"
                  "-b, --builtins" "--ignore-errors"
                  "-k, --continue-on-error" "-n, --no-execution"
                  "-A, --ascii" "-S, --strict")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arguments

(test-equal "initialize returns the targets named on the command line."
  '("all" "clean")
  (initialize '("test" "--verbosity=0" "all" "CC=gcc" "clean")))

(test-equal "initialize with no targets returns an empty list."
  '()
  (initialize quiet))

(test-equal "initialize with an empty list ignores the real command line."
  '()
  (let ((targets #f))
    (output-of (set! targets (initialize '())))
    targets))

(test-equal "KEY=VALUE arguments become makevars."
  '("gcc" "-g")
  (begin
    (initialize '("test" "--verbosity=0" "CC=gcc" "all" "CFLAGS=-g"))
    (list ($ CC) ($ CFLAGS))))

(test-equal "A makevar value may itself contain an equals sign."
  "-DFOO=1"
  (begin
    (initialize '("test" "--verbosity=0" "CFLAGS=-DFOO=1"))
    ($ CFLAGS)))

(test-equal "A makevar may be given an empty value."
  '("" ())
  (let ((targets (initialize '("test" "--verbosity=0" "EMPTY="))))
    (list ($ EMPTY) targets)))

(test-equal "Short options are accepted."
  "cc"
  (begin
    (initialize '("test" "-V" "0" "-b"))
    ($ CC)))

(test-assert "Each call to initialize starts from the default settings."
  (begin
    (initialize '("test" "--verbosity=0" "--no-execution" "--builtins"
                  "FOO=bar"))
    (: "stale" '() #t)
    (let ((out (output-of
                (initialize '("test"))
                (reset-commands!)
                (: "all" '() "run me")
                (execute))))
      (and (equal? (commands) '("run me"))    ; --no-execution was reset
           (string-contains out "run me")     ; verbosity was reset
           (string-null? ($ FOO))             ; makevars were reset
           (string-null? ($ CC))))))          ; built-ins were reset

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKEFLAGS

(define (output-with-makeflags makeflags . args)
  "Run a one-rule build with MAKEFLAGS in the environment and return
what was printed."
  (setenv "MAKEFLAGS" makeflags)
  (let ((out (output-of
              (initialize (cons "test" args))
              (reset-commands!)
              (: "all" '() "run me")
              (execute))))
    (unsetenv "MAKEFLAGS")
    out))

(test-assert "MAKEFLAGS is ignored unless the environment is enabled."
  (not (string-null? (output-with-makeflags "silent"))))

(test-equal "MAKEFLAGS 'silent' is honored with --environment."
  ""
  (output-with-makeflags "silent" "--environment"))

(test-equal "MAKEFLAGS 'silent' is honored with --elevate-environment."
  ""
  (output-with-makeflags "silent" "--elevate-environment"))

(test-assert "MAKEFLAGS 'verbose' is honored."
  (string-contains (output-with-makeflags "verbose" "-e") "Target rule"))

(test-assert "The command line overrides the verbosity from MAKEFLAGS."
  (not (string-null? (output-with-makeflags "silent" "-e" "--verbosity=2"))))

(test-equal "MAKEFLAGS 'no-execution' is honored."
  '()
  (begin
    (output-with-makeflags "silent no-execution" "-e")
    (commands)))

(test-equal "MAKEFLAGS 'builtins' is honored."
  "cc"
  (begin
    (output-with-makeflags "silent builtins" "-e")
    ($ CC)))

(test-equal "MAKEFLAGS may mix option words and makevar assignments."
  "BAR"
  (begin
    (output-with-makeflags "silent FOO=BAR" "-e")
    ($ FOO)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; execute

(test-assert "execute with no rules and no targets succeeds."
  (begin
    (initialize quiet)
    (execute)))

(test-equal "execute builds the first rule by default."
  '(first)
  (let ((seen '()))
    (initialize quiet)
    (: "first" '() (lambda () (set! seen (cons 'first seen))))
    (: "second" '() (lambda () (set! seen (cons 'second seen))))
    (execute)
    seen))

(test-equal "execute builds the targets from the command line, in order."
  '(second first)
  (let ((seen '()))
    (initialize '("test" "--verbosity=0" "second" "first"))
    (: "first" '() (lambda () (set! seen (cons 'first seen))))
    (: "second" '() (lambda () (set! seen (cons 'second seen))))
    (execute)
    (reverse seen)))

(test-equal "execute #:targets overrides the command line."
  '(second)
  (let ((seen '()))
    (initialize '("test" "--verbosity=0" "first"))
    (: "first" '() (lambda () (set! seen (cons 'first seen))))
    (: "second" '() (lambda () (set! seen (cons 'second seen))))
    (execute #:targets '("second"))
    seen))

(test-equal "execute stops at the first failed target."
  '(#f (bad))
  (let ((seen '()))
    (initialize '("test" "--verbosity=0" "bad" "good"))
    (: "bad" '() (lambda () (set! seen (cons 'bad seen)) #f))
    (: "good" '() (lambda () (set! seen (cons 'good seen)) #t))
    (let ((result (execute)))
      (list result (reverse seen)))))

(test-equal "execute -k carries on after a failed target, but reports failure."
  '(#f (bad good))
  (let ((seen '()))
    (initialize '("test" "--verbosity=0" "-k" "bad" "good"))
    (: "bad" '() (lambda () (set! seen (cons 'bad seen)) #f))
    (: "good" '() (lambda () (set! seen (cons 'good seen)) #t))
    (let ((result (execute)))
      (list result (reverse seen)))))

(test-assert "execute fails for a target that has no rule and no file."
  (begin
    (initialize quiet)
    (: "all" '() #t)
    (not (execute #:targets '("no-such-target-or-file")))))

(test-assert "A later rule for the same target replaces an earlier one."
  (begin
    (initialize quiet)
    (: "all" '() #f)
    (: "all" '() #t)
    (execute #:targets '("all"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extensions

(define %hook-calls 0)
(define %hook-value #f)

(register-extension!
 #:id 'test-extension
 #:option-spec '((flavor (value #t)))
 #:help-lines '("    --flavor=NAME" "        choose a flavor")
 #:init-hook (lambda (options)
               (set! %hook-calls (1+ %hook-calls))
               (set! %hook-value
                 ((@ (ice-9 getopt-long) option-ref) options 'flavor #f))
               (:= FLAVOR (or %hook-value "plain"))))

(test-equal "An extension can add a command-line option."
  "russet"
  (begin
    (initialize '("test" "--verbosity=0" "--flavor=russet"))
    %hook-value))

(test-equal "An extension's init hook can set makevars."
  '("russet" "plain")
  (list
   (begin
     (initialize '("test" "--verbosity=0" "--flavor=russet"))
     ($ FLAVOR))
   (begin
     (initialize quiet)
     ($ FLAVOR))))

(test-assert "An extension can add lines to --help."
  (let ((out (output-of
              (call-catching-exit
               (lambda () (initialize '("test" "--help")))))))
    (and (string-contains out "--flavor=NAME")
         (string-contains out "choose a flavor"))))

(test-equal "Registering an extension ID twice has no effect."
  1
  (begin
    (register-extension!
     #:id 'test-extension
     #:init-hook (lambda (options)
                   (set! %hook-calls (+ 100 %hook-calls))))
    (set! %hook-calls 0)
    (initialize quiet)
    %hook-calls))

(test-exit "make")
