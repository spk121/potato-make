(define-module (potato make)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 receive)
  #:use-module (system vm trace)
  #:use-module (potato exceptions)
  #:use-module (potato makevars)
  #:use-module (potato rules)
  #:use-module (potato text)
  #:export (initialize
            execute)
  #:re-export (%suffix-rules
               lazy-assign    ?=
               assign         :=
               reference      $    Q
               reference-func $$
               target-rule    :
               suffix-rule    ->
               target-name          $@
               target-basename      $*
               newer-prerequisites  $? $$?
               prerequisites        $^ $$^
               primary-prerequisite $<
               string-compose       ~
               silent-compose       ~@
               always-execute-compose ~+
               ignore-error-compose ~-
               install-alternate-system-driver
                              ))

(define %version "1.0")
(define %debug-argv0 #f)

;; #:re-export (
;;              lazy-assign ?=
;;              assign      :=
;;              ref         $
;;              target-rule :
;;              suffix-rule  ->
;;              compose     ~
;;              ))

;; Asynchronous events.
;; SIGHUP, SIGTERM, SIGINT and SIGQUIT remove the current target
;; unless that target is a directory or the target is a prerequisite
;; of .PRECIOUS or the -n, -q, or -p option was specified.  This
;; deletion shall be reported to the error port, then the default for
;; that action will continue.

;; .SILENT
;; The utility shall write all commands to the standard output unless
;; the -s option was specified, the command is prefixed with +, or
;; .SILENT has the current target as a prerequisite or has no pre
;; requisites.

;; Nothing to be done
;; If make was invoked but found no work to do, it shall write a
;; message to standard output that no action was taken

;; File Touched
;; If the -t option was specified, make shall write to standard
;; output a message for each file that was touched.

;; Verbosity is 0 = silent, 1 = terse, 2 = default, 3 = verbose
(define %verbosity 2)
(define %opt-verbose #f)
(define %opt-ignore-errors #f)
(define %opt-continue-on-error #f)
(define %targets '())
(define %initialized #f)

(define (critical spec . args)
  (apply format (append (list #t spec) args)))
(define (print spec . args)
  (when (>= %verbosity 2)
    (apply format (append (list #t spec) args))))
(define (debug spec . args)
  (when (>= %verbosity 3)
    (apply format (append (list #t spec) args))))

(define option-spec
  '((help              (single-char #\h) (value #f))
    (version           (single-char #\v) (value #f))
    (verbosity         (single-char #\V) (value #t))
    (environment       (single-char #\e) (value #f))
    (elevate-environment (single-char #\E) (value #f))
    (builtins          (single-char #\b) (value #f))
    (ignore-errors                       (value #f))
    (continue-on-error (single-char #\k) (value #f))
    (no-execution      (single-char #\n) (value #f))
    (ascii             (single-char #\A) (value #f))
    (strict            (single-char #\S) (value #f))
    ))

(define (display-help-and-exit argv0)
  (format #t "~A [-hvqVeEbn] [KEY=VALUE ...] [targets ...]~%" argv0)
  (format #t "    -h, --help                     print help and exit~%")
  (format #t "    -v, --version               print version and exit~%")
  (format #t "    -V 0..3, --verbosity=0..3~%")
  (format #t "           set output level from 0=silent to 3=verbose~%")
  (format #t "    -e, --environment        use environment variables~%")
  (format #t "    -E, --elevate-environment~%")
  (format #t "                     use environment variables and let~%")
  (format #t "                        them override script variables~%")
  (format #t "    -b, --builtins~%")
  (format #t "        include some common variables and suffix rules~%")
  (format #t "    --ignore-errors~%")
  (format #t "                                     ignore all errors~%")
  (format #t "    -k, --continue-on-error~%")
  (format #t "           after an error, keep building other targets~%")
  (format #t "    -n, --no-execution~%")
  (format #t "         only execute rules marked as 'always execute'~%")
  (format #t "    -a, --ascii~%")
  (format #t "                       ASCII only output and no colors~%")
  (format #t "    -S, --strict~%")
  (format #t "                causes some behaviours to throw errors~%")
  (exit 0))

(define (display-version-and-exit argv0)
  (format #t "~a~%" argv0)
  (format #t "  using potato make~a~%" %version)
  (exit 0))

(define (parse-macros lst)
  "Search for list for strings of the form KEY=VAR and return a list
of pairs of KEY VAL"
  (filter-map
   (lambda (str)
     (let ((tok (string-split str #\=)))
       (cond
        ((= 1 (length tok))
         #f)
        ((= 2 (length tok))
         (cons (car tok) (cadr tok)))
        (else
         (invalid-macro "parse-macros" str)))))
   lst))

(define (parse-targets lst)
  "Search the list for strings that don't have equals signs, and
return them in a list."
  (filter-map
   (lambda (str)
     (if (string-index str #\=)
         #f
         str))
   lst))

(define* (initialize #:optional
                     (arguments #f))
  "Set up the options, rules, and makevars. If ARGUMENTS
is not set, it will use options, makevars, and targets as
specified by the command line.  If it is set, it is
expected to be a list of strings that are command-line
arguments."

  ;; If left unset, assume user want all the command line arguments.
  (when (not arguments)
    (set! arguments (command-line)))
  ;; If the user has set it to '(), expecting a null environment, add
  ;; back in a filename, which is required.
  (when (null? arguments)
    (set! arguments (list (car (program-arguments)))))

  ;; We start of with the --help and --version command-line arguments.
  (let ((options (getopt-long arguments option-spec))
        (%opt-builtins #f)
        (%opt-environment #f)
        (%opt-elevate-environment #f)
        (%opt-no-errors #f)
        (%opt-continue-on-error #f)
        (%opt-no-execution #f)
        (%opt-ascii #f)
        (%opt-strict #f))
    (when (option-ref options 'help #f)
      (display-help-and-exit (car arguments)))
    (when (option-ref options 'version #f)
      (display-version-and-exit (car arguments)))

    ;; Then, we do --environment, because we need to know that
    ;; before we start parsing MAKEFLAGS
    (set! %opt-environment
      (option-ref options 'environment #f))

    ;; Parse MAKEFLAGS before the command-line, because we want
    ;; command-line options to override MAKEFLAGS options.
    (when %opt-environment
      (let ((mf (getenv "MAKEFLAGS")))
        (when mf
          (let ((tokens (string-tokenize mf)))
            (when (member "silent" tokens)
              (set! %verbosity 0))
            (when (member "terse" tokens)
              (set! %verbosity 1))
            (when (member "verbose" tokens)
              (set! %verbosity 3))
            (when (member "builtins" tokens)
              (set! %opt-builtins #t))
            (when (member "ascii" tokens)
              (set! %opt-ascii #t))
            (when (member "ignore-errors" tokens)
              (set! %opt-ignore-errors #t))
            (when (member "continue-on-error" tokens)
              (set! %opt-continue-on-error #t))
            (when (member "strict" tokens)
              (set! %opt-strict #t))
            (when (member "no-execution" tokens)
              (set! %opt-no-execution #t))))))

    ;; Now the bulk of the command-line options.
    (when (option-ref options 'verbosity #f)
      (let ((verbosity (string->number (option-ref options 'verbosity #f))))
        (when verbosity
          (set! %verbosity verbosity))))
    (when (option-ref options 'builtins #f)
      (set! %opt-builtins #t))
    (when (option-ref options 'elevate-environment #f)
      (set! %opt-elevate-environment #t))
    (when (option-ref options 'ignore-errors #f)
      (set! %opt-ignore-errors #t))
    (when (option-ref options 'continue-on-error #f)
      (set! %opt-continue-on-error #t))
    (when (option-ref options 'no-execution #f)
      (set! %opt-no-execution #t))
    (when (option-ref options 'ascii #f)
      (set! %opt-ascii #t))
    (when (option-ref options 'strict #f)
      (set! %opt-strict #t))

    ;; Now that all the options are set, we can set up
    ;; the build environment.
    (let ((extra (option-ref options '() '())))
      (initialize-text %opt-ascii)
      (initialize-makevars (parse-macros extra)
                           %opt-environment
                           %opt-elevate-environment
                           %opt-builtins
                           %opt-strict
                           %verbosity
                           %opt-ascii)
      ;; The remaining command-line words are the build targets that
      ;; we're going to tackle.
      (set! %targets (parse-targets extra))
      (initialize-rules %targets
                        %opt-builtins
                        %opt-ignore-errors
                        %opt-continue-on-error
                        %opt-no-execution
                        %verbosity
                        %opt-ascii)
      (set! %initialized #t)
      %targets
      )))

(define* (execute #:key (targets '()))
  "This function runs build actions.  TARGETS, if provided, is a list
of target names to be executed.  If TARGETS is not provided, the
targets listed on the parsed command-line are used."

  ;; First, let's figure out what targets we're building.
  (unless %initialized
    (critical "The initialize procedure was not called in this build script.~%")
    (critical "Using an empty environment.~%"))
  (when (null? targets)
    (set! targets %targets))
  (when (null? targets)
    (debug "No build target was explicitely specified.~%")
    (let ((rule (first-target-rule-name)))
      (if rule
          (begin
            (debug "Using first rule ~a~A~a as the build target.~%" (lquo) rule (rquo))
            (set! targets (list rule)))
          ;; else
          (debug "There are no target rules in the recipe.~%"))))

  ;; Build each target in order.
  (when (not (null? targets))
    (let loop ((target (car targets))
               (rest (cdr targets)))
      (if (not (build target))
          (begin
            (print "The recipe for “~A” has failed.~%" target)
            #f)
          ;; else
          (begin
            (print "The recipe “~A” finished successfully.~%" target)
            (if (not (null? rest))
                (loop (car rest) (cdr rest))

                ;; True if all targets are built successfully.
                #t))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

