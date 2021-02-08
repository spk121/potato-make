(define-module (potato command-line)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 getopt-long)
  #:use-module (potato exceptions)
  #:export (OPT_QUIET
            OPT_VERBOSE
            opt-elevate-env-vars
            opt-ignore-errors
            opt-continue-on-error
            opt-dry-run
            opt-dump-macros
            opt-no-builtins
            opt-silent
            parse-command-line-and-makeflags))

(define option-spec
  '((help             (single-char #\h) (value #f))
    (version          (single-char #\v) (value #f))
    (quiet            (single-char #\q) (value #f))
    (verbose                            (value #f))
    (elevate-env-vars (single-char #\e) (value #f))
    (ignore-errors    (single-char #\i) (value #f))
    (continue-on-error (single-char #\k) (value #f))
    (dry-run          (single-char #\n) (value #f))
    (dump-macros      (single-char #\p) (value #f))
    (no-builtins      (single-char #\r) (value #f))
    (silent           (single-char #\s) (value #f))))

(define OPT_HELP 0)
(define OPT_VERSION 1)
(define OPT_QUIET 2)
(define OPT_VERBOSE 3)
(define opt-elevate-env-vars 1)
(define opt-ignore-errors 2)
(define opt-continue-on-error 3)
(define opt-dry-run 4)
(define opt-dump-macros 5)
(define opt-no-builtins 6)
(define opt-silent 7)
#|
;; FIXME: unused. Kill it?
(define* (open-makefile-port #:optional (name #f))
  "If NAME is defined and is a regular file, a input file port will be
opened for NAME.  If NAME is '-', the standard input port will be
returned.  If NAME is #f, a file port will be attempted for 'makefile'
and then 'Makefile'.  It will throw an exception if no file can be
found."
  (cond
   ((equal? name "-")
    current-input-port)
   ((string? name)
    (if (file-exists? name)
        (open-input-file name)
        (raise-exception (make-makefile-not-found-exception))))
   ((file-exists? "makefile")
    (open-input-file "makefile"))
   ((file-exists? "Makefile")
    (open-input-file "Makefile"))
   (else
    (raise-exception (make-makefile-not-found-exception)))))
|#

(define (parse-macros lst)
  "Search for list for strings of the form KEY=VAR and return a list
of pairs of KEY VAL"
  (filter-map
   (lambda (str)
     (let ((tok (string-split str #\x)))
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

(define* (parse-makeflags #:key (environ #t))
  "Checks the MAKEFLAGS environment variable for single space-separated
letters that match options flags."
  (let ((makeflags (getenv "MAKEFLAGS")))
    (if (or (not makeflags) (not environ))
        (list #f #f #f #f #f #f #f #f)
        ;; else
        (let* ((tokens (string-tokenize makeflags))
               (check (lambda (str)
                        (if (member str tokens) #t #f))))
          (map check '("v" "e" "i" "k" "n" "p" "s"))))))

(define (merge-options opts1 opts2)
  (map (lambda (a b)
         (or a b))
       opts1
       opts2))

(define (display-help-and-exit argv0)
  (format #t "command-line options for ~S~%" argv0)
  (format #t "    -h  --help     display this help~%")
  (format #t "    -v  --version  display library version~%")
  (exit 0))

(define (display-version-and-exit argv0 ver)
  (format #t "~S version ~S~%" argv0 ver)
  (exit 0))

(define* (parse-command-line-and-makeflags
          #:key (arguments '("unknown"))
          (environ #f)
          (version "unknown"))
  "This does the standard parsing of the MAKEFLAGS environment
variable and the command-line arguments and sets up the
environment. ARGUMENTS, if present, should be of the form passed by
Guile's 'program-arguments' procedure."
  (let* ((makeflags-options (parse-makeflags #:environ environ))
         (options-requested (getopt-long arguments option-spec))
         (options
          (map (lambda opt
                 (option-ref options-requested opt #f))
               '(help
                 version
                 silent
                 elevate-env-vars
                 ignore-errors
                 continue-on-error
                 dry-run
                 dump-macros
                 check-target
                 no-builtins))))
    (when (list-ref options OPT_HELP)
      (display-help-and-exit (car arguments)))
    (when (list-ref options OPT_VERSION)
      (display-version-and-exit (car arguments) version))
    (let ((macros (parse-macros (option-ref options-requested '() '())))
         (targets (parse-targets (option-ref options-requested '() '()))))
    (values
     (merge-options makeflags-options options)
     macros
     targets)))
