(define-module (potato makevars)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (potato exceptions)
  #:use-module (potato builtins)
  #:use-module (potato text)
  #:export (initialize-makevars
            %makevars
            %elevate-environment?
            lazy-assign    ?=
            assign         :=
            reference      $
            reference-func $$
            dump-makevars
            ))

;; There are five priority levels
;; 1. defined - in the script itself
;; 2. command-line
;; 3. makeflags - in the MAKEFLAGS environment variable
;; 4. env - specified in the environment
;; 5. built-in - one of the built-in macros

;; The lower priority level always win, unless the '-e' flag was set
;; If the '-e' flag is set level 1 doesn't override level 3 and 4.

(define %ascii? #f)
(define %makevars #f)
(define %elevate-environment? #f)
(define %verbose? #t)
(define (debug spec . args)
  (when %verbose?
    (apply format (append (list #t spec) args))))


(define (split-at-equals str)
  "Splits the string at the first equals sign, ignoring
later equals signs."
  (let ((idx (string-index str #\=)))
    (if (and idx (> idx 0))
        (cons (substring str 0 idx)
              (substring str (1+ idx)))
        ;; else
        #f)))

(define (string-append-with-spaces lst)
  "Appends the strings in lst, adding spaces in between."
  (fold
   (lambda (elem prev)
     (string-append prev " " elem))
   (car lst)
   (cdr lst)))

(define (override? old-priority new-priority)
  "The logic of whether which makemacro priority levels can override
others."
  (if %elevate-environment?
      (if (and (or (= old-priority) (= old-priority 3) (= old-priority 4))
               (= new-priority 1))
          #f
          ;; else
          (<= new-priority old-priority))
      ;; else
      (<= new-priority old-priority)))

(define* (makevars-set key
                       #:optional (new-val "") (new-priority 1))
  "Maybe add key / val to %makevars hash table, if there is sufficient
priority."
  ;; Note that VAL can be either a string or a procedure.  If it is a
  ;; procedure, it is converted into a promise to be evaluated later.
  (let* ((val&priority (hash-ref %makevars key))
         (old-val (if (pair? val&priority) (cdr val&priority) #f))
         (old-priority (if (pair? val&priority) (cdr val&priority) #f)))
    (if (or (not old-val)
            (override? old-priority new-priority))
        (if (procedure? new-val)
            (hash-set! %makevars key (cons (delay new-val) new-priority))
            (hash-set! %makevars key (cons new-val new-priority)))))
  *unspecified*)

(define (makevars-add-keyvals keyvals)
  "Adds any suitable macros passed in from the command line, which
here are expected to be a list of key / val string pairs."
  (for-each
   (lambda (entry)
     (let ((key (car entry))
           (val (cdr entry)))
       (unless (or (string=? key "SHELL")
                   (string=? key "MAKEFLAGS"))
         (makevars-set key val 2))))
   keyvals))

(define (makevars-add-makeflags)
  "Adds any suitable environment variables found in the MAKEFLAGS
environment variable to the macro store"
  (let ((makeflags (getenv "MAKEFLAGS")))
    (when makeflags
      (for-each
       (lambda (entry)
         (let* ((keyval (split-at-equals entry))
                (key (if keyval (car keyval) #f))
                (val (if keyval (cdr keyval) #f)))
           (unless (or (not (string? key))
                       (string=? key "SHELL")
                       (string=? key "MAKEFLAGS"))
             (makevars-set key val 3))))
       (string-split makeflags #\space)))))

(define (makevars-add-environment)
  "Adds any suitable environment variables to the macro store, but not
the value of MAKEFLAGS or SHELL."
  (for-each
   (lambda (entry)
     (let* ((keyval (split-at-equals entry))
            (key (if keyval (car keyval) #f))
            (val (if keyval (cdr keyval) #f)))
       (unless (or (string=? key "SHELL")
                   (string=? key "MAKEFLAGS"))
         (makevars-set key val 4))))
   (environ)))

(define (makevars-add-builtins)
  "Adds the default macros to the store"
  (for-each
   (lambda (keyval)
     (makevars-set (car keyval) (cdr keyval) 5))
   builtin-makevars))

(define (dump-makevars)
  "Write out a list of the current makevars."
  (when (not (zero? (hash-count (const #t) %makevars)))
    (display (underline))
    (display "Makevars")
    (display (default))
    (newline)
    (let ((keyvals
           (sort
            (hash-map->list cons %makevars)
            (lambda (a b)
              (string<? (car a) (car b))))))
      (for-each
       (lambda (keyval)
         (let ((key (car keyval))
               (val (cdr keyval)))
           (let ((keyval-string
                  (if (zero? (string-length (car val)))
                      (string-copy key)
                      (string-append key " " (right-arrow) " " (car val)))))
             ;; Replace any control characters in VAL, like newline or tab
             (set! keyval-string
               (string-fold
                (lambda (c str)
                  (string-append str
                                 (if (char<? c #\space)
                                     (C0 c)
                                     (string c))))
                  ""
                  keyval-string))
             ;; Truncate
             (if (> (string-length keyval-string) 60)
                 (if %ascii?
                     (set! keyval-string
                       (string-append (substring keyval-string 0 57) "..."))
                     (set! keyval-string
                       (string-append (substring keyval-string 0 59) "…"))))
             (let* ((space (make-string (- 64 (string-length keyval-string))
                                        #\space))
                    (priority (cdr val))
                    (source-string (list-ref '("unknown"
                                               "script"
                                               "command line"
                                               "MAKEFLAGS"
                                               "environment"
                                               "built-in")
                                             priority)))
               (display "  ")
               (display keyval-string)
               (display space)
               (display source-string)
               (newline)))))
           keyvals))))

(define (initialize-makevars keyvals
                             environment?
                             elevate-environment?
                             builtins?
                             verbose?
                             ascii?)
  (set! %elevate-environment? elevate-environment?)
  (set! %makevars (make-hash-table))
  (set! %verbose? verbose?)
  (set! %ascii? ascii?)
  (when builtins?
    (makevars-add-builtins))
  (when environment?
    (makevars-add-environment)
    (makevars-add-makeflags))
  (makevars-add-keyvals keyvals)
  (when %verbose?
    (dump-makevars)))

;; API
(define* (lazy-assign key #:optional (val ""))
  "This procedure sets an entry in the %makevars hash table.
KEY must be a string or a thunk that evaluates to a string. Likewise
VAL.
    If KEY is a thunk, it is immediately evaluated to a string to use as
the key in the hash table entry.
    If VAL is a thunk, it is stored as a *promise* to be evaluated
later. The promise will be evaluated the first time this key is
referenced.
    If VAL is not given, the empty string will be used."
  (when (and (not (string? key))
             (not (procedure? key)))
    (bad-key-type "lazy-assign" (list key)))
  (when (and (not (string? val))
             (not (procedure? val)))
    (bad-value-type "lazy-assign" (list val)))
  (let ((KEY (if (string? key) key (key)))
        (VAL (if (string? val) val (delay val))))
    (unless (string? KEY)
      (bad-proc-output "lazy-assign" key))
    (makevars-set KEY VAL)
    (when (and %verbose? (string? VAL))
      (format #t "~A=~A~%" KEY VAL))))

(define-syntax ?=
  (lambda (stx)
    (syntax-case stx ()
      ((_ key val)
       #'(lazy-assign (symbol->string (syntax->datum #'key)) val))
      ((_ key)
       #'(lazy-assign (symbol->string (syntax->datum #'key)))))))

(define* (assign key #:optional (val ""))
  "This procedure sets an entry in the %makevars hash table.
KEY must be a string or a thunk that evaluates to a string. Likewise
VAL.
    If KEY and/or VAL is a thunk, it is immediately evaluated to a
string to use as the key in the hash table entry.
    If VAL is not given, the empty string will be used."
  (when (and (not (string? key))
             (not (procedure? key)))
    (bad-key-type "assign" (list key)))
  (when (and (not (string? val))
             (not (procedure? val)))
    (bad-value-type "assign" (list val)))
  (let ((KEY (if (string? key) key (key)))
        (VAL (if (string? val) val (val))))
    (unless (string? KEY)
      (bad-proc-output "assign" KEY))
    (unless (string? VAL)
      (bad-proc-output "assign" VAL))
    (makevars-set KEY VAL)
    (when %verbose?
      (format #t "~A=~A~%" KEY VAL))))

(define-syntax :=
  (lambda (stx)
    (syntax-case stx ()
      ((_ key val)
       #'(assign (symbol->string (syntax->datum #'key)) val))
      ((_ key)
       #'(assign (symbol->string (syntax->datum #'key)))))))


(define* (reference key #:optional (transformer #f))
  "Looks up KEY in the %makevars hash table. KEY may be a string
or a procedure that evaluates to a string.
     If the value of the key
in the hash table was a *promise* it will be forced,
evaluated, and set to that result.
     If no transformer is supplied, the looked up value will be
returned. 
     TRANSFORMER, if
supplied, should be a procedure of one string argument that returns a
string. If a transformer is supplied, it will be applied to every
space-separated token in the looked-up value."
  (when (and (not (string? key))
             (not (procedure? key)))
    (bad-key-type "reference" (list key)))
  (when (procedure? key)
    (set! key (key))
    (unless (string? key)
      (bad-proc-output "reference" key)))
  (let* ((val&priority (hash-ref %makevars key))
         (val (if (pair? val&priority) (car val&priority) #f))
         (priority (if (pair? val&priority) (cdr val&priority) #f)))
    (if (not val)
        #f
        ;; else
        (begin
          (when (promise? val)
            (set! val (force val))
            (unless (string? val)
              (bad-proc-output "reference" val))
            (hash-set! %makevars key (cons val priority))
            (when %verbose?
              (format #t "~A=~A~%" key val)))
          (when (procedure? transformer)
            (set! val (string-append-with-spaces
                       (map transformer
                            (string-tokenize val)))))
          val))))

(define-syntax $
  (lambda (stx)
    (syntax-case stx ()
      ((_ key val)
       #'(reference (symbol->string (syntax->datum #'key)) transformer))
      ((_ key)
       #'(reference (symbol->string (syntax->datum #'key)))))))

(define (reference-func key)
  "Looks up KEY in the %makevars hash table. KEY shall be a string
or a procedure that evaluates to a string.
    If the value of the key
in the hash table was a *promise*, a procedure will be returned that,
when called, will call that promise.  If the value of the key is
a string, a procedure will be returned that, when called, returns
that string."
  (when (and (not (string? key))
             (not (procedure? key)))
    (bad-key-type "reference" (list key)))
  (when (procedure? key)
    (set! key (key))
    (unless (string? key)
      (bad-proc-output "reference" key))
  (let* ((val&priority (hash-ref %makevars key))
         (val (if (pair? val&priority) (cdr val&priority) #f)))
    (if (not val)
        #f
        ;; else
        (begin
          (if (promise? val)
              (lambda ()
                (let ((VAL (force val)))
                  ;; FIXME: put verbose print here?
                  VAL))
            ;; else
            (lambda ()
              val)))))))

(define-syntax $$
  (lambda (stx)
    (syntax-case stx ()
      ((_ key)
       #'(reference-func (symbol->string (syntax->datum #'key)))))))
