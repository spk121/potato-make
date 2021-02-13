#!/usr/bin/env sh
exec guile -L . -s "$0" "$@"
!#

(use-modules (potato make)
             (srfi srfi-1)
             (srfi srfi-64))


;; These stubs override the driver that calls
;; 'system' so we can instead just investigate
;; what string it was passed.
(define %cmd #f)
(define (stub-system-pass cmd)
  (set! %cmd cmd)
  0)
(define (stub-system-fail cmd)
  (set! %cmd cmd)
  1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKEVARS

(test-begin "makevars")

(test-equal "can set makevar with environment"
  "BAR1"
  (begin
    (setenv "FOO1" "BAR1")
    (initialize '("test" "--environment"))
    (let ((result ($ FOO1)))
      (unsetenv "FOO1")
      result)))

(test-equal "can set makevar with MAKEFLAGS"
  "BAR2"
  (begin
    (setenv "MAKEFLAGS" "FOO2=BAR2")
    (initialize '("test" "--environment"))
    (let ((result ($ FOO2)))
      (unsetenv "MAKEFLAGS")
      result)))

(test-equal "can set makevar with initialize"
  "BAR3"
  (begin
    (initialize '("test" "FOO3=BAR3"))
    ($ FOO3)))

(test-equal "can set makevar in script"
  "BAR4"
  (begin
    (:= FOO4 "BAR4")
    ($ FOO4)))

(test-equal "can set makevar lazily in script"
  "BAR5"
  (begin
    (?= FOO5 "BAR5")
    ($ FOO5)))

(test-assert "a lazy makevar of a procedure is a promise before it is referenced"
  (begin
    (?= FOO6 (lambda () "BAR6"))
    (let ((val (hash-ref (@@ (potato makevars) %makevars) "FOO6")))
      (promise? (car val)))))

(test-equal "a lazy makevar of a procedure is a string after it is referenced"
  "BAR7"
  (begin
    (?= FOO7 (lambda () "BAR7"))
    ($ FOO7)
    (let ((val (hash-ref (@@ (potato makevars) %makevars) "FOO7")))
      (car val))))

(test-equal "referencing an unset makevar returns an empty string"
  ""
  ($ FOO8))

(test-error "referencing an unset makevar throws an error in strict mode"
  #t
  (begin
    (initialize '("test" "--strict"))
    ($ FOO9)))

(test-equal "assign converts integers to strings"
  "100"
  (begin
    (:= FOO10 100)
    ($ FOO10)))

(test-equal "assign converts characters to strings"
  "x"
  (begin
    (:= FOO11 #\x)
    ($ FOO11)))

(test-equal "quote-reference adds quotation marks"
  "\"BAR 12\""
  (begin
    (:= FOO12 "BAR 12")
    (Q FOO12)))

(test-equal "quote-reference of an unassigned makevar returns empty quotation marks in non-strict mode"
  "\"\""
  (begin
    (initialize '("test"))
    (Q FOO13)))

(test-error "quote-reference of an unassigned makevar throws an error in strict mode"
  #t
  (begin
    (initialize '("test" "--strict"))
    (Q FOO13)))

(test-equal "script assignment overrides command-line assignment"
  "BAZ14"
  (begin
    (initialize '("test" "FOO14=BAR14"))
    (:= FOO14 "BAZ14")
    ($ FOO14)))

(test-equal "script assignment overrides MAKEFLAGS assignment"
  "BAZ15"
  (begin
    (setenv "MAKEFLAGS" "FOO15=BAR15")
    (initialize '("test" "--environment"))
    (:= FOO15 "BAZ15")
    ($ FOO15)))

(test-equal "script assignment overrides environment assignment"
  "BAZ16"
  (begin
    (setenv "FOO16" "BAR16")
    (initialize '("test" "--environment"))
    (unsetenv "FOO16")
    (:= FOO16 "BAZ16")
    ($ FOO16)))

(test-equal "command-line assignment overrides script assignment in elevate mode"
  "BAR14"
  (begin
    (initialize '("test" "FOO14=BAR14" "--elevate-environment"))
    (:= FOO14 "BAZ14")
    ($ FOO14)))

(test-equal "MAKEFLAGS assignment overrides script assignment in elevate mode"
  "BAR15"
  (begin
    (setenv "MAKEFLAGS" "FOO15=BAR15")
    (initialize '("test" "--elevate-environment"))
    (unsetenv "MAKEFLAGS")
    (:= FOO15 "BAZ15")
    ($ FOO15)))

(test-equal "environment assignment overrides script assignment in elevate mode"
  "BAR16"
  (begin
    (setenv "FOO16" "BAR16")
    (initialize '("test" "--elevate-environment"))
    (:= FOO16 "BAZ16")
    ($ FOO16)))


(test-end "makevars")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RECIPE HELPERS

(test-begin "recipe_helpers")

(test-assert "string-compose returns 'default and procedure"
  (let ((ret (~ "hello")))
    (and
     (eq? (car ret) 'default)
     (procedure? (cdr ret)))))

(test-assert "silent-compose returns 'silent and procedure"
  (let ((ret (~@ "hello")))
    (and
     (eq? (car ret) 'silent)
     (procedure? (cdr ret)))))

(test-assert "always-execute-compose returns 'always-execute and procedure"
  (let ((ret (~+ "hello")))
    (and
     (eq? (car ret) 'always-execute)
     (procedure? (cdr ret)))))

(test-assert "ignore-error-compose returns 'ignore-error and procedure"
  (let ((ret (~- "hello")))
    (and
     (eq? (car ret) 'ignore-error)
     (procedure? (cdr ret)))))

(test-equal "string-compose string passthrough"
  "hello"
  (let ((ret (~ "hello")))
    ((cdr ret))))

(test-equal "string-compose two strings passthrough"
  "hello world"
  (let ((ret (~ "hello" "world")))
    ((cdr ret))))

(test-equal "string-compose empty initial string"
  "world"
  (let ((ret (~ "" "world")))
    ((cdr ret))))

(test-equal "string-compose empty terminal string"
  "hello"
  (let ((ret (~ "hello" "")))
    ((cdr ret))))

(test-equal "string-compose empty medial string"
  "hello world"
  (let ((ret (~ "hello" "" "world")))
    ((cdr ret))))

(test-equal "string-compose handles procedure"
  "hello world"
  (let ((ret (~ "hello" (lambda () "world"))))
    ((cdr ret))))

(test-equal "string-compose handles integer"
  "hello 123"
  (let ((ret (~ "hello" 123)))
    ((cdr ret))))

(test-equal "string-compose handles character"
  "hello w"
  (let ((ret (~ "hello" #\w)))
    ((cdr ret))))

(test-equal "string-compose handles makevar"
  "hello BAR"
  (begin
    (:= FOO "BAR")
    (let ((ret (~ "hello" ($ FOO))))
      ((cdr ret)))))

(test-equal "empty string-compose"
  ""
  (let ((ret (~)))
    ((cdr ret))))

(test-end "recipe_helpers")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TARGET RULES

(test-begin "target_rules")

(test-assert "install alternate system driver"
  (false-if-exception (install-alternate-system-driver stub-system-pass)))

(test-equal "target rule is a string"
  "cc -o foo.exe foo.c"
  (begin
    (initialize '("test" "foo.exe"))
    (: "foo.exe" '("foo.c")
       "cc -o foo.exe foo.c")
    (execute)
    %cmd))

(test-assert "target rule is a procedure"
  (begin
    (let ((tmpvar #f))
      (initialize '("test" "foo.exe"))
      (: "foo.exe" '("foo.c")
         (lambda ()
           (set! tmpvar #t)))
      (execute)
      tmpvar)))

(test-equal "target rule is a procedure returning a string"
  "cc -o foo.exe foo.c"
  (begin
    (initialize '("test" "foo.exe"))
    (: "foo.exe" '("foo.c")
       (lambda ()
         ("cc -o foo.exe foo.c")))
    (execute)
    %cmd))

(test-equal "target rule using string-compose on a string"
  "cc -o foo.exe foo.c"
  (begin
    (initialize '("test" "foo.exe"))
    (: "foo.exe" '("foo.c")
       (~ "cc -o foo.exe foo.c"))
    (execute)
    %cmd))

(test-equal "target rule using string-compose on special variables"
  "cc -o foo.exe foo.c"
  (begin
    (initialize '("test" "foo.exe"))
    (: "foo.exe" '("foo.c")
       (~ "cc -o" $@ $<))
    (execute)
    %cmd))

(test-equal "target rule check success"
  #t
  (begin
    (initialize '("test" "foo.exe"))
    (: "foo.exe" '("foo.c")
       (~ "cc -o" $@ $<))
    (execute)))

(test-assert "install failing alternate system driver"
  (false-if-exception (install-alternate-system-driver stub-system-fail)))

(test-equal "target rule check failure of system call"
  #f
  (begin
    (initialize '("test" "foo.exe"))
    (: "foo.exe" '("foo.c")
       (~ "cc -o" $@ $<))
    (execute)))

(test-equal "target rule check failure of scheme procedure"
  #f
  (begin
    (initialize '("test" "foo.exe"))
    (: "foo.exe" '("foo.c")
       (lambda ()
         #f))
    (execute)))

(test-end "target_rules")

(test-begin "suffix_rules")

(test-assert "install alternate system driver"
  (false-if-exception (install-alternate-system-driver stub-system-pass)))

(test-equal "suffix rule simple"
  "cc -c foo.c"
  (begin
    (initialize '("test" "foo.o"))
    (-> ".c" ".o"
        (~ "cc -c" $<))
    (execute)
    %cmd))

(test-end "suffix_rules")
