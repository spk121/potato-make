;;; makevars.scm -- part of the Potato Make test suite.

(use-modules (potato make)
             (srfi srfi-1)
             (srfi srfi-64)
             (tests common))

(test-begin "makevars")

(test-equal "Makevars can be set in the environment with the --environment flag."
  "BAR1"
  (begin
    (setenv "FOO1" "BAR1")
    (initialize '("test" "--environment"))
    (let ((result ($ FOO1)))
      (unsetenv "FOO1")
      result)))

(test-equal "Makevars can be set in the MAKEFLAGS environment variable with the --environment flag."
  "BAR2"
  (begin
    (setenv "MAKEFLAGS" "FOO2=BAR2")
    (initialize '("test" "--environment"))
    (let ((result ($ FOO2)))
      (unsetenv "MAKEFLAGS")
      result)))

(test-equal "Makevars can be set with initialize."
  "BAR3"
  (begin
    (initialize '("test" "FOO3=BAR3"))
    ($ FOO3)))

(test-equal "Makevars can be set in the script."
  "BAR4"
  (begin
    (:= FOO4 "BAR4")
    ($ FOO4)))

(test-equal "Quoted makevar reference wraps value in quotes."
  "\"BAR4\""
  (begin
    (:= FOO4 "BAR4")
    (Q FOO4)))

(test-equal "Makevars can be set lazily in the script."
  "BAR5"
  (begin
    (?= FOO5 "BAR5")
    ($ FOO5)))

(test-assert "A lazy makevar of a procedure is a promise before it is referenced."
  (begin
    (?= FOO6 (lambda () "BAR6"))
    (let ((val (hash-ref (@@ (potato makevars) %makevars) "FOO6")))
      (promise? (car val)))))

(test-equal "A lazy makevar of a procedure is a string after it is referenced."
  "BAR7"
  (begin
    (?= FOO7 (lambda () "BAR7"))
    ($ FOO7)
    (let ((val (hash-ref (@@ (potato makevars) %makevars) "FOO7")))
      (car val))))

(test-equal "Transformer applies to each token in value."
  "AAA BBB"
  (begin
    (:= FOO7X "aaa bbb")
    ($ FOO7X string-upcase)))

(test-equal "Quoted transformer applies and preserves quotes around full value."
  "\"AAA BBB\""
  (begin
    (:= FOO7Y "aaa bbb")
    (Q FOO7Y string-upcase)))

(test-equal "Referencing an unset makevar returns an empty string."
  ""
  ($ FOO8))

(test-error "Referencing an unset makevar throws an error in strict mode."
  #t
  (begin
    (initialize '("test" "--strict"))
    ($ FOO9)))

(test-equal "Assigning an integer to a makevar converts it into a string."
  "100"
  (begin
    (:= FOO10 100)
    ($ FOO10)))

(test-equal "Assigning a character to a makevar converts it into a string."
  "x"
  (begin
    (:= FOO11 #\x)
    ($ FOO11)))

(test-assert "$$ returns a thunk for existing key and yields its value."
  (begin
    (:= FOO11B "BAR11B")
    (let ((f ($$ FOO11B)))
      (and (procedure? f)
           (string=? (f) "BAR11B")))))

(test-equal "$$ returns #f for missing key."
  #f
  ($$ FOO11C))

(test-assert "$$ forces lazy promise on demand."
  (begin
    (?= FOO11D "BAR11D")
    (let ((f ($$ FOO11D)))
      (and (procedure? f)
           (string=? (f) "BAR11D")))))

(test-equal "SHELL from environment is ignored."
  ""
  (begin
    (setenv "SHELL" "IGNORE_ME")
    (initialize '("test" "--environment"))
    ($ SHELL)))

(test-equal "MAKEFLAGS key itself is ignored as makevar."
  ""
  (begin
    (setenv "MAKEFLAGS" "MAKEFLAGS=IGNORE_THIS")
    (initialize '("test" "--environment"))
    (unsetenv "MAKEFLAGS")
    ($ MAKEFLAGS)))

(test-equal "Makevar script assignment overrides command-line assignment."
  "BAZ14"
  (begin
    (initialize '("test" "FOO14=BAR14"))
    (:= FOO14 "BAZ14")
    ($ FOO14)))

(test-equal "Makevar script assignment overrides MAKEFLAGS assignment."
  "BAZ15"
  (begin
    (setenv "MAKEFLAGS" "FOO15=BAR15")
    (initialize '("test" "--environment"))
    (:= FOO15 "BAZ15")
    ($ FOO15)))

(test-equal "Makevar script assignment overrides environment assignment."
  "BAZ16"
  (begin
    (setenv "FOO16" "BAR16")
    (initialize '("test" "--environment"))
    (unsetenv "FOO16")
    (:= FOO16 "BAZ16")
    ($ FOO16)))

(test-equal "Makevar command-line assignment overrides script assignment in elevate mode."
  "BAR14"
  (begin
    (initialize '("test" "FOO14=BAR14" "--elevate-environment"))
    (:= FOO14 "BAZ14")
    ($ FOO14)))

(test-equal "MAKEFLAGS assignment overrides script assignment in elevate mode."
  "BAR15"
  (begin
    (setenv "MAKEFLAGS" "FOO15=BAR15")
    (initialize '("test" "--elevate-environment"))
    (unsetenv "MAKEFLAGS")
    (:= FOO15 "BAZ15")
    ($ FOO15)))

(test-equal "Environment assignment overrides script assignment in elevate mode."
  "BAR16"
  (begin
    (setenv "FOO16" "BAR16")
    (initialize '("test" "--elevate-environment"))
    (unsetenv "FOO16")
    (:= FOO16 "BAZ16")
    ($ FOO16)))

(test-equal "Builtins are available with --builtins."
  "cc"
  (begin
    (initialize '("test" "--builtins"))
    ($ CC)))

(test-equal "A transformer applied to an empty makevar gives an empty string."
  ""
  (begin
    (initialize '("test"))
    (:= EMPTY1 "")
    ($ EMPTY1 string-upcase)))

(test-equal "A makevar with no value is the empty string."
  ""
  (begin
    (:= EMPTY2)
    ($ EMPTY2)))

(test-equal "A lazy makevar's procedure is not called until it is referenced."
  '(0 "BAR" 1 "BAR" 1)
  (let ((calls 0))
    (?= LAZY1 (lambda () (set! calls (1+ calls)) "BAR"))
    (let* ((before calls)
           (first ($ LAZY1))
           (middle calls)
           (second ($ LAZY1)))
      (list before first middle second calls))))

(test-equal "An eager makevar's procedure is called at once."
  '(1 "BAR")
  (let ((calls 0))
    (:= EAGER1 (lambda () (set! calls (1+ calls)) "BAR"))
    (list calls ($ EAGER1))))

(test-equal "$$ of a lazy procedure makevar yields its string value."
  "BAR"
  (begin
    (?= LAZY2 (lambda () "BAR"))
    (($$ LAZY2))))

(test-assert "Verbose mode can print makevars, including lazy ones."
  (let ((out (with-output-to-string
               (lambda ()
                 (initialize '("test" "--verbosity=3" "--ascii"))
                 (?= LAZY3 (lambda () "lazy value"))
                 (:= TABBED "a\tb")
                 (:= LONG (make-string 100 #\x))
                 ($ LAZY3)))))
    (initialize '("test"))
    (and (string-contains out "LAZY3")
         (string-contains out "lazy value")
         (string-contains out "a<HT>b")
         (string-contains out "xxx..."))))

(test-assert "dump-makevars lists the makevars and where they came from."
  (let ((out (with-output-to-string
               (lambda ()
                 (initialize '("test" "--ascii" "FROMCLI=1"))
                 (:= FROMSCRIPT "2")
                 (?= FROMLAZY (lambda () "3"))
                 ((@ (potato makevars) dump-makevars))))))
    (initialize '("test"))
    (and (string-contains out "FROMCLI -> 1")
         (string-contains out "command line")
         (string-contains out "FROMSCRIPT -> 2")
         (string-contains out "script")
         (string-contains out "FROMLAZY"))))

(test-error "A key that is neither a string nor a procedure is an error."
  #t
  (reference 42 #f))

(test-equal "A command-line makevar overrides the environment."
  "CLI"
  (begin
    (setenv "FOO17" "ENV")
    (initialize '("test" "--environment" "FOO17=CLI"))
    (unsetenv "FOO17")
    ($ FOO17)))

(test-equal "A MAKEFLAGS makevar overrides the environment."
  "FLAGS"
  (begin
    (setenv "FOO18" "ENV")
    (setenv "MAKEFLAGS" "FOO18=FLAGS")
    (initialize '("test" "--environment"))
    (unsetenv "FOO18")
    (unsetenv "MAKEFLAGS")
    ($ FOO18)))

(test-equal "The environment overrides a built-in."
  "ENVCC"
  (begin
    (setenv "CC" "ENVCC")
    (initialize '("test" "--environment" "--builtins"))
    (unsetenv "CC")
    ($ CC)))

(test-equal "The environment is not imported without --environment."
  ""
  (begin
    (setenv "FOO19" "ENV")
    (initialize '("test"))
    (unsetenv "FOO19")
    ($ FOO19)))

(test-equal "A script assignment still overrides a built-in in elevate mode."
  "SCRIPTCC"
  (begin
    (unsetenv "CC")
    (initialize '("test" "--elevate-environment" "--builtins"))
    (:= CC "SCRIPTCC")
    ($ CC)))

(test-exit "makevars")
