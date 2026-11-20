;;; build.scm -- part of the Potato Make test suite.
;;;
;;; These tests run builds against real files in a scratch directory,
;;; to exercise the timestamp logic and the dependency tree.

(use-modules (potato make)
             (srfi srfi-1)
             (srfi srfi-64)
             (tests common))

(test-begin "build")

(define quiet '("test" "--verbosity=0"))

(define %real-system (install-alternate-system-driver stub-system-pass))

(define (use-real-system!)
  (install-alternate-system-driver %real-system))

(define (use-stub-system!)
  (reset-commands!)
  (install-alternate-system-driver stub-system-pass))

;; Timestamps, in seconds since the epoch.
(define old 1000000000)
(define mid 1500000000)
(define new 1700000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Real shell commands

(use-real-system!)

(test-assert "A shell command recipe that succeeds passes."
  (begin
    (initialize quiet)
    (: "all" '() "true")
    (execute)))

(test-assert "A shell command recipe that fails makes the target fail."
  (begin
    (initialize quiet)
    (: "all" '() "false")
    (not (execute))))

(test-assert "A shell command recipe creates its target file."
  (in-temp-dir
   (touch "in.txt" old "hello")
   (initialize quiet)
   (: "out.txt" '("in.txt")
      (~ "cp" $< $@))
   (and (execute)
        (file-exists? "out.txt"))))

(test-equal "Recipes stop at the first failing command."
  '(#f #f)
  (in-temp-dir
   (initialize quiet)
   (: "all" '()
      "false"
      "touch should-not-exist")
   (let ((result (execute)))
     (list result (file-exists? "should-not-exist")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timestamps

(use-stub-system!)

(test-equal "A target that does not exist is built."
  '(#t ("build out"))
  (in-temp-dir
   (use-stub-system!)
   (touch "in" old)
   (initialize quiet)
   (: "out" '("in") "build out")
   (let ((result (execute)))
     (list result (commands)))))

(test-equal "A target newer than its prerequisites is not rebuilt."
  '(#t ())
  (in-temp-dir
   (use-stub-system!)
   (touch "in" old)
   (touch "out" new)
   (initialize quiet)
   (: "out" '("in") "build out")
   (let ((result (execute)))
     (list result (commands)))))

(test-equal "A target older than a prerequisite is rebuilt."
  '(#t ("build out"))
  (in-temp-dir
   (use-stub-system!)
   (touch "in" new)
   (touch "out" old)
   (initialize quiet)
   (: "out" '("in") "build out")
   (let ((result (execute)))
     (list result (commands)))))

(test-equal "A target older than any one of its prerequisites is rebuilt."
  '(#t ("build out"))
  (in-temp-dir
   (use-stub-system!)
   (touch "in1" old)
   (touch "in2" new)
   (touch "out" mid)
   (initialize quiet)
   (: "out" '("in1" "in2") "build out")
   (let ((result (execute)))
     (list result (commands)))))

(test-equal "A target with no prerequisites is always built."
  '(#t ("build out"))
  (in-temp-dir
   (use-stub-system!)
   (touch "out" new)
   (initialize quiet)
   (: "out" '() "build out")
   (let ((result (execute)))
     (list result (commands)))))

(test-equal "A missing prerequisite without a rule fails the target."
  '(#f ())
  (in-temp-dir
   (use-stub-system!)
   (initialize quiet)
   (: "out" '("missing") "build out")
   (let ((result (execute)))
     (list result (commands)))))

(test-equal "$? lists only the prerequisites newer than the target."
  '("in2" ("in2"))
  (in-temp-dir
   (touch "in1" old)
   (touch "in2" new)
   (touch "out" mid)
   (initialize quiet)
   (let ((seen #f))
     (: "out" '("in1" "in2")
        (lambda ()
          (set! seen (list ($?) ($$?)))
          #t))
     (execute)
     seen)))

(test-equal "Prerequisites are built before the target, in order."
  '("build a.o" "build b.o" "link prog")
  (in-temp-dir
   (use-stub-system!)
   (touch "a.c" old)
   (touch "b.c" old)
   (initialize quiet)
   (: "prog" '("a.o" "b.o") "link prog")
   (: "a.o" '("a.c") "build a.o")
   (: "b.o" '("b.c") "build b.o")
   (execute)
   (commands)))

(test-equal "Only the out-of-date branch of the tree is rebuilt."
  '("build b.o" "link prog")
  (in-temp-dir
   (use-stub-system!)
   (touch "a.c" old)
   (touch "a.o" mid)
   (touch "b.c" new)
   (touch "b.o" mid)
   (touch "prog" mid)
   (initialize quiet)
   (: "prog" '("a.o" "b.o") "link prog")
   (: "a.o" '("a.c") "build a.o")
   (: "b.o" '("b.c") "build b.o")
   ;; Simulate the effect of the compile step.
   (install-alternate-system-driver
    (lambda (cmd)
      (when (string=? cmd "build b.o")
        (touch "b.o" (+ new 10)))
      (stub-system-pass cmd)))
   (execute)
   (commands)))

(test-equal "A failed prerequisite stops the target from being built."
  '(#f ("build a.o"))
  (in-temp-dir
   (reset-commands!)
   (install-alternate-system-driver stub-system-fail)
   (touch "a.c" old)
   (initialize quiet)
   (: "prog" '("a.o") "link prog")
   (: "a.o" '("a.c") "build a.o")
   (let ((result (execute)))
     (list result (commands)))))

(test-error "A circular dependency raises an error."
  #t
  (begin
    (initialize quiet)
    (: "a" '("b") #t)
    (: "b" '("a") #t)
    (execute)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme procedure recipes

(test-assert "A procedure recipe returning zero passes."
  (begin
    (initialize quiet)
    (: "all" '() (lambda () 0))
    (execute)))

(test-assert "A procedure recipe returning a non-zero integer fails."
  (begin
    (initialize quiet)
    (: "all" '() (lambda () 2))
    (not (execute))))

(test-assert "A procedure recipe returning an unspecified value passes."
  (begin
    (initialize quiet)
    (: "all" '() (lambda () (if #f #f)))
    (execute)))

(test-assert "A recipe of #f always fails."
  (begin
    (initialize quiet)
    (: "all" '() #f)
    (not (execute))))

(test-equal "Recipes run in order."
  '(1 2 3)
  (let ((seen '()))
    (initialize quiet)
    (: "all" '()
       (lambda () (set! seen (cons 1 seen)))
       (lambda () (set! seen (cons 2 seen)))
       (lambda () (set! seen (cons 3 seen))))
    (execute)
    (reverse seen)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suffix rules against real files

(test-equal "A suffix rule builds from an existing source file."
  '(#t ("cc -c foo.c"))
  (in-temp-dir
   (use-stub-system!)
   (touch "foo.c" old)
   (initialize quiet)
   (: "all" '("foo.o"))
   (-> ".c" ".o" (~ "cc -c" $<))
   (let ((result (execute)))
     (list result (commands)))))

(test-equal "A suffix rule does not rebuild an up-to-date target."
  '(#t ())
  (in-temp-dir
   (use-stub-system!)
   (touch "foo.c" old)
   (touch "foo.o" new)
   (initialize quiet)
   (: "all" '("foo.o"))
   (-> ".c" ".o" (~ "cc -c" $<))
   (let ((result (execute)))
     (list result (commands)))))

(test-equal "A suffix rule rebuilds a target older than its source."
  '(#t ("cc -c foo.c"))
  (in-temp-dir
   (use-stub-system!)
   (touch "foo.c" new)
   (touch "foo.o" old)
   (initialize quiet)
   (: "all" '("foo.o"))
   (-> ".c" ".o" (~ "cc -c" $<))
   (let ((result (execute)))
     (list result (commands)))))

(test-equal "A suffix rule fails when there is no source file."
  '(#f ())
  (in-temp-dir
   (use-stub-system!)
   (initialize quiet)
   (: "all" '("foo.o"))
   (-> ".c" ".o" (~ "cc -c" $<))
   (let ((result (execute)))
     (list result (commands)))))

(test-equal "Suffix rules work when there are no target rules at all."
  '(#t ("cc -c foo.c"))
  (in-temp-dir
   (use-stub-system!)
   (touch "foo.c" old)
   (initialize quiet)
   (-> ".c" ".o" (~ "cc -c" $<))
   (let ((result (execute #:targets '("foo.o"))))
     (list result (commands)))))

(test-equal "Suffix rules chain through an intermediate file."
  '(#t ("yacc foo.y" "cc -c foo.c"))
  (in-temp-dir
   (use-stub-system!)
   (touch "foo.y" old)
   (initialize quiet)
   (: "all" '("foo.o"))
   (-> ".y" ".c" (~ "yacc" $<))
   (-> ".c" ".o" (~ "cc -c" $<))
   (let ((result (execute)))
     (list result (commands)))))

(test-equal "An existing file is usable even if a suffix rule could, in
principle, make it from a source that does not exist."
  '(#t ("cc -c foo.c"))
  (in-temp-dir
   (use-stub-system!)
   ;; foo.c is hand-written: there is no foo.y to generate it from.
   (touch "foo.c" old)
   (initialize quiet)
   (: "all" '("foo.o"))
   (-> ".y" ".c" (~ "yacc" $<))
   (-> ".c" ".o" (~ "cc -c" $<))
   (let ((result (execute)))
     (list result (commands)))))

(test-equal "The built-in rules compile a C file."
  '(#t ("cc -g -O2 -c foo.c"))
  (in-temp-dir
   (use-stub-system!)
   (touch "foo.c" old)
   (initialize '("test" "--verbosity=0" "--builtins"))
   (: "all" '("foo.o"))
   (let ((result (execute)))
     (list result (commands)))))

(test-equal "The built-in rules compile a Fortran file."
  '(#t ("gfortran -O1 -c foo.f"))
  (in-temp-dir
   (use-stub-system!)
   (touch "foo.f" old)
   (initialize '("test" "--verbosity=0" "--builtins"))
   (: "all" '("foo.o"))
   (let ((result (execute)))
     (list result (commands)))))

(test-equal "The built-in rules turn a lex file into C using LFLAGS."
  '(#t ("lex -X foo.l" "mv lex.yy.c foo.c"))
  (in-temp-dir
   (use-stub-system!)
   (touch "foo.l" old)
   (initialize '("test" "--verbosity=0" "--builtins"
                 "LFLAGS=-X" "LDFLAGS=-WRONG"))
   (: "all" '("foo.c"))
   (let ((result (execute)))
     (list result (commands)))))

(test-equal "A script-defined makevar overrides a built-in in a built-in rule."
  '("clang -g -O2 -c foo.c")
  (in-temp-dir
   (use-stub-system!)
   (touch "foo.c" old)
   (initialize '("test" "--verbosity=0" "--builtins"))
   (:= CC "clang")
   (: "all" '("foo.o"))
   (execute)
   (commands)))

(test-equal "A suffix rule with a multi-part target suffix defines $<, $* and $@."
  '("a.tar" "a" "a.tar.gz")
  (in-temp-dir
   (touch "a.tar" old)
   (initialize quiet)
   (let ((seen #f))
     (: "all" '("a.tar.gz"))
     (-> ".tar" ".tar.gz"
         (lambda ()
           (set! seen (list ($<) ($*) ($@)))
           #t))
     (execute)
     seen)))

(test-equal "A suffix rule with an empty target suffix keeps dots in the name."
  '("my.prog.sh" "my.prog" "my.prog")
  (in-temp-dir
   (touch "my.prog.sh" old)
   (initialize quiet)
   (let ((seen #f))
     (: "all" '("my.prog"))
     (-> ".sh" ""
         (lambda ()
           (set! seen (list ($<) ($*) ($@)))
           #t))
     (execute)
     seen)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output

(define (build-with-arguments . args)
  "Run a tiny build with the given command-line arguments and return
what it printed."
  (use-stub-system!)
  (output-of
   (initialize (cons "test" args))
   (?= LAZY (lambda () "lazy value"))
   (:= EAGER "eager value")
   (: "all" '("part"))
   (: "part" '()
      (~ "echo" ($ EAGER) ($ LAZY))
      (~@ "echo silent"))
   (execute)))

(test-equal "Verbosity 0 prints nothing."
  ""
  (build-with-arguments "--verbosity=0"))

(test-assert "Verbosity 1 prints the target but not the commands."
  (let ((out (build-with-arguments "--verbosity=1")))
    (and (string-contains out "part")
         (not (string-contains out "echo")))))

(test-assert "Verbosity 2 prints commands, except silent ones."
  (let ((out (build-with-arguments "--verbosity=2")))
    (and (string-contains out "echo eager value lazy value")
         (not (string-contains out "echo silent")))))

(test-assert "Verbosity 3 prints everything, including silent commands."
  (let ((out (build-with-arguments "--verbosity=3")))
    (and (string-contains out "echo eager value lazy value")
         (string-contains out "echo silent")
         (string-contains out "Target rule")
         (string-contains out "LAZY"))))

(test-assert "ASCII mode output contains only ASCII characters."
  (let ((out (build-with-arguments "--verbosity=3" "--ascii")))
    (string-every (lambda (c)
                    (or (char=? c #\newline)
                        (and (char>=? c #\space) (char<? c #\delete))))
                  out)))

(test-assert "--no-execution prints the commands that would have been run."
  (let ((out (build-with-arguments "--no-execution")))
    (and (null? (commands))
         (string-contains out "echo eager value lazy value")
         (not (string-contains out "echo silent")))))

(test-exit "build")
