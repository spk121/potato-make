#!/usr/bin/env sh
exec guile -s "$0" "$@"
!#

;;; Builds a small C program whose sources include a file with a
;;; space in its name.  Run it from this directory:
;;;
;;;     ./makefile.scm          build the "hello" program
;;;     ./makefile.scm clean    remove the build products

(use-modules (potato make)
             (potato lib))
(initialize)

(:= CC "cc")
(:= CFLAGS "-g -O2")
(:= PROGRAM "hello")
;; Quotes group a name that contains a space into a single word.
(:= SOURCES "main.c foo.c 'alpha bravo.c'")

;; The W syntax splits a makevar into a list of words, honoring the
;; quotes: ("main.o" "foo.o" "alpha bravo.o")
(define objects
  (map (lambda (source)
         (patsubst "%.c" "%.o" (string-append "'" source "'")))
       (W SOURCES)))

(define (quoted . names)
  "Wrap each of NAMES in double quotes, so that the shell treats a
name that contains spaces as a single argument."
  (string-join (map (lambda (name) (string-append "\"" name "\""))
                    names)
               " "))

(: "all" (list ($ PROGRAM)))

(: ($ PROGRAM) objects
   (~ ($ CC) ($ CFLAGS) "-o" $@ (lambda () (apply quoted ($$^)))))

(-> ".c" ".o"
    (~ ($ CC) ($ CFLAGS) "-c" "-o"
       (lambda () (quoted ($@)))
       (lambda () (quoted ($<)))))

(: "clean" '()
   (~ "rm -f" ($ PROGRAM) (apply quoted objects)))

(execute)
