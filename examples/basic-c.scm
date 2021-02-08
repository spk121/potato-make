#!/usr/bin/env sh
exec guile -s "$0" "$@"
!#

(use-modules (studious-potato))
(write (command-line)) (newline)
(initialize (command-line))

(write ($ CFLAGS)) (newline)
(write %suffix-rules) (newline)
;; We will rely on the built-in ".c" to ".o" rule.
(: "hello-world" '("hello-world.o")
   (~ ($ CC) "-o" $@ $<))
(: "hello-world.o" '("hello-world.c")
   (~ ($ CC) "-c" $<))

(execute)
