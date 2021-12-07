#!/usr/bin/env -S guile -s "$0" "$@"

(use-modules (potato make))
(initialize)
(:= CC "gcc")
(:= CFLAGS "-g -O2")

(: "all" '("foo"))
(: "foo" '("foo.o" "bar.o")
   (~ ($ CC) "-o" $@ $^))
(-> ".c" ".o"
    (~ ($ CC) "-c" $<))
(execute)
