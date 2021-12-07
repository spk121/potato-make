#!/usr/bin/env sh
exec guile -s "$0" "$@"
!#

;;; Rule that outputs "Hello from potato-make!" when called as `./hello.scm hello`

(use-modules (potato make))
(initialize)

(: "hello")
  (~ (display "Hello from potato-make!\n"))

(execute)
