#!/usr/bin/env sh
exec guile -L . -s "$0" "$@"
!#

(use-modules (potato make)
             (srfi srfi-1))

(initialize '("test" "foo.exe" "--verbose"))
(: "foo.exe" '("foo.c")
   "cc -o foo.exe foo.c")
(execute)


