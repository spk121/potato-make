#!/usr/bin/env sh
exec guile -L . -s "$0" "$@"
!#

(use-modules (studious-potato)
             (srfi srfi-1))


(initialize #:arguments (command-line))
(write %opt-verbose) (newline)
(write %opt-quiet) (newline)

