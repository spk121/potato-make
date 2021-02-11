#!/usr/bin/env sh
exec guile -L . -s "$0" "$@"
!#

(use-modules (potato make)
             (srfi srfi-1)
             (srfi srfi-64))

(test-begin "initialization")

(test-assert "initialize doesn't set verbose flag"
  (begin
    (initialize #:arguments '() #:environ #f)
    (not (%opt-verbose?))))

(test-end "initialization")

