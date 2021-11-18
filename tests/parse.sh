#!/usr/bin/env sh
exec guile -L . -s "$0" "$@"
!#
(use-modules (potato parse)
             ;; (potato parse)
             (ice-9 receive)
             (srfi srfi-1)
             (srfi srfi-64))
(parse "../guile/Makefile")

  ;; Local Variables:
;; mode: scheme
;; End:
