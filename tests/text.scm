;;; text.scm -- part of the Potato Make test suite.

(use-modules (potato text)
             (srfi srfi-1)
             (srfi srfi-64)
             (tests common))

(test-begin "text")

(define (ascii-string? str)
  (string-every (lambda (c) (and (char>=? c #\space) (char<? c #\delete)))
                str))

;;;;;;;;;;;;;;;;
;; ASCII mode

(initialize-text #t)

(test-equal "ASCII mode: color and style codes are empty strings."
  '("" "" "" "")
  (list (default) (underline) (red) (green)))

(test-equal "ASCII mode: arrows." '("->" "<-") (list (right-arrow) (left-arrow)))
(test-equal "ASCII mode: ellipses." "..." (ellipses))
(test-equal "ASCII mode: quotes." '("\"" "\"") (list (lquo) (rquo)))
(test-equal "ASCII mode: NUL control character name." "<NUL>" (C0 #\nul))
(test-equal "ASCII mode: tab control character name." "<HT>" (C0 #\tab))
(test-equal "ASCII mode: newline control character name." "<LF>" (C0 #\newline))
(test-equal "ASCII mode: US control character name." "<US>" (C0 (integer->char 31)))

(test-assert "ASCII mode: every helper returns plain ASCII."
  (every ascii-string?
         (list (default) (underline) (red) (green) (right-arrow)
               (left-arrow) (ellipses) (lquo) (rquo) (C0 #\tab))))

;;;;;;;;;;;;;;;;
;; Fancy mode

(initialize-text #f)

(test-equal "Fancy mode: default is the ANSI reset sequence."
  (string-append (string #\escape) "[0m") (default))
(test-equal "Fancy mode: underline." (string-append (string #\escape) "[4m") (underline))
(test-equal "Fancy mode: red." (string-append (string #\escape) "[31m") (red))
(test-equal "Fancy mode: green." (string-append (string #\escape) "[32m") (green))
(test-equal "Fancy mode: arrows." '("→" "←") (list (right-arrow) (left-arrow)))
(test-equal "Fancy mode: ellipses." "…" (ellipses))
(test-equal "Fancy mode: quotes." '("“" "”") (list (lquo) (rquo)))
(test-equal "Fancy mode: tab is shown as a control picture."
  "␉" (C0 #\tab))
(test-equal "Fancy mode: newline is shown as a control picture."
  "␊" (C0 #\newline))

(test-exit "text")
