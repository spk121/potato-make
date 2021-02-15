(define-module (potato text)
  #:export (underline
            default
            right-arrow
            left-arrow
            ellipses
            C0
            red green
            lquo
            rquo
            initialize-text))

(define %fancy #t)
(define (initialize-text ascii)
  (set! %fancy (not ascii)))

(define (default)
  (if %fancy
      (string #\escape #\[ #\0 #\m)
      ""))

(define (bold) 
  (if %fancy
      (string #\escape #\[ #\1 #\m)
      ""))

(define (underline) 
  (if %fancy
      (string #\escape #\[ #\4 #\m)
      ""))

(define (red)
  (if %fancy
      (string #\escape #\[ #\3 #\1 #\m)
      ""))

(define (green)
  (if %fancy
      (string #\escape #\[ #\3 #\2 #\m)
      ""))

(define (blue)
  (if %fancy
      (string #\escape #\[ #\3 #\4 #\m)
      ""))

(define (important)
  (if %fancy
      "⚠"                               ; U+26A0 WARNING SIGN
      "!!!"))

(define (stop)
  (if %fancy
      "🛑"                               ; U+26A0 WARNING SIGN
      "XXX"))

(define (right-arrow)
  (if %fancy
      "→" "->"))

(define (left-arrow)
  (if %fancy
      "←" "<-"))

(define (ellipses)
  (if %fancy "…" "..."))

(define (QED)
  (if %fancy "∎" "QED"))                ; U+220E END OF PROOF

(define (C0 c)
  (if %fancy
      ;; Replace control codes with control pictures
      (string (integer->char (+ #x2400 (char->integer c))))
      (list-ref '("<NUL>" "<SOH>" "<STX>" "<ETX>" "<EOT>" "<ENQ>"
                  "<ACK>" "<BEL>" "<BS>"  "<HT>"  "<LF>"
                  "<VT>" "<FF>" "<CR>" "<SO>" "<SI>"
                  "<DLE>" "<DC1>" "<DC2>" "<DC3>" "<DC4>"
                  "<NAK>" "<SYN>" "<ETB>" "<CAN>" "<EM>"
                  "<SUB>" "<ESC>" "<FS>" "<GS>" "<RS>"
                  "<US>")
                (char->integer c))))

(define (lquo)
  (if %fancy (string #\“) (string #\")))

(define (rquo)
  (if %fancy (string #\”) (string #\")))

(define (BOL)
  "go to beginning of line"
  (if %fancy (string #\escape #\[ #\G) "\n"))

#|
in quiet mode it is just 
☐ target -> parent (when building)
☒ target -> parent   (on pass)
⚠ target -> parent   (on fail but continue)
🛑 target -> parent  (on stop) 
∎                    (on successful completion)

in normal mode it is
?  target -> parent
☐ recipe truncated to 70 cols, using C0 control pics
  etc
then
☒ target -> parent   (on pass)
|#
