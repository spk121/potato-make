(define-module (potato parse-lib)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 optargs)
  #:use-module (system vm trace)
  #:use-module (potato exceptions)
  #:use-module (potato makevars)
  #:use-module (potato rules)
  #:use-module (potato text)
  #:export(
           read-line-handle-escaped-newline
           string-collapse-continuations
           string-count-backslashes-at-end
           string-count-matching-chars-at-end
           string-find-char-unquote
           string-find-preceding-backslashes
           string-find-repeated-chars
           string-next-token
           string-remove-comments
           string-starts-with?
           string-shrink-whitespace
           ))

(define (read-line-handle-escaped-newline)
  "Reads a line of text from the current input port.

If the line ends with an odd number of backslashes, the following line
is read and appended.  The output string has newlines as line
terminators.

It returns two values
 - the string containing the one or more lines read
 - the number of lines read."
  (let loop ((output "")
             (nline 0))
    (let ((line (read-line)))
      (if (eof-object? line)
          (values output nline)
          ;; else
          (if (odd? (string-count-backslashes-at-end line))
              (loop (string-append output (string-drop-right line 1) "\n")
                    (1+ nline))
              ;; else
              (values (string-append output line "\n") (1+ nline)))))))

(define* (string-collapse-continuations str #:optional (squash-whitespace? #f))
  "Returns a new string where backslash+newline is discarded, and
backslash+backslash+newline becomes backslash+newline. Any whitespace
after the newline may be squashed to a single space, if
squash-whitespace? is #t."
  (let loop ((str str)
             (newline-index (string-rindex str #\newline)))
    (if (not newline-index)
        (string-copy str)
        ;; else
        (let* ((backslash-index (string-find-preceding-backslashes str newline-index))
               (backslash-count (- newline-index backslash-index)))
          (cond
           ((even? backslash-count)
            ;; We cut the number of backslashes in half, but, keep the newline.
            (loop
             (string-append
              (substring str 0 backslash-index)
              (make-string (quotient backslash-count 2) #\\)
              (substring str newline-index))
             (string-rindex str #\newline 0 backslash-index)))

           (else
            ;; We cut the number of backslashes in half, remove a backslash
            ;; and newline, maybe squash any following whitespace.
            (loop
             (string-append
              (substring str 0 backslash-index)
              (make-string (quotient backslash-count 2) #\\)
              (if squash-whitespace?
                  (string-shrink-whitespace
                   (substring str (1+ newline-index))
                   0)
                  (substring str (1+ newline-index))))
             (string-rindex str #\newline 0 backslash-index))))))))

(define (string-count-backslashes-at-end str)
  (string-count-matching-chars-at-end str #\\))

(define (string-count-matching-chars-at-end str c)
  (if (not (string? str))
      0
      ;; else
      (let ((len (string-length str)))
        (if (zero? len)
            0
            ;; else
            (let loop ((i (1- len))
                       (n 0))
              (if (char=? (string-ref str i) c)
                  (if (zero? i)
                      (1+ n)
                      ;; else
                      (loop (1- i) (1+ n)))
                  ;; else
                  n))))))

(define* (string-find-char-unquote str #:optional (stop1 #f) (stop2 #f) #:key (blank #f) (ignorevars #f))
  "Search string for an unquoted stopchar. Search until the end of
line is reached or until the blank char is reached.  A backslash
quotes the stopchars and the blank.

The stopchars and blank may be #f, to ignore them in this call.

If ignorevars is #t, stopchars inside of variable references are ignored

Returns two values
 - A string with the quoting backslashes removed
 - The position of the first unquoted stopchar, or #f"
  (let ((i 0)
        (str2 (string-copy str))
        (ret #f))
    (while #t
      (while (and (< i (string-length str))
                  (not (or
                        (and stop1 (eqv? (string-ref str i) stop1))
                        (and stop2 (eqv? (string-ref str i) stop2))
                        (and ignorevars (eqv? (string-ref str i) #\$))
                        (and blank (char-set-contains? char-set:blank (string-ref str i))))))
        (set! i (1+ i)))

      (when (>= i (string-length str))
        (break))

      ;; If we stopped due to a variable reference, skip its contents
      (when (and ignorevars (eqv? (string-ref str i) #\$))
        (let ((openparen (false-if-exception (string-ref str (1+ i)))))
          (set! i (+ i 2))
          
          (when (or (eqv? openparen #\() (eqv? openparen #\{))
            (let ((pcount 1)
                  (closeparen (if (eqv? openparen #\()
                                  #\)
                                  #\})))
              (while (< i (string-length str))
                (cond
                 ((eqv? (string-ref str i) openparen)
                  (set! pcount (1+ pcount)))
                 ((eqv? (string-ref str i) closeparen)
                  (set! pcount (1- pcount))
                  (when (zero? pcount)
                    (set! i (1+ i))
                    (break))))
                (set! i (1+ i)))))
          
          ;; Skipped the variable reference: look for STOPCHARS again
          (continue)))

      (if (and (> i 0) (eqv? (string-ref str (1- i)) #\\))
          ;; Search for more backslashes
          (let ((j 2))
            (while (and (>= (- i j) 0)
                        (eqv? (string-ref str (- i j)) #\\))
              (set! j (1+ j)))
            (set! j (1- j))
            ;; Copy the string to swallow half the backslashes
            (set! str2
              (string-append 
               (string-take str (- i j))
               (string-drop str (- i (quotient j 2)))))
            (set! i (- i (quotient j 2)))
            (when (even? j)
              ;; All the backslashes quoted each other; the stopchar
              ;; was unquoted
              (set! ret #t)
              (break))
            (set! str str2))
          ;; else
          ;; no backslash in sight
          (begin
            (set! ret #t)
            (break))))

    (if ret
        (values i str2)
        (values #f #f))))

(define (string-find-preceding-backslashes str i)
  "Given I, a position in a string, this returns a position,
of first of a range sequential backslashes that immediately precedes
that position in the string. If no backslashes precede that position,
I is returned."
  (let loop ((j i))
    (cond
     ((= j 0)
      0)
     ((char=? (string-ref str (1- j)) #\\)
      (loop (1- j)))
     (else
      j))))

(define (string-find-repeated-chars str c)
  "Given a character c, this finds the position of of first instance
of the character in the string. If the character repeats, it returns,
as a pair, the position of the character, and the position after the
run of characters.  If the character is not present, it returns #f"
  (let ((i (string-index str c)))
    (if (not i)
        #f
        ;; else
        (let ((len (string-length str)))
          (if (= (1+ i) len)
              (cons i (1+ i))
              ;; else
              (let loop ((j (1+ i)))
                (if (false-if-exception (char=? (string-ref str j) c))
                    (loop (1+ j))
                    ;; else
                    (cons i j))))))))

(define (string-next-token str i)
  "Given a position i, this returns the position of the first
non-whitespace character after i. If the end of line is reached,
it returns the length of the string."
  (or
   (string-index str
                 (lambda (c)
                   (char-set-contains? char-set:blank c))
                 i)
   (string-length str)))

(define (string-remove-comments str)
  "Returns a copy of str with any '#' comments removed"
  (let ((i (string-find-char-unquote str #\#)))
    (if i
        (string-take str i)
        (string-copy str))))

(define (string-shrink-whitespace str start)
  "Given a string, and a location in a string, this returns a new copy
of string where all the whitespace beginning at location i is replaced
by a single space"
  (let ((len (string-length str)))
    (if (or (>= start len)
            (not (char-set-contains? char-set:blank (string-ref str start))))
        (string-copy str)
        ;; else
        (let loop ((end start))
          (cond
           ((>= end len)
            (string-append (substring str 0 start) " "))
           ((char-set-contains? char-set:blank (string-ref str end))
            (loop (1+ end)))
           (else
            (string-append (substring str 0 start) " " (substring str end))))))))


(define (string-starts-with? str c)
  (char=? (string-ref str 0) c))
