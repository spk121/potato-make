(define-module (potato lib)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 ftw)
  #:use-module (potato makevars)
  #:export (subst
            patsubst
            words        W
            strip        S
            findstring   F
            filter-in
            filter-out
            word
            firstword
            lastword
            wordlist
            sort-list
            file-dir
            file-notdir
            file-suffix
            file-basename
            addprefix
            addsuffix
            join-two
            wildcard
            file-realpath
            file-abspath
            strip-quotes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERNAL HELPERS

(define (join-words words)
  "Join WORDS using a single ASCII space."
  (if (null? words)
      ""
      (let loop ((rest (cdr words))
                 (out (car words)))
        (if (null? rest)
            out
            (loop (cdr rest)
                  (string-append out " " (car rest)))))))

(define (strip-quotes text)
  "Remove all single-quote and double-quote characters from TEXT."
  (string-filter (lambda (c)
                   (not (or (char=? c #\")
                            (char=? c #\'))))
                 text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUOTING HELPERS

(define (fold-chars/quoted text proc init)
  "Walk TEXT maintaining single/double quote state.
For each non-quote-delimiter character, call (PROC char in-quote? acc),
where in-quote? is #t when inside single or double quotes, #f otherwise.
Quote delimiter characters themselves are consumed and not passed to PROC.
Unmatched quotes remain active to the end of the string.
Returns the final accumulator."
  (let ((len (string-length text)))
    (let loop ((i 0) (quote-state #f) (acc init))
      (if (>= i len)
          acc
          (let ((c (string-ref text i)))
            (cond
              ((char=? c #\")
               (loop (+ i 1) (if (eq? quote-state 'double) #f 'double) acc))
              ((char=? c #\')
               (loop (+ i 1) (if (eq? quote-state 'single) #f 'single) acc))
              (else
               (loop (+ i 1) quote-state
                     (proc c (if quote-state #t #f) acc)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PATTERN MATCHING HELPERS

(define (pattern-stem pattern word)
  "Return the stem if WORD matches PATTERN with %, else #f."
  (let ((pct (string-index pattern #\%)))
    (if (not pct)
        (and (string=? pattern word) "")
        (let* ((prefix (substring pattern 0 pct))
               (suffix (substring pattern (+ pct 1)))
               (prefix-len (string-length prefix))
               (suffix-len (string-length suffix))
               (word-len (string-length word)))
          (and (>= word-len (+ prefix-len suffix-len))
               (string-prefix? prefix word)
               (string-suffix? suffix word)
               (substring word
                          prefix-len
                          (- word-len suffix-len)))))))

(define (replace-stem replacement stem)
  "Replace the first % in REPLACEMENT with STEM."
  (let ((pct (string-index replacement #\%)))
    (if pct
        (string-append (substring replacement 0 pct)
                       stem
                       (substring replacement (+ pct 1)))
        replacement)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOB HELPERS

(define (glob-match? pattern name)
  "Match NAME against a glob PATTERN containing * and ? wildcards.
Does not handle character classes."
  (let ((plen (string-length pattern))
        (nlen (string-length name)))
    (let loop ((pi 0) (ni 0))
      (cond
        ((= pi plen) (= ni nlen))
        ((char=? (string-ref pattern pi) #\*)
         (let star ((ni ni))
           (cond
             ((loop (+ pi 1) ni) #t)
             ((>= ni nlen) #f)
             (else (star (+ ni 1))))))
        ((>= ni nlen) #f)
        ((char=? (string-ref pattern pi) #\?)
         (loop (+ pi 1) (+ ni 1)))
        ((char=? (string-ref pattern pi) (string-ref name ni))
         (loop (+ pi 1) (+ ni 1)))
        (else #f)))))

(define (has-glob-chars? str)
  "Return #t if STR contains * or ? wildcard characters."
  (or (string-index str #\*)
      (string-index str #\?)))

(define (glob-one pattern)
  "Expand a single glob PATTERN into a sorted list of matching paths."
  (if (not (has-glob-chars? pattern))
      ;; No wildcards: just test existence
      (if (file-exists? pattern)
          (list pattern)
          '())
      (let* ((sep (string-rindex pattern #\/))
             (dir-part (if sep (substring pattern 0 (+ sep 1)) ""))
             (file-pat (if sep (substring pattern (+ sep 1)) pattern))
             (dir-path (if (string-null? dir-part) "." dir-part)))
        (catch #t
          (lambda ()
            (let* ((d (opendir dir-path))
                   (entries
                    (let loop ((acc '()))
                      (let ((entry (readdir d)))
                        (if (eof-object? entry)
                            acc
                            (loop (if (and (not (string=? entry "."))
                                           (not (string=? entry ".."))
                                           (glob-match? file-pat entry))
                                      (cons (string-append dir-part entry)
                                            acc)
                                      acc)))))))
              (closedir d)
              (sort entries string<?)))
          (lambda _ '())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PATH NORMALIZATION

(define (normalize-path path)
  "Remove . and resolve .. components from PATH."
  (let loop ((parts (string-split path #\/))
             (acc '()))
    (cond
      ((null? parts)
       (string-join (reverse acc) "/"))
      ((string=? (car parts) ".")
       (loop (cdr parts) acc))
      ((string=? (car parts) "..")
       (loop (cdr parts) (if (null? acc) acc (cdr acc))))
      (else
       (loop (cdr parts) (cons (car parts) acc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUBLIC TEXT FUNCTIONS

(define (words text)
  "Split TEXT into a list of words at whitespace, respecting single and
double quotes.  Spaces inside a quoted region are not treated as
word boundaries.  Quote characters themselves are stripped from the
result.  Unmatched quotes are consumed to the end of the string."
  (unless (string? text)
    (error "words: TEXT must be a string" text))
  (let* ((result
          (fold-chars/quoted text
            (lambda (c in-quote? state)
              (let ((buf  (car state))
                    (acc  (cdr state)))
                (if (and (char-whitespace? c) (not in-quote?))
                    (if (string-null? buf)
                        state
                        (cons "" (cons buf acc)))
                    (cons (string-append buf (string c)) acc))))
            (cons "" '())))
         (buf       (car result))
         (word-list (cdr result)))
    (reverse (if (string-null? buf) word-list (cons buf word-list)))))

(define-syntax W
  (lambda (stx)
    (syntax-case stx ()
      ((_ key)
       #'(words (reference (symbol->string (syntax->datum #'key)) #f))))))

(define (strip text)
  "Remove leading and trailing unquoted whitespace from TEXT and replace
each internal sequence of unquoted whitespace characters with a single
space.  Whitespace inside single or double quotes is preserved literally.
Quote characters themselves are stripped from the result."
  (unless (string? text)
    (error "strip: TEXT must be a string" text))
  (let* ((result
          (fold-chars/quoted text
            (lambda (c in-quote? state)
              (let ((out (car state))
                    (ws? (cdr state)))
                (if (and (char-whitespace? c) (not in-quote?))
                    (cons out (not (string-null? out)))
                    (cons (string-append out
                                         (if ws? " " "")
                                         (string c))
                          #f))))
            (cons "" #f))))
    (car result)))

(define-syntax S
  (lambda (stx)
    (syntax-case stx ()
      ((_ key)
       #'(strip (reference (symbol->string (syntax->datum #'key)) #f))))))

(define (findstring find in)
  "GNU make-like findstring: search IN for FIND.
Returns FIND if it occurs in IN, otherwise returns the empty string."
  (unless (string? find)
    (error "findstring: FIND must be a string" find))
  (unless (string? in)
    (error "findstring: IN must be a string" in))
  (if (string-contains in find)
      find
      ""))

(define-syntax F
  (lambda (stx)
    (syntax-case stx ()
      ((_ find-key in-key)
       #'(findstring (reference (symbol->string (syntax->datum #'find-key)) #f)
                     (reference (symbol->string (syntax->datum #'in-key)) #f))))))

(define (subst from to text)
  "Performs textual substitution on string TEXT,
replacing every instance of the string FROM with string TO.
Returns the new string."
  (unless (string? from)
    (error "subst: FROM must be a string" from))
  (unless (string? to)
    (error "subst: TO must be a string" to))
  (unless (string? text)
    (error "subst: TEXT must be a string" text))
  (if (zero? (string-length from))
      text
      (let ((from-len (string-length from)))
        (let loop ((start 0)
                   (chunks '()))
          (let ((idx (string-contains text from start)))
            (if idx
                (loop (+ idx from-len)
                      (cons to
                            (cons (substring text start idx) chunks)))
                (apply string-append
                       (reverse (cons (substring text start) chunks)))))))))

(define (patsubst pattern replacement text)
  "GNU make-like pattern substitution over whitespace-separated words.
PATTERN may contain one `%` wildcard; matching words are rewritten using
REPLACEMENT with `%` substituted by the captured stem."
  (unless (string? pattern)
    (error "patsubst: PATTERN must be a string" pattern))
  (unless (string? replacement)
    (error "patsubst: REPLACEMENT must be a string" replacement))
  (unless (string? text)
    (error "patsubst: TEXT must be a string" text))
  (join-words
   (map (lambda (word)
          (let ((stem (pattern-stem pattern word)))
            (if stem
                (replace-stem replacement stem)
                word)))
        (words text))))

(define (filter-in patterns text)
  "GNU make-like filter over whitespace-separated words.
PATTERNS is a whitespace-separated set of pattern words that may contain
`%` wildcards.  Returns the words in TEXT that match any PATTERNS word."
  (unless (string? patterns)
    (error "filter-in: PATTERNS must be a string" patterns))
  (unless (string? text)
    (error "filter-in: TEXT must be a string" text))
  (let ((pattern-words (words patterns)))
    (define (matches-any-pattern? word)
      (let loop ((patterns pattern-words))
        (and (not (null? patterns))
             (or (let ((stem (pattern-stem (car patterns) word)))
                   (not (eq? stem #f)))
                 (loop (cdr patterns))))))
    (join-words
     (let loop ((text-words (words text))
                (matches '()))
       (if (null? text-words)
           (reverse matches)
           (let ((word (car text-words)))
             (loop (cdr text-words)
                   (if (matches-any-pattern? word)
                       (cons word matches)
                       matches))))))))

(define (filter-out patterns text)
  "GNU make-like filter-out over whitespace-separated words.
PATTERNS is a whitespace-separated set of pattern words that may contain
`%` wildcards.  Returns the words in TEXT that do NOT match any PATTERNS
word."
  (unless (string? patterns)
    (error "filter-out: PATTERNS must be a string" patterns))
  (unless (string? text)
    (error "filter-out: TEXT must be a string" text))
  (let ((pattern-words (words patterns)))
    (define (matches-any-pattern? word)
      (let loop ((patterns pattern-words))
        (and (not (null? patterns))
             (or (let ((stem (pattern-stem (car patterns) word)))
                   (not (eq? stem #f)))
                 (loop (cdr patterns))))))
    (join-words
     (let loop ((text-words (words text))
                (kept '()))
       (if (null? text-words)
           (reverse kept)
           (let ((word (car text-words)))
             (loop (cdr text-words)
                   (if (matches-any-pattern? word)
                       kept
                       (cons word kept)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WORD FUNCTIONS

(define (word n text)
  "Return the Nth word of TEXT (1-indexed).
Returns the empty string if N is out of range."
  (unless (string? text)
    (error "word: TEXT must be a string" text))
  (let ((ws (words text)))
    (if (or (< n 1) (> n (length ws)))
        ""
        (list-ref ws (- n 1)))))

(define (firstword text)
  "Return the first word of TEXT, or empty string if TEXT has no words."
  (unless (string? text)
    (error "firstword: TEXT must be a string" text))
  (let ((ws (words text)))
    (if (null? ws) "" (car ws))))

(define (lastword text)
  "Return the last word of TEXT, or empty string if TEXT has no words."
  (unless (string? text)
    (error "lastword: TEXT must be a string" text))
  (let ((ws (words text)))
    (if (null? ws) "" (car (last-pair ws)))))

(define (wordlist start end text)
  "Return words START through END (1-indexed, inclusive) from TEXT.
Returns the empty string if the range is empty."
  (unless (string? text)
    (error "wordlist: TEXT must be a string" text))
  (let* ((ws  (words text))
         (len (length ws))
         (s   (max 1 start))
         (e   (min len end)))
    (if (> s e)
        ""
        (join-words
         (let loop ((i 1) (rest ws) (acc '()))
           (cond
             ((null? rest) (reverse acc))
             ((> i e) (reverse acc))
             ((>= i s) (loop (+ i 1) (cdr rest) (cons (car rest) acc)))
             (else (loop (+ i 1) (cdr rest) acc))))))))

(define (sort-list text)
  "Sort words in TEXT lexicographically and remove duplicates.
Returns a space-separated string."
  (unless (string? text)
    (error "sort-list: TEXT must be a string" text))
  (join-words (delete-duplicates (sort (words text) string<?) string=?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILENAME FUNCTIONS

(define (file-dir text)
  "Directory part of each filename word, with trailing slash.
Files without a directory component yield \"./\"."
  (unless (string? text)
    (error "file-dir: TEXT must be a string" text))
  (join-words
   (map (lambda (w)
          (let* ((raw (strip-quotes w))
                 (sep (string-rindex raw #\/)))
            (if sep
                (substring raw 0 (+ sep 1))
                "./")))
        (words text))))

(define (file-notdir text)
  "Non-directory part of each filename word."
  (unless (string? text)
    (error "file-notdir: TEXT must be a string" text))
  (join-words
   (map (lambda (w)
          (let* ((raw (strip-quotes w))
                 (sep (string-rindex raw #\/)))
            (if sep
                (substring raw (+ sep 1))
                raw)))
        (words text))))

(define (file-suffix text)
  "The suffix (including dot) of each word.  Empty string for words
with no suffix.  Only the last dot after the final slash counts."
  (unless (string? text)
    (error "file-suffix: TEXT must be a string" text))
  (join-words
   (map (lambda (w)
          (let* ((raw (strip-quotes w))
                 (sep (string-rindex raw #\/))
                 (base (if sep (substring raw (+ sep 1)) raw))
                 (dot (string-rindex base #\.)))
            (if dot
                (substring base dot)
                "")))
        (words text))))

(define (file-basename text)
  "Everything except the suffix of each word."
  (unless (string? text)
    (error "file-basename: TEXT must be a string" text))
  (join-words
   (map (lambda (w)
          (let* ((raw (strip-quotes w))
                 (sep (string-rindex raw #\/))
                 (dot (string-rindex raw #\.)))
            (if (and dot (or (not sep) (> dot sep)))
                (substring raw 0 dot)
                raw)))
        (words text))))

(define (addprefix prefix text)
  "Prepend PREFIX to each word in TEXT."
  (unless (string? prefix)
    (error "addprefix: PREFIX must be a string" prefix))
  (unless (string? text)
    (error "addprefix: TEXT must be a string" text))
  (join-words
   (map (lambda (w) (string-append prefix w))
        (words text))))

(define (addsuffix suffix text)
  "Append SUFFIX to each word in TEXT."
  (unless (string? suffix)
    (error "addsuffix: SUFFIX must be a string" suffix))
  (unless (string? text)
    (error "addsuffix: TEXT must be a string" text))
  (join-words
   (map (lambda (w) (string-append w suffix))
        (words text))))

(define (join-two list1 list2)
  "Pairwise concatenation of words in LIST1 and LIST2.
If the lists differ in length, extra words are included unchanged."
  (unless (string? list1)
    (error "join-two: LIST1 must be a string" list1))
  (unless (string? list2)
    (error "join-two: LIST2 must be a string" list2))
  (let loop ((a (words list1))
             (b (words list2))
             (acc '()))
    (cond
      ((and (null? a) (null? b))
       (join-words (reverse acc)))
      ((null? a)
       (join-words (append (reverse acc) b)))
      ((null? b)
       (join-words (append (reverse acc) a)))
      (else
       (loop (cdr a) (cdr b)
             (cons (string-append (car a) (car b)) acc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILESYSTEM FUNCTIONS

(define (wildcard text)
  "GNU make-like wildcard expansion.  TEXT is a whitespace-separated
list of glob patterns.  Returns a single string of all matching
filenames, sorted and space-separated.  Each pattern is sorted
independently and the results are concatenated in pattern order.
Patterns without wildcards are included only if the file exists."
  (unless (string? text)
    (error "wildcard: TEXT must be a string" text))
  (join-words
   (append-map (lambda (pat)
                 (glob-one (strip-quotes pat)))
               (words text))))

(define (file-realpath text)
  "Canonical absolute path of each word, resolving symlinks.
Words that do not correspond to existing paths are silently omitted."
  (unless (string? text)
    (error "file-realpath: TEXT must be a string" text))
  (join-words
   (filter-map (lambda (w)
                 (let ((raw (strip-quotes w)))
                   (catch #t
                     (lambda () (canonicalize-path raw))
                     (lambda _ #f))))
               (words text))))

(define (file-abspath text)
  "Absolute path of each word without resolving symlinks.
Relative paths are prefixed with the current working directory.
The . and .. components are resolved lexically."
  (unless (string? text)
    (error "file-abspath: TEXT must be a string" text))
  (join-words
   (map (lambda (w)
          (let ((raw (strip-quotes w)))
            (normalize-path
             (if (absolute-file-name? raw)
                 raw
                 (string-append (getcwd) "/" raw)))))
        (words text))))
