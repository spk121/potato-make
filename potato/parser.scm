(define-module (potato parser)
  #:use-module (ice-9 peg)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 rdelim)
  #:use-module (system vm trace)
  #:use-module (potato exceptions)
  #:use-module (potato makevars)
  #:use-module (potato rules)
  #:use-module (potato text)
  #:export (parse))

;; A makefile can contain rules, macro definitions, include lines,
;; and comments.

(define (parse filename)
  (with-input-from-file filename parse-input #:guess-encoding #t))

(define (last-char str)
  (string-ref str (1- (string-length str))))

(define (parse-input)
  (while #t
    (let loop ((line "")
               (str (read-line)))
      (cond
       ((eof-object? str)
        (break))
       ((char=? (last-char str) #\\)
        (loop (string-append line str) (read-line)))
       (else
        (parse-line (string-append line str)))))))

;; For include lines
(define-peg-pattern I_TOK none "include")
(define-peg-pattern I_SPACE none (or " " "\t"))
(define-peg-pattern I_FILENAME_CHAR body (or (range #\a #\z)
                                          (range #\A #\Z)
                                          (range #\0 #\9)
                                          "_" "-" "."))
(define-peg-pattern I_FILENAME all (+ I_FILENAME_CHAR))
(define-peg-pattern I_NL none "\n")
(define-peg-pattern I_COMMENT none (and "#" (* peg-any)))
(define-peg-pattern INCLUDE all (and I_TOK
                                     (+ (and (* I_SPACE)
                                             I_FILENAME))
                                     (* I_SPACE)
                                     (? I_COMMENT)))

;; For comment lines
(define-peg-pattern C_SPACE none (or " " "\t"))
(define-peg-pattern C_COMMENT none (and "#" (* peg-any)))
(define-peg-pattern COMMENT none (or C_COMMENT
                                    (and (+ C_SPACE) (not-followed-by peg-any))))


(define (parse-line line)
  (write (peg:tree (match-pattern INCLUDE line)))
  (newline)
  (write (peg:tree (match-pattern COMMENT line)))
  (newline)
  (cond
   ((line-is-include? line)
    (format #t "INCLUDE:   ~S~%" line))
   ((line-is-comment? line)
    (format #t "COMMENT:   ~S~%" line))
   ((line-is-macro? line)
    (format #t "MACRO:     ~S~%" line))
   ((line-is-special-target? line)
    (format #t "SPECIAL:   ~S~%" line))
   ((line-is-inference-rule? line)
    (format #t "INFERENCE: ~S~%" line))
   ((line-is-rule? line)
    (format #t "RULE:      ~S~%" line))
   (else
    (format #t "UNKNOWN:   ~S~%" line))))



(define (line-is-include? line)
  (and (> (string-length line) 8)
       (string= line "include " 0 8)))

(define (line-is-comment? line)
  (or (string-null? (string-trim-both line char-set:whitespace))
      (char=? (string-ref line 0) #\#)))

(define (line-is-macro? line)
  (let ((len (string-length line)))
    (let loop ((i 0))
      (if (>= i len)
          #f
          ;; else
          (let ((c (string-ref line i)))
            (cond
             ((and (zero? i)
                   (not (char-is-pcs? c)))
              #f)
             ((and (not (zero? i))
                   (char=? #\= c))
              #t)
             ((not (char-is-pcs-or-space? c))
              #f)
             (else
              (loop (+ i 1)))))))))

(define (line-is-special-target? line)
  (or (and (>= (string-length line) 8)
           (string= line ".DEFAULT" 0 8))
      (and (>= (string-length line) 8)
           (string= line ".IGNORE" 0 7))
      (and (>= (string-length line) 6)
           (string= line ".POSIX"))
      (and (>= (string-length line) 9)
           (string= line ".PRECIOUS" 0 9))
      (and (>= (string-length line) 9)
           (string= line ".SCCS_GET" 0 9))
      (and (>= (string-length line) 7)
           (string= line ".SILENT" 0 7))))

(define (line-is-rule? line)
  (let ((len (string-length line)))
    (let loop ((i 0))
      (if (>= i len)
          #f
          ;; else
          (let ((c (string-ref line i)))
            (cond
             ((and (zero? i)
                   (not (char-is-pcs? c)))
              #f)
             ((and (not (zero? i))
                   (char=? #\: c))
              #t)
             ((not (char-is-pcs-or-space? c))
              #f)
             (else
              (loop (+ i 1)))))))))

(define (line-is-inference-rule? line)
  (let ((len (string-length line)))
    (let loop ((i 0)
               (dot-count 0))
      (if (>= i len)
          #f
          ;; else
          (let ((c (string-ref line i)))
            (cond
             ((and (zero? i)
                   (not (char=? #\. c)))
              #f)
             ((and (not (zero? i))
                   (char=? #\: c))
              (if (or (= dot-count 1)
                      (= dot-count 2))
                  #t
                  #f))
             ((not (char-is-pcs? c))
              #f)
             (else
              (loop (+ i 1)
                    (+ dot-count
                       (if (char=? c #\.)
                           1
                           0))))))))))

(define (char-is-pcs? c)
  (or (and (char<=? #\a c) (char>=? #\z c))
      (and (char<=? #\A c) (char>=? #\Z c))
      (and (char<=? #\0 c) (char>=? #\9 c))
      (char=? #\. c)
      (char=? #\_ c)))

(define (char-is-pcs-or-space? c)
  (or (char-is-pcs? c)
      (char=? #\space c)))         

