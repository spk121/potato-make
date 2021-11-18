(define-module (potato parse)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 receive)
  #:use-module (system vm trace)
  #:use-module (potato exceptions)
  #:use-module (potato makevars)
  #:use-module (potato rules)
  #:use-module (potato text)
  #:use-module (potato parse-lib)
  #:export (parse _readline))

;; A makefile can contain rules, macro definitions, include lines,
;; and comments.

(define (parse filename)
  (with-input-from-file filename _eval #:guess-encoding #t))

(define (_eval)
  (let ((filenames #f)
        (ignoring #f)
        (commands '()))
    (while #t
      (receive (line nlines)
          (read-line-handle-escaped-newline)
        (cond
         ((zero? nlines)
          (break))

         ((string-starts-with? line #\tab)
          ;; Shell-command lines
          (when filenames
            (when ignoring
              (continue))
            (set! commands (append commands (list line)))))

         (else
          (display
           (string-trim-both
            (string-remove-comments
             (string-collapse-continuations line #t))))
           (newline)))))))

(define (string-parse-variable-definition str i)
  "Parse a string as a variable definition."
  (let loop ((i (string-next-token str)))
    (cond
     ((= i (string-length str))
      (values i 'null))

     ((char=? (string-ref str i) #\#)
      ;; Comments aren't variable definitions.
      (values i 'null))

     ((char=? (string-ref str i) #\$)
      ;; This begins a variable expansion reference.
      (let* ((openparen (false-if-exception (string-ref str (1+ i))))
             (closeparen (if (eqv? openparen #\()
                             #\)
                             (if (eqv? openparen #\{)
                                 #\}
                                 #f))))
        (if (not closeparen)
            (values i 'null)

            ;; else, skip over the matching closeparen
            (begin
              (let ((count 0))
                (while #t
                  (set! i (1+ i))
                  (when (char=? (string-ref str i) openparen)
                    (set! count (1+ count)))
                  (when (char=? (string-ref str i) closeparen)
                    (set! count (1- count))
                    (when (zero? count)
                      (set! i (1+ i))
                      (break)))))

              ;; Any whitespace before the operator?
              (when (char-set-contains? char-set:blank (string-ref str i))
                (set! wspace #t)
                (set! i (string-next-token str i)))

              (cond
               ((eqv? (string-ref str i) #\=)
                (values (1+ i) 'recursive))
               ((and (eqv? (string-ref str i) #\:)
                     (eqv? (string-ref str (1+ i)) #\=))
                (values (+ i 2) 'simple))
               ((and (eqv? (string-ref str i) #\+)
                     (eqv? (string-ref str (1+ i)) #\=))
                (values (+ i 2) 'append))
               ((and (eqv? (string-ref str i) #\?)
                     (eqv? (string-ref str (1+ i)) #\=))
                (values (+ i 2) 'conditional))
               (else
                (values i 'null)))))))
     (else
      (values i 'null)))))
#|
(define (parse-var-assignment line)
  (let ((i (string-next-token line 0)))
    (if (= i (string-length line))
        #f
        ;; else
        (while #t
          
|#
