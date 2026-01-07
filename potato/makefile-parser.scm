(define-module (potato makefile-parser)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (potato parse-lib)
  #:export (parse-makefile
            makefile->potato-make
            parse-makefile-line
            parse-suffix-rule
            parse-makefile-from-port))

;; Parser for POSIX Makefiles that converts them to potato-make code

;; Parse a line and identify its type
(define (parse-makefile-line line continuation?)
  "Parse a single line and identify what type of makefile element it is.
   Returns (type . data) or #f if the line should be ignored."
  (cond
   ;; Blank line
   ((string-null? (string-trim-both line))
    (cons 'blank #f))
   
   ;; Comment line (starting with #)
   ((and (> (string-length line) 0)
         (char=? (string-ref line 0) #\#))
    (cons 'comment line))
   
   ;; Recipe line (starting with tab)
   ((and (> (string-length line) 0)
         (char=? (string-ref line 0) #\tab))
    (cons 'recipe (string-trim-right (substring line 1))))
   
   ;; Variable assignment (contains = but not :)
   ((and (string-index line #\=)
         (not continuation?))
    (let* ((parts (string-split-on-first line #\=))
           (var-name (string-trim-both (car parts)))
           (var-value (string-trim-both (cadr parts))))
      (cons 'variable (cons var-name var-value))))
   
   ;; Suffix rule (.ext1.ext2:)
   ((is-suffix-rule? line)
    (let* ((colon-pos (string-index line #\:))
           (rule-str (string-trim-both (substring line 0 colon-pos))))
      (cons 'suffix-rule rule-str)))
   
   ;; Target rule (contains : with target and optional prerequisites)
   ((string-index line #\:)
    (let* ((colon-pos (string-index line #\:))
           (target (string-trim-both (substring line 0 colon-pos)))
           (prereqs-str (string-trim-both (substring line (+ colon-pos 1))))
           (prereqs (if (string-null? prereqs-str)
                       '()
                       (string-split prereqs-str #\space))))
      (cons 'target-rule (cons target (filter (lambda (s) (not (string-null? s))) prereqs)))))
   
   (else
    ;; Unknown line type
    (cons 'unknown line))))

;; Helper to check if a line is a suffix rule (.c.o:)
(define (is-suffix-rule? line)
  (and (> (string-length line) 0)
       (char=? (string-ref line 0) #\.)
       (string-index line #\:)
       (let* ((colon-pos (string-index line #\:))
              (before-colon (substring line 0 colon-pos))
              (dot-count (string-count before-colon #\.)))
         (= dot-count 2))))

;; Split string on first occurrence of a character
(define (string-split-on-first str ch)
  (let ((idx (string-index str ch)))
    (if idx
        (list (substring str 0 idx)
              (substring str (+ idx 1)))
        (list str ""))))

;; Count occurrences of a character in a string
(define (string-count str ch)
  (let loop ((i 0) (count 0))
    (if (>= i (string-length str))
        count
        (loop (+ i 1)
              (if (char=? (string-ref str i) ch)
                  (+ count 1)
                  count)))))

;; Parse an entire makefile from a port
(define (parse-makefile-from-port port)
  "Parse a makefile from an input port and return a list of parsed elements."
  (let loop ((elements '())
             (current-rule #f)
             (current-recipes '()))
    (let ((line (read-line port)))
      (cond
       ;; End of file
       ((eof-object? line)
        (if current-rule
            (reverse (cons (cons current-rule (reverse current-recipes)) elements))
            (reverse elements)))
       
       ;; Handle line continuation (ending with \)
       ((and (> (string-length line) 0)
             (char=? (string-ref line (- (string-length line) 1)) #\\))
        (let* ((next-line (read-line port))
               (continued-line (string-append 
                               (substring line 0 (- (string-length line) 1))
                               (if (eof-object? next-line) "" next-line))))
          (let ((parsed (parse-makefile-line continued-line #t)))
            (if current-rule
                ;; If we're in a rule, this might be part of prerequisites
                (loop elements current-rule current-recipes)
                ;; Otherwise add as normal element
                (loop (cons parsed elements) #f '())))))
       
       (else
        (let ((parsed (parse-makefile-line line #f)))
          (case (car parsed)
            ((recipe)
             ;; Recipe line - add to current rule's recipes
             (if current-rule
                 (loop elements current-rule (cons (cdr parsed) current-recipes))
                 ;; Recipe without a rule - skip
                 (loop elements #f '())))
            
            ((target-rule suffix-rule)
             ;; New rule - save previous rule if any
             (if current-rule
                 (loop (cons (cons current-rule (reverse current-recipes)) elements)
                       parsed '())
                 (loop elements parsed '())))
            
            ((variable comment blank)
             ;; Non-rule line - save any current rule first
             (if current-rule
                 (loop (cons (cons current-rule (reverse current-recipes))
                            (cons parsed elements))
                       #f '())
                 (loop (cons parsed elements) #f '())))
            
            (else
             ;; Unknown - skip
             (loop elements current-rule current-recipes)))))))))

;; Parse a makefile from a filename
(define (parse-makefile filename)
  "Parse a POSIX makefile from a file and return parsed elements."
  (call-with-input-file filename parse-makefile-from-port))

;; Convert parsed elements to potato-make code
(define (elements->potato-make elements)
  "Convert parsed makefile elements to potato-make Scheme code as a string."
  (let ((output (open-output-string)))
    ;; Write header
    (display "#!/usr/bin/env sh\n" output)
    (display "exec guile -s \"$0\" \"$@\"\n" output)
    (display "!#\n\n" output)
    (display "(use-modules (potato make))\n" output)
    (display "(initialize)\n\n" output)
    
    ;; Process elements
    (for-each
     (lambda (element)
       (cond
        ;; Variable assignment
        ((and (pair? element) (eq? (car element) 'variable))
         (let ((var-name (cadr element))
               (var-value (cddr element)))
           (format output "(:= ~a ~s)\n" var-name var-value)))
        
        ;; Comment
        ((and (pair? element) (eq? (car element) 'comment))
         (format output "~a\n" (cdr element)))
        
        ;; Blank line
        ((and (pair? element) (eq? (car element) 'blank))
         (display "\n" output))
        
        ;; Target rule with recipes
        ((and (pair? element) (pair? (car element))
              (eq? (caar element) 'target-rule))
         (let* ((rule-data (cdar element))
                (target (car rule-data))
                (prereqs (cdr rule-data))
                (recipes (cdr element)))
           (format output "(: ~s '(~a)\n" target 
                   (string-join (map (lambda (p) (format #f "~s" p)) prereqs) " "))
           (for-each
            (lambda (recipe)
              (format output "  ~s\n" recipe))
            recipes)
           (display ")\n\n" output)))
        
        ;; Suffix rule with recipes
        ((and (pair? element) (pair? (car element))
              (eq? (caar element) 'suffix-rule))
         (let* ((rule-str (cdar element))
                (recipes (cdr element))
                (parts (parse-suffix-rule rule-str)))
           (when parts
             (let ((src-ext (car parts))
                   (tgt-ext (cdr parts)))
               (format output "(-> ~s ~s\n" src-ext tgt-ext)
               (for-each
                (lambda (recipe)
                  (format output "  ~s\n" recipe))
                recipes)
               (display ")\n\n" output)))))))
     elements)
    
    ;; Write footer
    (display "(execute)\n" output)
    (get-output-string output)))

;; Parse suffix rule string like ".c.o" into source and target extensions
(define (parse-suffix-rule rule-str)
  "Parse a suffix rule string like '.c.o' into ('.c' . '.o')."
  (if (and (> (string-length rule-str) 2)
           (char=? (string-ref rule-str 0) #\.))
      (let* ((rest (substring rule-str 1))
             (dot-pos (string-index rest #\.)))
        (if dot-pos
            (cons (string-append "." (substring rest 0 dot-pos))
                  (string-append "." (substring rest (+ dot-pos 1))))
            #f))
      #f))

;; Main conversion function
(define (makefile->potato-make filename)
  "Parse a POSIX makefile and convert it to potato-make code."
  (let ((elements (parse-makefile filename)))
    (elements->potato-make elements)))
