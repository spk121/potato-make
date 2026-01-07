(define-module (potato makefile-parser)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:export (parse-makefile
            makefile->potato-make
            parse-makefile-line
            parse-suffix-rule
            parse-makefile-from-port))

;; Parser for POSIX Makefiles that converts them to potato-make code

;; Helper to check if target is a special target
(define (special-target? target)
  "Check if target is a special target (.POSIX, .PHONY, .IGNORE, .DEFAULT)"
  (member target '(".POSIX" ".PHONY" ".IGNORE" ".DEFAULT")))

;; Validate .POSIX special target
(define (validate-posix-target target prereqs first-non-comment-seen?)
  "Validate that .POSIX appears as first non-comment line with no prerequisites"
  (when (equal? target ".POSIX")
    (unless (and (not first-non-comment-seen?)
                 (null? prereqs))
      (error "Error: .POSIX must be the first non-comment line with no prerequisites or commands"))))

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
   
   ;; Variable assignment - check for =, :=, ?=, += operators
   ;; Must check this before target rules to handle := correctly
   ((and (string-index line #\=)
         (not continuation?))
    (let* ((eq-pos (string-index line #\=))
           (colon-pos (string-index line #\:))
           (lhs-raw (substring line 0 eq-pos))
           (rhs-raw (substring line (+ eq-pos 1)))
           (lhs-trimmed (string-trim-right lhs-raw))
           (lhs-len (string-length lhs-trimmed))
           (op-char (and (> lhs-len 0)
                         (string-ref lhs-trimmed (- lhs-len 1))))
           ;; Check for :=, ?=, += operators (colon/question/plus immediately before =)
           (is-special-op? (and op-char
                                (or (char=? op-char #\:)
                                    (char=? op-char #\?)
                                    (char=? op-char #\+))))
           ;; Determine if this is a variable assignment:
           ;; - If it's a special operator (:=, ?=, +=), it's always a variable
           ;; - If there's no colon, it's a variable
           ;; - If = comes before : (and not special op), it's a variable assignment
           ;; - If : comes before = (and not special op), it's a target rule like "target: prereq=$(VAR)"
           (is-var-assignment? (or is-special-op?
                                   (not colon-pos)
                                   (< eq-pos colon-pos))))
      (if is-var-assignment?
          (let* ((var-name-raw (if is-special-op?
                                   (substring lhs-trimmed 0 (- lhs-len 1))
                                   lhs-trimmed))
                 (var-name (string-trim-both var-name-raw))
                 (var-value (string-trim-both rhs-raw)))
            (cons 'variable (cons var-name var-value)))
          ;; Not a variable assignment - will be handled by target rule below
          (cons 'unknown line))))
   
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

;; Helper function renamed for consistency with Guile conventions
(define (string-split-first str ch)
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
             (current-recipes '())
             (first-non-comment-seen? #f))
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
            ;; Process the parsed continuation line properly
            (case (car parsed)
              ((recipe)
               ;; Recipe line - add to current rule's recipes
               (if current-rule
                   (loop elements current-rule (cons (cdr parsed) current-recipes) first-non-comment-seen?)
                   ;; Recipe without a rule - skip
                   (loop elements #f '() first-non-comment-seen?)))
              
              ((target-rule suffix-rule)
               ;; Validate .POSIX if present
               (when (eq? (car parsed) 'target-rule)
                 (validate-posix-target (cadr parsed) (cddr parsed) first-non-comment-seen?))
               
               ;; New rule - save previous rule if any
               (if current-rule
                   (loop (cons (cons current-rule (reverse current-recipes)) elements)
                         parsed '() #t)
                   (loop elements parsed '() #t)))
              
              ((variable comment blank)
               ;; Non-rule line - save any current rule first
               (let ((new-first-seen? (if (eq? (car parsed) 'variable) #t first-non-comment-seen?)))
                 (if current-rule
                     (loop (cons (cons current-rule (reverse current-recipes))
                                (cons parsed elements))
                           #f '() new-first-seen?)
                     (loop (cons parsed elements) #f '() new-first-seen?))))
              
              (else
               ;; Unknown - skip
               (loop elements current-rule current-recipes first-non-comment-seen?))))))
       
       (else
        (let ((parsed (parse-makefile-line line #f)))
          (case (car parsed)
            ((recipe)
             ;; Recipe line - add to current rule's recipes
             (if current-rule
                 (loop elements current-rule (cons (cdr parsed) current-recipes) first-non-comment-seen?)
                 ;; Recipe without a rule - skip
                 (loop elements #f '() first-non-comment-seen?)))
            
            ((target-rule suffix-rule)
             ;; Validate .POSIX if present
             (when (eq? (car parsed) 'target-rule)
               (validate-posix-target (cadr parsed) (cddr parsed) first-non-comment-seen?))
             
             ;; New rule - save previous rule if any
             (if current-rule
                 (loop (cons (cons current-rule (reverse current-recipes)) elements)
                       parsed '() #t)
                 (loop elements parsed '() #t)))
            
            ((variable comment blank)
             ;; Non-rule line - save any current rule first
             (let ((new-first-seen? (if (eq? (car parsed) 'variable) #t first-non-comment-seen?)))
               (if current-rule
                   (loop (cons (cons current-rule (reverse current-recipes))
                              (cons parsed elements))
                         #f '() new-first-seen?)
                   (loop (cons parsed elements) #f '() new-first-seen?))))
            
            (else
             ;; Unknown - skip
             (loop elements current-rule current-recipes first-non-comment-seen?)))))))))

;; Parse a makefile from a filename
(define (parse-makefile filename)
  "Parse a POSIX makefile from a file and return parsed elements."
  (call-with-input-file filename parse-makefile-from-port))

;; Constants for recipe prefixes
(define %ignore-error-prefix "~~-")
(define %default-prefix "~~")

;; Convert a make recipe string to potato-make DSL
(define (convert-recipe-to-potato-make recipe)
  "Convert a POSIX make recipe to potato-make DSL.
   Converts $(VAR) to ($ VAR) and automatic variables like $@ to $@ DSL."
  ;; For simple cases, we'll keep recipes as strings
  ;; More complex conversion could parse and reconstruct using ~ syntax
  ;; This is a basic implementation
  (let ((converted recipe))
    ;; Convert $(VAR) to ($ VAR) references, allowing variable names starting with digits
    (set! converted (regexp-substitute/global #f "\\$\\(([A-Za-z0-9_]+)\\)" 
                                               converted
                                               'pre "($ " 1 ")" 'post))
    ;; For now, keep automatic variables as-is since they work in potato-make
    converted))

;; Collect special targets from elements
(define (collect-special-targets elements)
  "Collect and organize special targets (.PHONY, .IGNORE, .DEFAULT) from parsed elements.
   Returns an association list with keys: phony-targets, ignore-targets, default-rule"
  (let ((phony-targets '())
        (ignore-targets '())
        (ignore-all? #f)
        (default-rule #f))
    
    (for-each
     (lambda (element)
       (when (and (pair? element) (pair? (car element))
                 (eq? (caar element) 'target-rule))
         (let* ((rule-data (cdar element))
                (target (car rule-data))
                (prereqs (cdr rule-data))
                (recipes (cdr element)))
           
           (cond
            ;; .POSIX - already validated, just skip it
            ((equal? target ".POSIX")
             #t)
            
            ;; .PHONY - collect phony targets
            ((equal? target ".PHONY")
             (unless (null? prereqs)
               (set! phony-targets (append phony-targets prereqs))))
            
            ;; .IGNORE - collect targets to ignore errors
            ((equal? target ".IGNORE")
             (if (null? prereqs)
                 (set! ignore-all? #t)
                 (set! ignore-targets (append ignore-targets prereqs))))
            
            ;; .DEFAULT - validate and store default rule
            ((equal? target ".DEFAULT")
             (when (not (null? prereqs))
               (error "Error: .DEFAULT must not have prerequisites"))
             (when (null? recipes)
               (error "Error: .DEFAULT must have commands"))
             (set! default-rule recipes))))))
     elements)
    
    (list (cons 'phony-targets phony-targets)
          (cons 'ignore-targets ignore-targets)
          (cons 'ignore-all? ignore-all?)
          (cons 'default-rule default-rule))))

;; Check if a target is in the phony list
(define (phony-target? target phony-list)
  (member target phony-list))

;; Check if a target should ignore errors
(define (ignore-errors-target? target ignore-list ignore-all?)
  (or ignore-all? (member target ignore-list)))

;; Convert parsed elements to potato-make code
(define (elements->potato-make elements)
  "Convert parsed makefile elements to potato-make Scheme code as a string."
  (let ((output (open-output-string))
        (special-info (collect-special-targets elements)))
    
    ;; Extract special target info
    (let ((phony-targets (cdr (assq 'phony-targets special-info)))
          (ignore-targets (cdr (assq 'ignore-targets special-info)))
          (ignore-all? (cdr (assq 'ignore-all? special-info)))
          (default-rule (cdr (assq 'default-rule special-info))))
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
        ;; Variable assignment - sanitize variable name to prevent code injection
        ((and (pair? element) (eq? (car element) 'variable))
         (let* ((var-name-raw (cadr element))
                (var-value (cddr element))
                ;; Sanitize: only allow alphanumeric and underscore characters
                (var-name (if (string-every (lambda (c) 
                                              (or (char-alphabetic? c)
                                                  (char-numeric? c)
                                                  (char=? c #\_))) 
                                            var-name-raw)
                              var-name-raw
                              (begin
                                (format (current-error-port) 
                                        "Warning: Skipping invalid variable name: ~s~%" 
                                        var-name-raw)
                                #f))))
           (when var-name
             (format output "(:= ~a ~s)\n" var-name var-value))))
        
        ;; Comment - convert # to ;; for proper Scheme syntax
        ((and (pair? element) (eq? (car element) 'comment))
         (let* ((raw (cdr element))
                (text (if (and (string? raw)
                               (> (string-length raw) 0)
                               (char=? (string-ref raw 0) #\#))
                          (substring raw 1 (string-length raw))
                          raw)))
           (format output ";;~a\n" text)))
        
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
           
           ;; Skip special targets - they are handled separately
           (unless (special-target? target)
             ;; Add comment annotations for phony and ignore targets
             (when (phony-target? target phony-targets)
               (format output ";; ~a is a phony target (always out-of-date)~%" target))
             (when (ignore-errors-target? target ignore-targets ignore-all?)
               (format output ";; ~a ignores command errors~%" target))
             
             (format output "(: ~s '(~a)" target 
                     (string-join (map (lambda (p) (format #f "~s" p)) prereqs) " "))
             (if (null? recipes)
                 (display ")\n\n" output)
                 (begin
                   (display "\n" output)
                   (for-each
                    (lambda (recipe)
                      (let* ((converted (convert-recipe-to-potato-make recipe))
                             ;; Use ~- (ignore errors) if target is in ignore list or ignore-all
                             (recipe-prefix (if (ignore-errors-target? target ignore-targets ignore-all?)
                                               %ignore-error-prefix
                                               %default-prefix)))
                        (format output "  (~a ~s)\n" recipe-prefix converted)))
                    recipes)
                   (display ")\n\n" output))))))
        
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
                  (let ((converted (convert-recipe-to-potato-make recipe)))
                    (format output "  (~~ ~s)\n" converted)))
                recipes)
               (display ")\n\n" output)))))))
     elements)
    
      ;; Handle .DEFAULT rule if present
      (when default-rule
        (display ";; .DEFAULT rule for targets without other rules\n" output)
        (display ";; In .DEFAULT, $< evaluates to the current target name\n" output)
        (display ";; Note: potato-make doesn't directly support .DEFAULT\n" output)
        (display ";; This is a placeholder comment for the .DEFAULT rule\n" output)
        (for-each
         (lambda (recipe)
           (let ((converted (convert-recipe-to-potato-make recipe)))
             (format output ";; DEFAULT RECIPE: ~a\n" converted)))
         default-rule)
        (display "\n" output))
    
    ;; Write footer
    (display "(execute)\n" output)
    (get-output-string output))))

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
