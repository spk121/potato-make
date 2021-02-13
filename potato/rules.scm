(define-module (potato rules)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (potato exceptions)
  #:use-module (potato builtins)
  #:use-module (potato makevars)
  #:use-module (potato text)
  #:export(<target-rule>
           <suffix-rule>
           <node>
           %target-rules
           %suffix-rules
           initialize-rules
           first-target-rule-name
           install-alternate-system-driver
           target-rule :
           suffix-rule ->
           target-name         $@
           newer-prerequisites $?
           primary-prerequisite $<
           target-basename     $*
           prerequisites       $^
           build
           string-compose         ~
           silent-compose         ~@
           always-execute-compose ~+
           ignore-error-compose   ~-
           ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBALS

(define %ignore-errors? #f)
(define %continue-on-error? #f)
(define %no-execution? #f)
(define %terse? #f)
(define %verbose? #f)
(define %ascii? #f)
(define %top-level-targets '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS

(define (basename str)
  "Strip off the '.ext' part of a filename string."
  (let ((idx (string-index-right str #\.)))
    (if idx
        (substring str 0 idx)
        str)))

(define (base-compose . args)
  "Returns a lambda that appends args together as a string,
adding intermediate spaces.  If an arg is a procedure,
it is evaluated."
  (lambda ()
    ;; Loop over all the args, appending them together as a
    ;; string. Try to be smart about the types of args.
    (let loop ((args args)
               (result ""))
      (cond
       ((null? args)
        result)
       (else
        (let ((arg (car args))
              (effective-arg #f))
          (cond
           ((procedure? arg)
            (format #t "BLAMMO ~s ~s ~%" arg (arg))
            (set! effective-arg (arg))
            
            (unless (string? effective-arg)
              (bad-proc-output "~" arg))
            )
           ((string? arg)
            (set! effective-arg arg))
           (else
            ;; Not a string or procedure?
            ;; Let's just write it, I guess. YOLO!
            (set! effective-arg
              (format #f "~a" arg))))

          ;; Loop, only adding spaces as necessary
          (let ((need-a-space?
                 (and (not (string-null? result))
                      (not (string-null? effective-arg)))))
            (loop
             (cdr args)
             (string-append
              result
              (if need-a-space? " " "")
              effective-arg)))))))))

(define (string-compose . args)
  (cons 'default (apply base-compose args)))

(define ~ string-compose)

(define (ignore-error-compose . args)
  (cons 'ignore-error (apply base-compose args)))

(define ~- ignore-error-compose)

(define (silent-compose . args)
  (cons 'silent (apply base-compose args)))

(define ~@ silent-compose)

(define (always-execute-compose . args)
  (cons 'always-execute (apply base-compose args)))

(define ~+ always-execute-compose)

(define (regular-file? filename)
  (let ((st (stat filename #f)))
    (eq? (stat:type st) 'regular)))

(define (compute-mtime filename)
  (let ((st (stat filename #f)))
    (+ (* 1000000000 (stat:mtime st))
       (stat:mtimensec st))))

(define %system-proc system)

(define (install-alternate-system-driver proc)
  "Give a procure to use rather than the standard
'system' procedure."
  (unless (procedure? proc)
    (not-a-procedure "install-alternate-system-driver" proc))
  (set! %system-proc proc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TARGET STRUCT AND METHODS

(define-record-type <target-rule>
  (make-target-rule name prerequisites recipes priority)
  target-rule?
  ;; A filename, for real targets, or just a name for phony targets
  (name          target-rule-get-name target-rule-set-name!)
  ;; A list of filenames and/or phony targets that have target rules
  (prerequisites target-rule-get-prerequisites
                 target-rule-set-prerequisites!)
  ;; A list of strings or procedures
  (recipes       target-rule-get-recipes
                 target-rule-set-recipes!)
  ;; 1 = script-defined. 2 = built-in 
  (priority      target-rule-get-priority
                 target-rule-set-priority!))

;; List of all target rules in order of importance
(define %target-rules '())

(define* (target-rule name #:optional (prerequisites '()) #:rest recipes)
  "Register a new target rule"

  (when %verbose?
    (format #t "Defining target rule: ~A ~A ~A~%" prerequisites (right-arrow) name))
  ;; If any recipes are raw strings, we need to make them into
  ;; (cons 'default string)
  (let ((recipes2
         (map (lambda (recipe)
                (cond
                 ((pair? recipe)
                  recipe)
                 ((string? recipe)
                  (cons 'default recipe))
                 (else
                  (error "bad recipe for target rule"))))
              recipes)))

    (let ((rule (make-target-rule name prerequisites recipes2 1)))
      ;; Add to %target-rules
      (set! %target-rules (cons rule %target-rules)))))

;; Alias
(define : target-rule)

(define (first-target-rule-name)
  (if (null? %target-rules)
      #f
      ;; else
      (target-rule-get-name (car %target-rules))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SUFFIX STRUCT AND METHODS

(define-record-type <suffix-rule>
  (make-suffix-rule source-suffix target-suffix recipes priority)
  suffix-rule?
  ;; A string, usually like ".c". Or a string->string proc.
  (source-suffix  suffix-rule-get-source
                  suffix-rule-set-source)
  ;; A string, usually like ".o". Or a string->bool proc.
  (target-suffix  suffix-rule-get-target
                  suffix-rule-set-suffix!)
  ;; A list of strings or procedures
  (recipes       suffix-rule-get-recipes
                 suffix-rule-set-recipes!)
  ;; 1 = script-defined. 2 = built-in 
  (priority      suffix-rule-get-priority
                 suffix-rule-set-priority!))

;; The list of all registered suffix rules in order of importance
(define %suffix-rules '())

(define (suffix-rule source target . recipes)
  "Register a suffix rule"
  
  ;; FIXME: Typecheck
  (when %verbose?
    (format #t "Defining suffix rule: ~A ~A ~A~%" source (right-arrow) target))
  ;; If any recipes are raw strings, we need to make them into
  ;; (cons 'default string)
  (let ((recipes2
         (map (lambda (recipe)
                (cond
                 ((pair? recipe)
                  recipe)
                 ((string? recipe)
                  (cons 'default recipe))
                 (else
                  (error "bad recipe for target rule"))))
              recipes)))
  
    (let ((rule (make-suffix-rule source target recipes 1)))
      (set! %suffix-rules (cons rule %suffix-rules)))))

;; Alias
(define -> suffix-rule)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NODE STRUCT AND METHODS

(define-record-type <node>
  (make-node name parent status)
  node?
  ;; For a real target, this is a filename. For a phony target it is
  ;; just a label.
  (name      node-get-name node-set-name!)
  ;; A <node> which is the parent of this node, or #f.
  (parent    node-get-parent node-set-parent!)
  ;; If 'name' is a regular file, mtime holds its last modification
  ;; time in nanoseconds since the epoch. If 'name' does not exist,
  ;; _mtime is #f.
  (mtime     node-get-mtime node-set-mtime!)
  ;; One of 'pass, 'fail, or 'undetermined
  (status    node-get-status node-set-status!)
  ;; A list of rules
  (rules     node-get-rules node-set-rules!)
  (children  node-get-children node-set-children!)
  ;; Determines how many children must pass for the parent
  ;; to be evaluated.  Either 'or or 'and.
  (logic     node-get-logic node-set-logic!)
  )

(define (has-rules? node)
  (not (null? (node-get-rules node))))

(define (set-fail! node)
  (node-set-status! node 'fail))

(define (set-pass! node)
  (node-set-status! node 'pass))

(define (failed? node)
  (eqv? (node-get-status node) 'fail))

(define (passed? node)
  (eqv? (node-get-status node) 'pass))

(define (leaf-node? node)
  (null? (node-get-children node)))

(define (undetermined? node)
  (eq? (node-get-status node) 'undetermined))

(define (any-child-has-passed? node)
  (let ((children (node-get-children node)))
    (if (null? children)
        #f
        (any passed? children))))

(define (every-child-has-passed? node)
  (let ((children (node-get-children node)))
    (if (null? children)
        #f
        (every passed? children))))

(define (any-child-has-failed? node)
  (let ((children (node-get-children node)))
    (if (null? children)
        #f
        (any failed? children))))

(define (every-child-has-failed? node)
  (let ((children (node-get-children node)))
    (if (null? children)
        #f
        (every failed? children))))

(define (children-complete? node)
  (cond
   ((leaf-node? node)
    #t)
   ((eqv? 'and (node-get-logic node))
    (or (every-child-has-passed? node)
        (any-child-has-failed? node)))
   ((eqv? 'or (node-get-logic node))
    (or (every-child-has-failed? node)
        (any-child-has-passed? node)))
   (else
    #f)))

(define (children-passed? node)
  (cond
   ((null? (node-get-children node))
    #t)
   ((eqv? 'and (node-get-logic node))
    (every-child-has-passed? node))
   ((eqv? 'or (node-get-logic node))
    (any-child-has-passed? node))
   (else
    #f)))

(define (get-next-child node)
  "Return the first child node that is not yet PASS or FAIL"
  (let ((children (node-get-children node)))
    (if (null? children)
        #f
    
        (any (lambda (child)
               (if (eqv? (node-get-status child) 'undetermined)
                   child
                   #f))
             children))))

(define (has-parent? node)
  (if (node-get-parent node)
      #t
      #f))

(define (get-parent node)
  (node-get-parent node))

(define (up-to-date? node)
  "Checks if node is up to date:
- it has an mtime
- all its children have mtimes
- its mtime is older than the mtime of its children"
  (let ((children (map node-get-mtime (node-get-children node)))
        (parent (node-get-mtime node)))
    (if (every (lambda (child)
                 (and (integer? parent)
                      (integer? child)
                      (>= parent child)))
               children)
        #t
        #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUTOMATIC VARIABLES

(define target-name 'unspecified)
(define target-basename 'unspecified)
(define prerequisites '())
(define primary-prerequisite 'unspecified)
(define newer-prerequisites '())

(define $@  (lambda () target-name))
(define $*  (lambda () target-basename))
(define $<  (lambda () primary-prerequisite))
(define $$? (lambda () newer-prerequisites))
(define $?  (lambda () (apply string-compose newer-prerequisites)))
(define $$^ (lambda () prerequisites))
(define $^  (lambda () (apply string-compose prerequisites)))

(define (target-rule-prep-automatic-variables node rule)
  (set! target-name (node-get-name node))
  (set! target-basename (basename target-name))
  (set! prerequisites (target-rule-get-prerequisites rule))
  (set! primary-prerequisite (if (null? prerequisites) #f (car prerequisites)))
  (set! newer-prerequisites
    ;; If this node doesn't have a real file attached, then all
    ;; prerequistes are "newer".
    (if (not (node-get-mtime node))
        prerequisites
        ;; Prerequisites that have no mtime or a higher mtime are
        ;; "newer".
        (filter-map
         (lambda (name)
           (cond
            ((and (file-exists? name)
                  (regular-file? name)
                  (>= (node-get-mtime node) (compute-mtime name)))
             name)
            ((not (file-exists? name))
             name)
            (else
             #f)))
         prerequisites))))

(define (suffix-rule-prep-automatic-variables node rule)
  (set! target-name (node-get-name node))
  (set! target-basename (basename target-name))
  (set! primary-prerequisite (string-append target-basename (suffix-rule-get-source rule)))
  (set! prerequisites (list primary-prerequisite))
  (set! newer-prerequisites
    ;; If this node doesn't have a real file attached, then the
    ;; prerequisite is newer.
    (if (not (node-get-mtime node))
        (list primary-prerequisite)
        ;; Prerequisites that have no mtime or a higher mtime are
        ;; "newer".
        (cond
         ((and (file-exists?  primary-prerequisite)
               (regular-file? primary-prerequisite)
               (> (node-get-mtime node) (compute-mtime primary-prerequisite)))
          (list primary-prerequisite))
         (else
          '())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MIXED METHODS
;; requiring more than one of node, automatic variables, suffix rules
;; and target rules

(define (add-builtins)
  (-> ".c" ""
      (~ ($ CC) ($ CFLAGS) ($ LDFLAGS) "-o" $@ $<))
  (-> ".f" ""
      (~ ($ FC) ($ FFLAGS) ($ LDFLAGS) "-o" $@ %<))
  (-> ".sh" ""
      (~ "cp" $< $@)
      (~ "chmod a+x" $< $@))
  (-> ".c" ".o"
      (~ ($ CC) ($ CFLAGS) "-c" $<))
  (-> ".f" ".o"
      (~ ($ FC) ($ FFLAGS) ",c" $<))
  (-> ".y" ".o"
      (~ ($ YACC) ($ YFLAGS) $<))
  (-> ".l" ".o"
      (~ ($ LEX) ($ LFLAGS) $<)
      (~ ($ CC) ($ CFLAGS) "-c lex.yy.c")
      "rm -f lex.yy.c"
      (~ "mv lex.yy.o" $@))
  (-> ".y" ".c"
      (~ ($ YACC) ($ YFLAGS) $<)
      (~ "mv y.tab.c" $@))
  (-> ".l" ".c"
      (~ ($ LEX) ($ LDFLAGS) $<)
      (~ "mv lex.yy.c" $@))
  (-> ".scm" ".go"
      (~ ($ GUILD) "compile" ($ GFLAGS) $<)))

(define (run-rules! node)
  "Runs the rules associated with this node.  If this node has *and*
logic, it runs the recipes one by one, quitting on the first failure.
If this rule has *or* logic, it runs them quitting on the first
success."
  (let ((rules (node-get-rules node)))
    (when (null? rules)
      (error "no rules"))

    (let ((logic (node-get-logic node)))
      (let loop ((rule (car rules))
                 (rest (cdr rules)))
        (cond
         ((target-rule? rule)
          (target-rule-prep-automatic-variables node rule)
          (run-recipes! node (target-rule-get-recipes rule)))
         ((suffix-rule? rule)
        (suffix-rule-prep-automatic-variables node rule)
        (run-recipes! node (suffix-rule-get-recipes rule)))
         (else
          (error "bad rule")))
        
        (let ((status (node-get-status node)))
          (cond
           ((or (and (eq? 'and logic)
                     (eq? 'fail status))
                (and (eq? 'or logic)
                     (eq? 'pass status))
                (null? rest))
            ;; We're done
            status)
           (else
            (loop (car rest) (cdr rest)))))))))

(define (run-recipes! node recipes)
  "Runs the recipes on this node, one by one.  Recipes are either
strings, procedures that return strings, or generic procedures.  If a
failure condition happens, mark the node as having failed."
  (when (null? recipes)
    (error "no recipe"))
  (let loop ((opt/recipe (car recipes))
             (rest (cdr recipes)))
    (let ((opt (car opt/recipe))
          (recipe (cdr opt/recipe)))
      ;; Recipes are either
      ;; - strings to pass to system
      ;; - procedures that return a string which is passed
      ;;   to system
      ;; - procedures (that don't return a string) that are executed
      ;;   that pass unless they return #f
      ;; OPT is one of 'default, 'ignore, 'silent
      (cond
       ((string? recipe)
        (format #t "[SYSTEM] ~A~%" recipe)
        (let ((retval (%system-proc recipe)))
          (format #t " --> ~S~%" retval)
          (unless (zero? retval)
            (set-fail! node))))
       
       ((procedure? recipe)
        (if (procedure-name recipe)
            (format #t "[PROC] ~A~%" (procedure-name recipe))
            (format #t "[PROC] ~%"))
        (let ((retval (recipe)))
          (format #t " --> ~S~%" retval)
          
          (cond
           ((eqv? retval #f)
            (set-fail! node))
           
           ;; If a procedure returns a string, that string gets
           ;; processed by system.
           ((string=? retval)
            (format #t "[SYSTEM] ~A~%" retval)
            (let ((retval2 (%system-proc retval)))
              (format #t " --> ~S~%" retval2)
              (unless (zero? retval2)
                (set-fail! node)))))))
       
       (else
        ;; Not a string or procedure
        (error "bad rule")))
      
      
      (cond
       ((failed? node)
        ;; quit
        )
       ((null? rest)
        (set-pass! node))
       (else
        ((loop (car rest)
               (cdr rest)))))))
  
  (when (passed? node)
    (let ((name (node-get-name node)))
      (when (and (file-exists? name)
                 (regular-file? name))
        (node-set-mtime! node (compute-mtime name))))))

(define (run-default-recipe! node)
  "The default recipe passes if the file exists"
  (let ((name (node-get-name node)))
    (if (and (file-exists? name)
             (regular-file? name))
        (begin
          (set-pass! node)
          (node-set-mtime! node (compute-mtime name)))
        ;; else
        (set-fail! node))))

;; Start at root

;; If cur is UNDETERMINED, find a leaf that is UNDETERMINED.
;; Descent to first leaf.
;; If self's mtime is earlier than parent's mtime, mark self as PASS.
;; Elif self has rules, run rules and mark self PASS/FAIL.
;; Else self has no mtime or rules, so mark self FAIL.
;; Go to parent.

;; IF PASS or FAIL, go to parent

;; IF UNDETERMINED do these...

;; Are we done with the children?
;; If AND rule and one child is FAIL, stop
;; If OR rule and one child is PASS, stop
;; If no children left, stop
;; Else keep going

;; Did the children pass?
;; IF AND rule and all children are PASS, true
;; IF OR rule an one child is PASS, true
;; Otherwise, false

;; If the children FAIL, cur is FAIL
;; If the children PASS, run rules and mark self PASS/FAIL
;; Go to parent

;; 3 failures
;; - If anything fails, stop immediately
;; - If anything fails, searching through tree
;; - Ignore errors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LET'S GO! 

(define (initialize-rules targets builtins? ignore-errors? continue-on-error? no-execution? terse? verbose? ascii?)
  (set! %target-rules '())
  (set! %suffix-rules '())
  (set! %top-level-targets targets)
  (set! %ignore-errors? ignore-errors?)
  (set! %continue-on-error? continue-on-error?)
  (set! %no-execution? no-execution?)
  (set! %terse? terse?)
  (set! %verbose? verbose?)
  (set! %ascii? ascii?)
  (when builtins?
    (add-builtins)))

(define (create-node name parent)
  "Constructs a tree of nodes, with name as the root node."
  (let ((node (make-node name parent 'undetermined)))
    (node-set-children! node '())
    ;; FIXME: here check that this name differs from all ancenstor's
    ;; names
    
    ;; Try to the file's modification time.
    (when (file-exists? name)
      (when (not (regular-file? name))
        (not-a-regular-file "create-node" name))
      (when (not (access? name R_OK))
        (no-read-access-to-file "create-node" name))
      (node-set-mtime! node (compute-mtime name)))
    
    ;; Search for matching target rule.
    (when (not (null? %target-rules))
      (let loop ((rule (car %target-rules))
                 (rest (cdr %target-rules)))
        
        ;; N.B: here we assume target rule names and
        ;; predicates are exclusively strings.
        (if (string=? name (target-rule-get-name rule))
            (begin
              ;; OK we have a matching rule
              (node-set-rules! node (list rule))
              (node-set-logic! node 'and)
              ;; For target-rules, the prerequisites comes from the
              ;; rule itself.
              
              ;; Oooh, recursion!
              (node-set-children! node
                                  (map (lambda (prereq)
                                         (create-node prereq node))
                                       (target-rule-get-prerequisites rule))))
            ;; else
            (if (not (null? rest))
                (loop (car rest) (cdr rest))
                ;; else, no matching rule found
                (node-set-rules! node '())))))

    #|
    ;; If no rule found so far, search for suffix rules.
    (when (null? (node-get-rules node))
    (for-each
    (lambda (rule)
    (let ((targ (suffix-rule-get-target rule)))
    (when (or
    ;; string suffix
    (and (string? targ)
    (string-suffix? targ name))
    ;; procedure suffix
    (and (procedure? targ)
    (targ name)))
    ;; For suffix rules, there will be exactly one child per
    ;; rule and the name of the child is constructed from a
    ;; suffix and the parent's name.
    (node-set-rules! node (cons rule (node-get-rules node)))
    (node-set-logic! node 'or)
    (let* ((src (suffix-rule-get-source rule))
    (prereq
    (if (string? src)
    (string-append
    (string-drop-right name (string-length src))
    src)
    ;; else, src is a conversion func.
    (src name))))
    ;; Note the recursion here.
    (node-set-children! node
    (cons (create-node prereq node)
    (node-get-children node)))))))
    %suffix-rules))

    ;; First matching rule has highest priority
    (node-set-rules! node (reverse (node-get-rules node)))
    (node-set-children! node (reverse (node-get-children node)))
    |#
    ;; And node is ready to go
    node))

(define (build root)
  "Give a tree of <node>, this executes the recipes therein."
  (let ((tree (create-node root #f)))
    (let ((node tree))
      (while #t
        (if (undetermined? node)
            (begin
              (if (children-complete? node)
                  (if (children-passed? node)
                      (if (up-to-date? node)
                          (set-pass! node)
                          ;; else, not up to date
                          (if (has-rules? node)
                              (run-rules! node)
                              ;; else, no recipe exists
                              (run-default-recipe! node)))
                      ;; else, children have failed
                      (set-fail! node))
                  ;; else, children aren't complete
                  (set! node (get-next-child node))))
            ;; else, this node is determined
            (begin
              (if (and (not %ignore-errors?) (failed? node))
                  (break)
                  ;; else not failed
                  (if (has-parent? node)
                      (set! node (get-parent node))
                      ;; else, there is no parent to this node
                      (break)))))))))


