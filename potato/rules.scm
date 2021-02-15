(define-module (potato rules)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 optargs)
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
           target-rule         :
           suffix-rule         ->
           target-name         $@
           newer-prerequisites $? $$?
           primary-prerequisite $<
           target-basename     $*
           prerequisites       $^ $$^
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
(define %verbosity 2)
(define %ascii? #f)
(define %top-level-targets '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS


(define (basename str)
  "Strip off the '.ext' part of a filename string."
  (unless (string? str)
    (scm-error 'wrong-type-arg "basename" "Not a string: ~S" (list str) #f))

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
            (set! effective-arg (arg))

            (unless (string? effective-arg)
              (bad-proc-output "~" arg)))

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
  (unless (string? filename)
    (scm-error 'wrong-type-arg "regular-file?" "Not a string: ~S" (list filename) #f))

  (let ((st (stat filename #f)))
    (eq? (stat:type st) 'regular)))

(define (compute-mtime filename)
  (unless (string? filename)
    (scm-error 'wrong-type-arg "regular-file?" "Not a string: ~S" (list filename) #f))

  (let ((st (stat filename #f)))
    (+ (* 1000000000 (stat:mtime st))
       (stat:mtimensec st))))

(define %system-proc system)

(define (install-alternate-system-driver proc)
  "Give a procure to use rather than the standard 'system' procedure,
installs it as the system driver.  Returns the old system driver."
  (unless (procedure? proc)
    (scm-error 'wrong-type-arg "install-alternate-system-driver" "Not a procedure: ~S" (list proc) #f))
  (let ((old-proc %system-proc))
    (set! %system-proc proc)
    old-proc))

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

  (when (>= %verbosity 3)
    (if (null? prerequisites)
        (format #t "Target rule: ~a~A~a~%~!" (lquo) name (rquo))
        (format #t "Target rule: ~a~A~a ~A ~A~%~!" (lquo) name (rquo) (left-arrow) prerequisites)))

  ;; Empty recipes is shorthand for a recipe that always passes.
  (when (null? recipes)
    (set! recipes (list #t)))

  ;; If any recipes are raw strings, we need to make them into
  ;; (cons 'default string)
  (let ((recipes2
         (map (lambda (recipe)
                (cond
                 ((pair? recipe)
                  recipe)
                     (else
                      (cons 'default recipe))))
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
      (target-rule-get-name (last %target-rules))))

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
  (when (>= %verbosity 3)
    (format #t "Suffix rule: ~a~A~a ~A ~a~A~a~%~!"
            (lquo) source (rquo) (right-arrow) (lquo) target (rquo)))
  
  ;; If any recipes are raw strings, we need to make them into
  ;; (cons 'default string)
  (let ((recipes2
         (map (lambda (recipe)
                (cond
                 ((pair? recipe)
                  recipe)
                 (else
                  (cons 'default recipe))))
              recipes)))

    (let ((rule (make-suffix-rule source target recipes2 1)))
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
  ;; Either 'target or 'suffix or 'default
  (rule-type     node-get-rule-type node-set-rule-type!)
  ;; A list of rules
  (rules     node-get-rules node-set-rules!)
  (children  node-get-children node-set-children!)
  )

(define (using-target-rule? node)
  (eq? 'target (node-get-rule-type node)))
(define (using-suffix-rules? node)
  (eq? 'suffix (node-get-rule-type node)))
(define (using-default-rule? node)
  (eq? 'default (node-get-rule-type node)))

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
  (unless (node? node)
    (scm-error 'wrong-type-arg "any-child-has-passed?" "Not a node: ~S" (list node) #f))
  (when (null? (node-get-children node))
    (scm-error 'misc-error "any-child-has-passed?" "Node ~a has no children"
               (list (node-get-name node)) #t))

  (let ((children (node-get-children node)))
    (any passed? children)))

(define (every-child-has-passed? node)
  (unless (node? node)
    (scm-error 'wrong-type-arg "every-child-has-passed?" "Not a node: ~S" (list node) #f))
  (when (null? (node-get-children node))
    (scm-error 'misc-error "every-child-has-passed?" "Node ~a has no children"
               (list (node-get-name node)) #t))

  (let ((children (node-get-children node)))
    (every passed? children)))

(define (any-child-has-failed? node)
  (unless (node? node)
    (scm-error 'wrong-type-arg "any-child-has-failed?" "Not a node: ~S" (list node) #f))
  (when (null? (node-get-children node))
    (scm-error 'misc-error "any-child-has-failed?" "Node ~a has no children"
               (list (node-get-name node)) #t))

  (let ((children (node-get-children node)))
    (any failed? children)))

(define (every-child-has-failed? node)
  (unless (node? node)
    (scm-error 'wrong-type-arg "every-child-has-failed?" "Not a node: ~S" (list node) #f))
  (when (null? (node-get-children node))
    (scm-error 'misc-error "every-child-has-failed?" "Node ~a has no children"
               (list (node-get-name node)) #t))

  (let ((children (node-get-children node)))
    (every failed? children)))

(define (children-complete? node)
  (cond
   ((leaf-node? node)
    #t)
   ((eqv? 'target (node-get-rule-type node))
    (or (every-child-has-passed? node)
        (any-child-has-failed? node)))
   ((eqv? 'suffix (node-get-rule-type node))
    (or (every-child-has-failed? node)
        (any-child-has-passed? node)))
   (else
    #f)))

(define (children-passed? node)
  (cond
   ((null? (node-get-children node))
    #t)
   ((eq? 'target (node-get-rule-type node))
    (every-child-has-passed? node))
   ((eq? 'suffix (node-get-rule-type node))
    (any-child-has-passed? node))
   (else
    #f)))

(define (get-next-child node)
  "Return the first child node that is not yet PASS or FAIL"
  (let ((children (node-get-children node)))
    (cond
     ((null? children)
      #f)
     (else
       (any (lambda (child)
              (if (eqv? (node-get-status child) 'undetermined)
                  child
                  #f))
            children)))))

(define (has-parent? node)
  (if (node-get-parent node)
      #t
      #f))

(define (has-children? node)
  (if (null? (node-get-children node))
      #f
      #t))

(define (get-parent node)
  (node-get-parent node))

(define (up-to-date? node)
  "Checks if node is up to date:
- it has an mtime
- all its children have mtimes
- its mtime is older than the mtime of its children"
  (let ((children (node-get-children node))
        (parent-mtime (node-get-mtime node)))
    (if (or (null? children) (not (integer? parent-mtime)))
        ;; Targets without children are always rebuilt.
        ;; Targets without mtimes are always rebuilt.
        #f
        (let ((children-mtime (map node-get-mtime children)))
          (if (every (lambda (child-mtime)
                       (and (integer? child-mtime)
                            (>= parent-mtime child-mtime)))
                     children-mtime)
              #t
              #f)))))

(define (node-depth node)
  (let loop ((depth 0)
             (cur node))
    (if (has-parent? cur)
        (loop (1+ depth) (get-parent cur))
        ;;
        depth)))
  
(define (node-depth-string node)
  (make-string (* 2 (node-depth node)) #\space))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUTOMATIC VARIABLES

(define target-name 'unspecified)
(define target-basename 'unspecified)
(define prerequisites '())
(define primary-prerequisite 'unspecified)
(define newer-prerequisites '())

(define (string-append-with-spaces lst)
  "Appends the strings in lst, adding spaces in between."
  (if (null? lst)
      ""
      ;; else
      (fold
       (lambda (elem prev)
         (string-append prev " " elem))
       (car lst)
       (cdr lst))))

(define $@  (lambda () target-name))
(define $*  (lambda () target-basename))
(define $<  (lambda () primary-prerequisite))
(define $$? (lambda () newer-prerequisites))
(define $?  (lambda () (string-append-with-spaces newer-prerequisites)))
(define $$^ (lambda () prerequisites))
(define $^  (lambda () (string-append-with-spaces prerequisites)))

(define (target-rule-prep-automatic-variables node rule)
  (set! target-name (node-get-name node))
  (set! target-basename (basename target-name))
  (set! prerequisites (target-rule-get-prerequisites rule))
  (set! primary-prerequisite (if (null? prerequisites) "" (car prerequisites)))
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
  #|
  (-> ".c" ""
      (~ ($ CC) ($ CFLAGS) ($ LDFLAGS) "-o" $@ $<))
  (-> ".f" ""
      (~ ($ FC) ($ FFLAGS) ($ LDFLAGS) "-o" $@ $<))
  (-> ".sh" ""
      (~ "cp" $< $@)
      (~ "chmod a+x" $< $@))
  |#
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

(define (run-target-rule! node)
  "Runs the (singular) target rule associated with this node."
  (unless (node? node)
    (scm-error 'wrong-type-arg "run-target-rule!" "Not a node: ~S" (list node) #f))

  (let ((rules (node-get-rules node)))
    (when (null? rules)
      (scm-error 'misc-error "run-target-rule!" "Node ~S has no target rules"
                 (list (node-get-name node)) #f))
    (unless (= 1 (length rules))
      (scm-error 'misc-error "run-target-rule!" "Node ~S has ~A target rules"
                 (list (node-get-name node) (length rules)) #f))
    (unless (or (leaf-node? node) (every-child-has-passed? node))
      (scm-error 'misc-error "run-target-rule!" "Node ~S: not all children have passed"
                 (list (node-get-name node)) #f))

    (let ((rule (car rules)))
      (target-rule-prep-automatic-variables node rule)
      (run-recipes! node (target-rule-get-recipes rule))
      (let ((status (node-get-status node)))
        status))))

(define (run-suffix-rules! node)
  "Runs the one-or-more suffix rules associated with this node.  It
runs them one-by-one, quitting on the first success."
  (unless (node? node)
    (scm-error 'wrong-type'arg "run-suffix-rules!" "Not a node: ~S" (list node) #f))

  (let ((rules (node-get-rules node))
        (children (node-get-children node)))
    (when (null? rules)
      (scm-error 'misc-error "run-suffix-rules!" "Node ~S has no rules"
                 (list (node-get-name node)) #f))
    (when (null? children)
      (scm-error 'misc-error "run-suffix-rule!" "Node ~S has no children"
                 (list (node-get-name node)) #f))
    (unless (any-child-has-passed? node)
      (scm-error 'misc-error "run-suffix-rule!" "Node ~S: not child has passed"
                 (list (node-get-name node)) #f))
    (unless (= (length rules) (length children))
      (scm-error 'misc-error "run-suffix-rule!" "Node ~S: must have as many children as rules"
                 (list (node-get-name node)) #f))

    (let ((i 0)
          (len (length children)))
      (while (< i len)
        (let ((rule (list-ref rules i))
              (child (list-ref children i)))
          (when (passed? child)
            (when (>= %verbosity 3)
              (format #t "~A: attempting to make using ~a~A~a ~a ~a~A~a rule"
                      (node-get-name node)
                      (lquo) (suffix-rule-get-source rule) (rquo)
                      (right-arrow)
                      (lquo) (suffix-rule-get-target rule) (rquo)))
            (suffix-rule-prep-automatic-variables node rule)
            (run-recipes! node (suffix-rule-get-recipes rule)))

          (when (passed? node)
            (break))
          (set! i (1+ i)))))

    (when (>= %verbosity 3)
      (if (passed? node)
          (format #t "PASS: ~a~%~!" (node-get-name node))
          (format #t "FAIL: ~a~%~!" (node-get-name node))))
    (node-get-status node)))

(define (run-recipes! node recipes)
  "Runs the recipes on this node, one by one.  Recipes are either
strings, procedures that return strings, or generic procedures.  If a
failure condition happens, mark the node as having failed."
  (unless (node? node)
    (scm-error 'wrong-type-arg "run-recipes!" "Not a node: ~S" (list node) #f))
  ;;(unless (and (list? recipes) (not (null? recipes)))
  ;;  (scm-error 'wrong-type-arg "run-recipes!" "Not a non-null list: ~S" (list recipes) #f))

  (let ((i 0)
        (len (length recipes)))
    (while (< i len)
      (let* ((opt/recipe (list-ref recipes i))
             (opt (car opt/recipe))
             (recipe (cdr opt/recipe)))
        ;; Recipes are either
        ;; - strings to pass to system
        ;; - procedures that return a string which is passed
        ;;   to system
        ;; - procedures (that don't return a string) that are executed
        ;;   that pass unless they return #f
        ;; OPT is one of 'default, 'ignore, 'silent

        (cond
         ((eq? recipe #t)
          (set-pass! node))

         ((eq? recipe #f)
          (set-fail! node))

         ((string? recipe)
          (when (= %verbosity 1)
            (format #t "~a~%~!" (node-get-name node)))
          (when (or (and (= %verbosity 2) (not (eq? 'silent opt)))
                    (= %verbosity 3))
            (format #t "~A~%~!" recipe))
          (let ((retval (%system-proc recipe)))
            (if (zero? retval)
                (set-pass! node)
                (set-fail! node))))

         ((procedure? recipe)
          (let ((retval (recipe)))
            (cond
             ;; If a procedure returns a string, that string gets
             ;; processed by system.
             ((string? retval)
              (when (= %verbosity 1)
                (format #t "~a~%~!" (node-get-name node)))
              (when (or (and (= %verbosity 2) (not (eq? 'silent opt)))
                        (= %verbosity 3))
                (format #t "~A~%~!" retval))
              (let ((retval2 (%system-proc retval)))
                (if (zero? retval2)
                    (set-pass! node)
                    (set-fail! node))))

             ;; A scheme procedure recipe that returns false.
             ((eqv? retval #f)
              (set-fail! node))

             (else
              ;; Otherwise, this was a procedure that didn't return
              ;; #f or a string, so it gets a pass.
              (set-pass! node)))))

         (else
          ;; Can't be converted to a viable string or procedure
          (scm-error 'misc-error "run-recipes!" "bad recipe: ~S" (list recipe) #f)))

        (when (failed? node) (break))
        (set! i (1+ i))))

    (when (passed? node)
      (let ((name (node-get-name node)))
        (when (and (file-exists? name)
                   (regular-file? name))
          (node-set-mtime! node (compute-mtime name)))))))

(define (run-default-rule! node)
  "The default rule if not other rule exists. It just passes if the
file exists."
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

(define (initialize-rules targets builtins? ignore-errors? continue-on-error? no-execution? verbosity ascii?)
  (set! %target-rules '())
  (set! %suffix-rules '())
  (set! %top-level-targets targets)
  (set! %ignore-errors? ignore-errors?)
  (set! %continue-on-error? continue-on-error?)
  (set! %no-execution? no-execution?)
  (set! %verbosity verbosity)
  (set! %ascii? ascii?)
  (when builtins?
    (add-builtins)))

(define (create-node name parent)
  "Constructs a tree of nodes, with name as the root node."
  (when (and (node? parent) (> (node-depth parent) 30))
    (error "Stack overflow"))
  (let ((node (make-node name parent 'undetermined)))
    (node-set-children! node '())
    (node-set-rule-type! node 'default)
    ;; FIXME: here check that this name differs from all ancestor's
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
              (node-set-rule-type! node 'target)
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

    ;; If no rule found so far, search for suffix rules.
    (when (null? (node-get-rules node))
      (for-each
       (lambda (rule)
         (let ((targ (suffix-rule-get-target rule)))
           (when (string-suffix? targ name)
             ;; For suffix rules, there will be exactly one child per
             ;; rule and the name of the child is constructed from a
             ;; suffix and the parent's name.
             (node-set-rules! node (cons rule (node-get-rules node)))
             (node-set-rule-type! node 'suffix)
             (let* ((src (suffix-rule-get-source rule))
                    (prereq
                     (string-append
                      (string-drop-right name (string-length src))
                      src)))

               ;; Note the recursion here.
               (node-set-children! node
                                   (cons (create-node prereq node)
                                         (node-get-children node)))))))
       %suffix-rules))

    ;; FIXME: First matching rule has highest priority? Or is last better?
    (node-set-rules! node (reverse (node-get-rules node)))
    (node-set-children! node (reverse (node-get-children node)))
    ;;(format #t "matching suffix rules ~S~%~!" (node-get-rules node))
    ;;(format #t "matching children rules ~S~%~!" (node-get-children node))

    ;; And node is ready to go
    node))

(define (build root)
  "Give a tree of <node>, this executes the recipes therein.
This is where the magic happens."
  (let ((tree (create-node root #f)))
    (let ((node tree))
      (when (>= %verbosity 3)
        (format #t "~ABegin building target ~a~A~a.~%~!"
                (node-depth-string node) (lquo) (node-get-name node) (rquo)))
      (while #t
        (when (>= %verbosity 3)
          (format #t "~AConsidering target ~a~A~a.~%~!"
                  (node-depth-string node) (lquo) (node-get-name node) (rquo)))
        (if (undetermined? node)
            (begin
              (when (>= %verbosity 3)
                (format #t "~ATarget file ~a~A~a is undetermined.~%~!"
                        (node-depth-string node) (lquo) (node-get-name node) (rquo))
                (unless (node-get-mtime node)
                  (format #t "~AFile ~a~A~a does not exist.~%~!"
                          (node-depth-string node) (lquo) (node-get-name node) (rquo))))
              (if (children-complete? node)
                  (begin
                    (when (and (>= %verbosity 3) (has-children? node))
                      (format #t "~AFinished prerequisites of target file ~a~A~a.~%~!"
                              (node-depth-string node) (lquo) (node-get-name node) (rquo)))
                    (if (children-passed? node)
                        (begin
                          (when (and (>= %verbosity 3) (has-children? node))
                            (format #t "~AThe prerequisites of target file ~a~A~a have passed.~%~!"
                                    (node-depth-string node) (lquo) (node-get-name node) (rquo)))
                          (if (up-to-date? node)
                            (begin
                              (when (node-get-mtime node)
                                (when (>= %verbosity 3)
                                  (format #t "~ATarget file ~a~A~a is up to date.~%~!"
                                          (node-depth-string node)
                                          (lquo) (node-get-name node) (rquo))))
                              (set-pass! node))
                            ;; else, not up to date
                            (begin
                              (when (>= %verbosity 3)
                                (format #t "~ATarget file ~a~A~a is not up to date.~%~!"
                                        (node-depth-string node)
                                        (lquo) (node-get-name node) (rquo)))
                              (cond
                               ((using-target-rule? node)
                                (when (>= %verbosity 3)
                                  (format #t "~ATarget file ~a~A~a has a target rule.~%~!"
                                          (node-depth-string node)
                                          (lquo) (node-get-name node) (rquo)))
                                (run-target-rule! node))
                               ((using-suffix-rules? node)
                                (when (>= %verbosity 3)
                                  (format #t "~ATarget file ~a~A~a has a suffix rule.~%~!"
                                          (node-depth-string node)
                                          (lquo) (node-get-name node) (rquo)))
                                (run-suffix-rules! node))
                               ((using-default-rule? node)
                                (when (>= %verbosity 3)
                                  (format #t "~ATarget file ~a~A~a is using the default rule.~%~!"
                                          (node-depth-string node)
                                          (lquo) (node-get-name node) (rquo)))
                                (run-default-rule! node))
                               (else
                                (error "bad rules")))

                              (if (passed? node)
                                  (when (>= %verbosity 3)
                                    (format #t "~ATarget file ~a~A~a has passed.~%~!"
                                            (node-depth-string node)
                                            (lquo) (node-get-name node) (rquo)))
                                  (when (>= %verbosity 3)
                                    (format #t "~ATarget file ~a~A~a has failed.~%~!"
                                            (node-depth-string node)
                                            (lquo) (node-get-name node) (rquo)))))))
                        ;; else, children have failed
                        (begin
                          (when (>= %verbosity 3)
                            (format #t "~AThe prerequisites of target file ~a~A~a have failed.~%~!"
                                    (node-depth-string node) (lquo) (node-get-name node) (rquo)))
                          (set-fail! node))))
                  ;; else, children aren't complete
                  (begin
                    (when (>= %verbosity 3)
                      (format #t "~AThe prerequisites of target file ~a~A~a are incomplete.~%~!"
                              (node-depth-string node) (lquo) (node-get-name node) (rquo)))
                    (let ((next (get-next-child node)))
                      (when (>= %verbosity 3)
                        (format #t "~ADescending node ~a~A~a ~a ~a~A~a.~%~!"
                                (node-depth-string node)
                                (lquo) (node-get-name node) (rquo)
                                (right-arrow)
                                (lquo) (node-get-name next) (rquo)))
                      (set! node (get-next-child node))
                      ))))
            ;; else, this node is determined
            (begin
              (if (passed? node)
                  (when (>= %verbosity 2)
                    (format #t "~A~a~A~a: ~Apass~A~%~!"
                            (node-depth-string node) (lquo) (node-get-name node) (rquo)
                            (green) (default)))
                  (when (>= %verbosity 2)
                    (format #t "~A~a~A~a: ~Afail~A~%~!"
                            (node-depth-string node) (lquo) (node-get-name node) (rquo)
                            (red) (default))))
              (if (has-parent? node)
                  (begin
                    (when (>= %verbosity 3)
                      (format #t "~AAscending node ~a~A~a ~a ~a~A~a.~%~!"
                              (node-depth-string node)
                              (lquo) (node-get-name node) (rquo)
                              (right-arrow)
                              (lquo) (node-get-name (node-get-parent node)) (rquo)))

                    (set! node (get-parent node)))
                  ;; else, there is no parent to this node
                  (begin
                    (when (>= %verbosity 3)
                      (format #t "~ATarget file ~a~A~a has no parent.~%~!"
                              (node-depth-string node)
                              (lquo) (node-get-name node) (rquo)))
                    (if (passed? node)
                        (when (>= %verbosity 1)
                          (format #t "~A~a~A~a: ~Acomplete~A~%~!"
                                  (node-depth-string node)
                                  (lquo) (node-get-name node) (rquo)
                                  (green) (default)))
                        (when (>= %verbosity 1)
                          (format #t "~A~a~A~a: ~Acomplete~A~%~!"
                                  (node-depth-string node)
                                  (lquo) (node-get-name node) (rquo)
                                  (red) (default))))
                    (break)))))))
    ;; Return the command output of the root node
    (passed? tree)))
