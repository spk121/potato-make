(define-module (potato rules)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-9)
  #:use-module (potato exceptions)
  #:use-module (potato builtins)
  #:use-module (potato makevars)
  #:export(<target-rule>
           <suffix-rule>
           <node>
           %target-rules
           %suffix-rules
           initialize-rules
           first-target-rule-name
           target-rule :
           suffix-rule ->
           target-name         $@
           newer-prerequisites $?
           primary-prerequisite $<
           target-basename     $*
           prerequisites       $^
           build
           string-compose      ~
           ))

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

(define-record-type <node>
  (make-node name parent status)
  node?
  ;; For a real target, this is a filename. For a phony target it is
  ;; just a label.
  (name      node-get-name node-set-name!)
  ;; A <node> which is the parent of this node, or #f.
  (parent    note-get-parent note-set-parent!)
  ;; If 'name' is a regular file, mtime holds its last modification
  ;; time in nanoseconds since the epoch. If 'name' does not exist,
  ;; _mtime is #f.
  (mtime     node-get-mtime node-set-mtime!)
  ;; One of 'success, 'failure, or 'undetermined
  (status    node-get-status node-set-status!)
  ;; A list of rules to evaluate to try to c
  (rules     node-get-rules node-set-rules!)
  (children  node-get-children node-set-children!)
  ;; Determines how many children must pass for the parent
  ;; to be evaluated.  Either 'or or 'and.
  (logic     node-get-logic node-set-logic!)
  )

(define %node-cur #f)

(define target-name
  (lambda ()
    (if %node-cur
        (node-get-name %node-cur)
        #f)))

(define $@ target-name)

(define (basename str)
  (let ((idx (string-index-right str #\.)))
    (if idx
        (substring str 0 idx)
        str)))

(define target-basename
  (lambda ()
    (if %node-cur
        (basename (node-get-name %node-cur)))))

(define $* target-basename)

(define primary-prerequisite
  (lambda ()
    (if %node-cur
        (let ((prereq (node-get-children %node-cur)))
          (if (null? prereq)
              ""
              (car prereq))))))

(define $< primary-prerequisite)

(define (first-target-rule-name)
  (if (null? %target-rules)
      #f
      ;; else
      (target-rule-get-name (car %target-rules))))
;;              target-name          $@
;;               target-basename      $*
;;               newer-prerequisites  $?
;;               prerequisites        $^
;;               primary-prerequisite $<
 

;; There are two priorities of rules
;; 1. rules defined in the script have higher priority than built-in rules
;; 2. later-defined rules have higher priority earlier defined-rules
;; 3. a target rule will have higher priority than a suffix rule for the same
;;    target

(define %target-rules '())
(define %suffix-rules '())
(define %debug? #f)

(define (string-compose . args)
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
            
            #;(unless (string? effective-arg)
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

(define ~ string-compose)

(define (initialize-rules no-builtins? debug?)
  (set! %target-rules '())
  (set! %suffix-rules '())
  (set! %debug? debug?)
  (unless no-builtins?
    ;; Set up initial suffix rules
    (suffix-rule ".c" ".o"
                 (string-compose
                  (reference "CC")
                  (reference "CFLAGS")
                  "-c"
                  primary-prerequisite)))
  )

(define* (target-rule name #:optional (prerequisites '()) #:rest recipes)
  ;; FIXME: Typecheck
  (let ((rule (make-target-rule name prerequisites recipes 1)))
    ;; Add to %target-rules
    (set! %target-rules (cons rule %target-rules))))

;; Alias
(define : target-rule)

(define (suffix-rule source target . recipes)
  ;; FIXME: Typecheck
  (let ((rule (make-suffix-rule source target recipes 1)))
    (set! %suffix-rules (cons rule %suffix-rules))))

;; Alias
(define -> suffix-rule)

(define (regular-file? filename)
  (let ((st (stat filename #f)))
    (eq? (stat:type st) 'regular)))

(define (compute-mtime filename)
  (let ((st (stat filename #f)))
    (+ (* 1000000000 (stat:mtime st))
       (stat:mtimensec st))))

(define (create-node name parent)
  "Constructs a tree of nodes, with name as the root node."
  (let ((node (make-node name parent 'untested)))

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

(define (undetermined? node)
  (eq? (node-get-status node) 'undetermined))

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

(define (build root)
  "Give a tree of <node>, this executes the recipes therein."
  (let ((tree (create-node root #f)))
    (let ((node root))
      (while #t
        (if (undetermined? node)
            (if (children-complete? node)
                (if (children-passed? node)
                    (if (has-recipe? node)
                        (run-recipe! node)
                        ;; else, no recipe exists
                        (run-default-recipe! node))
                    ;; else, children have failed
                    (set-fail! node))
                ;; else, children aren't complete
                (set! node (get-next-child node)))
            ;; else, this node is determined
            (if (and abort-on-error (failed? node))
                (break)
                ;; else not failed
                (if (has-parent? node)
                    (set! node (get-parent node))
                    ;; else, there is no parent to this node
                    (break))))))))
      
        
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
