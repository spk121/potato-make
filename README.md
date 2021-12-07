# POTATO MAKE

Potato Make is a scheme library that aims to simplify the task of
maintaining, updating, and regenerating programs.  It is inspired by
the `make` utility in POSIX.  With this library, you can write a
build script in Guile Scheme.

## Boilerplate

Add this at the top of your build script.

    #!/usr/bin/env sh
    exec guile -s "$0" "$@"
    !#

    (use-modules (potato make))
    (initialize)

Add this at the bottom of your build script

    (execute)

The rules go in between `initialize` and `build`.

## A Simple Example

    #!/usr/bin/env sh
    exec guile -s "$0" "$@"
    !#

    (use-modules (potato make))
    (initialize)

    ;; Variables
    (:= CC "gcc")
    (:= CFLAGS "-g -O2")
    
    ;; Rules
    (: "all" '("foo"))
    (: "foo" '("foo.o" "bar.o")
      (~ ($ CC) "-o" $@ $^))
    (-> ".c" ".o"
      (~ ($ CC) "-c" $<))

    (execute)

## Command-Line Arguments

This boilerplate loads the library functions and it parses the
command-line arguments.  The command-line arguments are the following,

    <your-script-name> [-hvqVeEbknB] [var=value...] [target_name...]
             -h, --help
                 displays help
             -v, --version
                 displays the version number of this script
             -V [0,1,2,3], --verbosity=[0,1,2,3]
                 choose the verbosity of the output
             -e, --environment
                 environment variables are converted to makevars
             -E, --elevate-environment
                 environment variables are converted to makevars
                 and will override makevars set in the script
             -b, --builtins
                 adds some default makevars and suffix rules
             --ignore-errors  [NOT IMPLEMENTED YET]
                 keep building even if a command fails
             -k, --continue-on-error  [NOT IMPLEMENTED YET]
                 keep building some targets even if a command fails
             -n, --no-execute  [NOT IMPLEMENTED YET]
                 print rules, but only execute rules marked as
                 'always execute'
             -a, --ascii
                 use ASCII-only output and no colors
             -W, --warn  [NOT IMPLEMENTED YET]
                 enable warning messages
             
             [var=value...]
               set the value of makevars
             [target_name...]
               Set one or more targets to be executed.  If no target
               is specified, the first target found will be executed.

## MAKEVARS

A hash table called `%makevars` has string keys. These procedures
are syntax that add quotation marks around `key`, so you call them without the quotes on
`key`. The returned value of `$` is a string, or an empty string on failure.
You define makevars in the script, in the environment, or on the command line.

    ($ KEY) -> "VAL"

    ($ key [transformer])
        Look up `key` in the `%makevars` hash table and return the
        result as a string.  If `key` is not found, return an empty
        string.  If a string-to-string transformer procedure is
        provided, apply it to each space-separated token in the
        result.

    (?= key val)
        Assign `val` to `key` in the `%makevars` hash table. If `val`
        is a procedure, assign its output to `key` the first time that
        `key` is referenced.

    (:= key val)
        Assign `val` to `key` in the `%makevars` hash table. If `val`
        is a procedure, evaluate it and assign its output to `key`
        immediately.

## Rules

The *target rule* is for when the target, and the prerequisites, if any,
have filenames or phony names.

    (: target-name '(prereq-name-1 prereq-name-2 ...)
       recipe-1
       recipe-2
       ...)

     `target-name` is a string which is either a filename to be
     created or an phony name like "all" or "clean".

     Recipe as a string to be evaluated by the system

     (: "foo.o" '("foo.c")
       "cc -c foo.o")

     Recipe as a procedure

     (: "clean-foo" '()
       (lambda ()
         (delete-file "foo.o")))

     Recipe as a procedure that returns #f to indicate failure

     (: "recent" '()
       (lambda ()
         (if condition
           #t
           #f))))

     Recipe as a procedure returning a string to be evaluated by the
     system

     (: "foo.o" '("foo.c")
       (lambda ()
         (format #f "cc ~A -c foo.c" some-flags))

     Recipe using recipe helper procedures, which create a string to
     be evaluated by the system

     (: "foo.c" '("foo.c")
       (~ ($ CC) ($ CFLAGS) "-c" $<))

     Recipe as a boolean to indicate pass or failure without doing any
     processing.  For example, the rule below tells Potato Make that
     the file "foo.c" exists without actually testing for it.
     
     (: "foo.c" '() #t)

     If there is no recipe at all, it is shorthand for the recipe #t,
     indicating a recipe that always passes. This is used
     in prerequisite-only target rules, such as below, which passes
     so long as the prerequisites
     pass. These two rules are the same.

     (: "all" '("foo.exe"))
     (: "all" '("foo.exe") #t)
     
     Lastly, if the recipe is #f, this target will always fail.
     
     (: "fail" '() #f)

The *suffix rule* is a generic rule to convert one source file to a
target file, based on the filename extensions.

     (-> ".c" ".o"
       (~ ($ CC) ($ CFLAGS) "-c" $< "-o" $@))

## Recipe Helpers

     Concatenate elements with `~`. `~` inserts spaces between the
     elements.

     Elements can be
     - strings
     - procedures that return strings
     - `%makevar` hash-table references
     - automatic variables
     - anything whose string representation as created by
       (format #f "~A" ...) make sense

     Any procedures are applied lazily, when the rule is executed.

     (~ "string" (lambda () "string") ($ KEY) $@ 100 )

     Three versions of `~` with special effects
     (~- ...)   ignores any errors
     (~@ ...)   doesn't print recipe to console
     (~+ ...)   runs even when `--no-execute` was chosen

## Automatic Variables

     Recipes can contain the following automatic variables

     $@    the target
     $*    the target w/o a filename suffix
     $<    the first prerequisite
     $^    the prerequisites, as a single space-separated string
     $$^   the prerequisites, as a scheme list of strings
     $?    the prerequisites that are files newer than the target file
           as a single space-separated string
     $$?   the prerequisites that are files newer than the target file
           as a scheme list of strings

## POSIX Makefile Parser

     Recipes can contain the following parser function

     (parse ...) reads a standard Makefile and creates
     rules based on its contents.
