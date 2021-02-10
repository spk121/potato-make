# CHEATSHEET FOR STUDIOUS POTATO MAKE

## Boilerplate

Add this at the top of your build script.

    #!/usr/bin/env sh
    exec guile -s "$0" "$@"
    !#

    (use-modules (studious-potato))
    (initialize)

Add this at the bottom of your build script

    (build)
    
The rules go in between `initialize` and `build`

## MAKEVARS

A hash table called `%makevars` has string keys. These procedures
are syntax that add quotation marks around `key`, so you call them without the quotes on
`key`. The returned value of `$` is a string, or an empty string on failure.

    ($ KEY) -> "VAL"

    ($ key [transformer])
        Look up `key` in the `%makevars` hash table and return the result.
        If a string-to-string transformer procedure is provided, apply it to each
        space-separated token in the result.
    (?= key val)
        Assign `val` to `key` in the `%makevars` hash table. If `val` is a procedure,
        assign its output to `key` the first time that `key` is referenced.
    (:= key val)
        Assign `val` to `key` in the `%makevars` hash table. If `val` is a procedure,
        evaluate it and assign its output to `key` immediately.

## RULES

The target rule is for when the target, and the prerequisites, if any,
have filenames or phony names.

    (: target-name '(prereq-name-1 prereq-name-2 ...)
       recipe-1
       recipe-2
       ...)
       
     `target-name` is a string which is either a filename to be created
     or an phony name like "all" or "clean".
     
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
       
     Recipe as a procedure returning a string to be evaluated by the system
     
     (: "foo.o" '("foo.c")
       (lambda ()
         (format #f "cc ~A -c foo.c" some-flags))
         
     Recipe using recipe helper procedures, which create a string to be
     evaluated by the system
     
     (: "foo.c" '("foo.c")
       (~ ($ CC) ($ CFLAGS) "-c" $<))
       
The suffix rule is a generic rule to convert one file type to another.
Note that the prerequisites is *not* a list.

     (-> ".c" ".o"
       (~ ($ CC) ($ CFLAGS) ".c" $<))
       
# RECIPE HELPERS

     Concatenate elements with `~`. `~` inserts spaces between the elements.
     Elements can be
     - strings
     - procedures that return strings
     - `%makevar` hash-table references
     - special variables
     - anything whose string representation as created by
       (format #f "~A" ...) make sense
     
     (~ "string" (lambda () "string") ($ KEY) $@ 100 )
     
     Three versions of `~` with special effects
     (~- ...)   ignores any errors
     (~@ ...)   doesn't print recipe to console
     (~+ ...)   runs even when `--no-execute` was chosen
     
     Recipes can contain the following special variables
     
     $@    the target
     $*    the target w/o a filename suffix
     $<    the first prerequisite
     $^    the prerequisites, as a single space-separated string
     $$^   the prerequisites, as a scheme list of strings
     $?    the prerequisites that are files newer than the target file
           as a single space-separated string
     $$?   the prerequisites that are files newer than the target file
           as a scheme list of strings
