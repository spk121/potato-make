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
`key`. The returned value is a string.

    ($ KEY) -> "VAL"

    ($ key [transformer])
        Look up `key` in the `%makevars` hash table and return the result.
        If transformer is provided, apply it to each
        space-separated token in the result.
    ($$ key)
        Returns a procedure that looks up `key` in the `%makevars` hash table.
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
     
     recipe as a string
     
     (: "foo.o" '("foo.c")
       "cc -c foo.o")
     
     recipe as a procedure
     
     (: "clean-foo" '()
       (delete-file "foo.o")
       
     recipe as a procedure that returns #f to indicate failure
     
     (: "recent" '()
       (if condition
         #t
         #f))
       
     recipe as procedure returning string
     
     (: "foo.o" '("foo.c")
       (lambda ()
         (format #f "cc ~A -c foo.c" some-flags))
         
     recipe using recipe helper procedures
     
     (: "foo.c" '("foo.c")
       (~ ($ CC) ($ CFLAGS) "-c" $<))
       
The suffix rule is a generic rule to convert one file type to another.
Note that the prerequisites is *not* a list.

     (-> ".c" ".o"
       (~ ($ CC) ($ CFLAGS) ".c" $<))
       
# RECIPE HELPER

     Concatenate elements with `~`. `~` inserts spaces between the elements.
     Elements can be
     - strings, characters, or numbers
     - procedures that return strings
     - `makevar` hash-table references
     - special variables
     
     (~ "string" (lambda () "string") ($ KEY) $@ )
     
     Three versions of `~` with special effects
     (~- ...)   ignores any errors
     (~@ ...)   doesn't print recipe to console
     (~+ ...)   runs even when `--no-execute` was chosen
     
     Recipes can contain the following special variables
     
     $@    the target
     $*    the target w/o a filename suffix
     $^    the prerequisites, as a single string
     $<    the first prerequisite
     $?    the prerequisites that are files newer than the target file
