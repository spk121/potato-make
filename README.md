# POTATO MAKE

Potato Make is a scheme library that aims to simplify the task of
maintaining, updating, and regenerating programs.  It is inspired by
the `make` utility in IEEE Std 1003.1-2017 (POSIX).  With this library, you can write a
build script in Guile Scheme.

## Boilerplate

Add this at the top of your build script.

```scheme    
#!/usr/bin/env sh
exec guile -s "$0" "$@"
!#

(use-modules (potato make))
(initialize)
```

Add this at the bottom of your build script

```scheme
(execute)
```

The rules go in between `initialize` and `build`.

## A Simple Example

```scheme
#!/usr/bin/env sh
exec guile -s "$0" "$@"
!#

(use-modules (potato make))
(initialize)
(:= CC "gcc")
(:= CFLAGS "-g -O2")
    
(: "all" '("foo"))
(: "foo" '("foo.o" "bar.o")
  (~ ($ CC) "-o" $@ $^))
(-> ".c" ".o"
  (~ ($ CC) "-c" $<))
(execute)
```

## Command-Line Arguments

`initialize` is what parses arguments and prepares the build context.
In the usual script form:

```scheme
(use-modules (potato make))
(initialize)
;; rules
(execute)
```

`initialize` reads the process command line (`$0 "$@"`) and applies:

1. options
2. makevar assignments (`KEY=VALUE`)
3. target names

The script then runs `execute`, which builds the selected targets.

You can also pass an explicit argument list to `initialize`:

```scheme
(initialize '("my-build" "--verbosity=3" "CC=clang" "all"))
```

This is useful for tests or programmatic invocation.

Supported command-line form:

  <your-script-name> [-hvVeEbknAS] [var=value ...] [target_name ...]

       -h, --help
         display help and exit
       -v, --version
         display version and exit
       -V [0,1,2,3], --verbosity=[0,1,2,3]
         choose output level
       -e, --environment
         import environment variables as makevars
       -E, --elevate-environment
         import environment variables as makevars with
         elevated precedence over script assignments
       -b, --builtins
         add built-in makevars and suffix rules
       --ignore-errors
         ignore recipe failures and continue
       -k, --continue-on-error
         continue building later top-level targets after failure
       -n, --no-execution
         skip normal recipe execution; run only recipes marked
         `always-execute`
       -a, --ascii
         use ASCII-only output and no colors
       -S, --strict
         throw errors for strict missing/invalid references

       [var=value ...]
         set makevars from the command line
       [target_name ...]
         set one or more targets to execute; if omitted,
         the first defined target rule is used

Examples:

```bash
./build.scm
./build.scm all
./build.scm CC=clang CFLAGS='-O3 -g' all
./build.scm --environment --verbosity=3 test
./build.scm --continue-on-error bad-target good-target
./build.scm --no-execution all
```

## MAKEVARS

A hash table called `%makevars` stores string keys and values used by
rules and recipes.

`initialize` builds this table before any rules are executed.

Initialization sources and priority:

1. Script assignments (`:=`, `?=`) - highest priority
2. Command-line assignments (`KEY=VALUE`)
3. `MAKEFLAGS` assignments (when `--environment` or `--elevate-environment` is used)
4. Environment variables (when `--environment` or `--elevate-environment` is used)
5. Built-ins (when `--builtins` is used) - lowest priority

Lower numeric priority overrides higher numeric priority.

With `--elevate-environment`, script assignments do not override
command-line, `MAKEFLAGS`, or environment values.

Notes:

1. `SHELL` and `MAKEFLAGS` are not imported as makevars.
2. `MAKEFLAGS` is still parsed for `KEY=VALUE` entries when environment import is enabled.
3. In strict mode (`--strict`), referencing an unset makevar raises an error.
4. Without strict mode, unset makevars return an empty string.

    ($ KEY) -> "VAL"

    ($ key [transformer])
        Look up `key` in the `%makevars` hash table and return the
        result as a string.  If `key` is not found, return an empty
        string.  If a string-to-string transformer procedure is
        provided, apply it to each space-separated token in the
        result.

    (Q key [transformer])
        Same as `$`, but wraps the returned value in double quotes.

    ($$ key)
        Return a procedure that yields the current value of `key`, or
        `#f` when the key is unset.

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

```scheme
(: target-name '(prereq-name-1 prereq-name-2 ...)
   recipe-1
   recipe-2
   ...)
```

`target-name` is a string which is either a filename to be
created or an phony name like "all" or "clean".

Recipe as a string to be evaluated by the system

```scheme
(: "foo.o" '("foo.c")
  "cc -c foo.o")
```

Recipe as a procedure

```scheme
(: "clean-foo" '()
  (lambda ()
    (delete-file "foo.o")))

```

Recipe as a procedure that returns #f to indicate failure

```scheme
(: "recent" '()
  (lambda ()
    (if condition
      #t
      #f))))
```

Recipe as a procedure returning a string to be evaluated by the
system

```scheme
(: "foo.o" '("foo.c")
  (lambda ()
    (format #f "cc ~A -c foo.c" some-flags))
```

Recipe using recipe helper procedures, which create a string to
be evaluated by the system

```scheme
(: "foo.c" '("foo.c")
  (~ ($ CC) ($ CFLAGS) "-c" $<))
```

Recipe as a boolean to indicate pass or failure without doing any
processing.  For example, the rule below tells Potato Make that
the file "foo.c" exists without actually testing for it.

```scheme     
(: "foo.c" '() #t)
```

If there is no recipe at all, it is shorthand for the recipe #t,
indicating a recipe that always passes. This is used
in prerequisite-only target rules, such as below, which passes
so long as the prerequisites pass. These two rules are the same.

```scheme
(: "all" '("foo.exe"))
(: "all" '("foo.exe") #t)
```
     
Lastly, if the recipe is #f, this target will always fail.

```scheme     
(: "fail" '() #f)
```

The *suffix rule* is a generic rule to convert one source file to a
target file, based on the filename extensions.

```scheme
(-> ".c" ".o"
  (~ ($ CC) ($ CFLAGS) "-c" $< "-o" $@))
```

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
    (~- ...)   ignores recipe failure and continues
    (~@ ...)   does not print recipe to the console
    (~+ ...)   runs even when `--no-execution` was chosen
    (~k ...)   continues to the next recipe after a failure

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
