# POTATO MAKE

A build tool written in Guile Scheme.

## Description

Potato Make is a scheme library that aims to simplify the task of
maintaining, updating, and regenerating programs.  It is inspired by
the `make` utility in POSIX.  With this library, you can write a
build script in Guile Scheme.

Like POSIX `make`, these scripts update files that are derived from
other files. The makefile script typically will describe how a file is
built from shell commands, and it will describe the relationships
between components to be built, so that they are built in order.  In a
typical script, the script will check the prerequisites to a target,
and if the prerequisites are newer than the target, it will rebuild
the target.

There are two types of rules that a makefile script can contain.

1. Target rules, which describe how a specific named file is to be
   built from prerequisites using a set of shell commands.
2. Suffix rules, which generically describe how to convert files with
   one filename suffix into files with another filename suffix.

The makefile script will make use of a custom variable type which can
be set either in the script, by environment variables, or in command
line arguments.  Let's call them *makevars*, to reduce confusion with
standard scheme variables.

## Setting up the Scheme Script

To write a build script with this library, one needs to add the
following boilerplate code at the top of an executable scheme script.
Throughout this documentation we will presume that this scheme script
is named `makefile.scm`; however, you may choose any name.

    #!/usr/bin/env sh
    exec guile -s "$0" "$@"
    !#

    (use-modules (potato make))
    (initialize)

This boilerplate loads the library functions and it parses the
command-line arguments.  The command-line arguments are the following,

    makefile.scm [-hvqVeEbknB] [var=value...] [target_name...]
             -h, --help
                 displays help
             -v, --version
                 displays the version number of this script
             -q, --quiet
                 use a terse output format
             -V, --verbose
                 use a verbose output format
             -e, --environment
                 environment variables are converted to makevars
             -E, --elevate-environment
                 environment variables are converted to makevars
                 and will override makevars set in the script
             -b, --builtins
                 adds some default makevars and suffix rules
             --ignore-errors
                 keep building even if a command fails
             -k, --continue-on-error
                 keep building some targets even if a command fails
             -n, --no-execute
                 print rules, but only execute rules marked as
                 'always execute'
             -a, --ascii
                 use ASCII-only output and no colors
             -W, --warn
                 enable warning messages
             
             [var=value...]
               set the value of makevars
             [target_name...]
               Set one or more targets to be executed.  If no target
               is specified, the first target found will be executed.

Note that in POSIX `make`, it, by default, adds in environment
variables and built-in rules.  With this library, these require
command-line arguments to be enabled to pick up environment variables
and built-in rules.  This is to make this tool more appropriate for
generating *reproducible builds*.

If you don't want `initialize` to parse the command line, you may call
it with specific command line arguments, like the example below.  The
first string is the name of the script, and then any combination of
flags, macro assignments and targets may follow.

    (initialize '("makefile.scm" "--verbose" "CC=gcc" "all"))

If you call initialize with an empty list as below, it will guess the
script name from the command-line arguements, but, will ignore all
other flags and options.

    ;; ignore all command line arguments except the script name
    (initialize '())

## Environment Variables

Certain environment variables affect the execution of the makefile
script.

`LANG` - affects the current locale

`MAKEFLAGS` - This will be parsed similar to command-line arguments.
    If it contains the single, space-separated letters 'e', 'f', 'i',
    'k', 'n', 'p', 'r', or 's', those options will be enabled as if
    set on the command line.  If it contains strings of the form
    VAR=VALUE, it will set those makevars.
    
`SHELL` - The shell environment variable is always ignored.

All other environment variables, including those with null values,
shall initialize makevars.

## Signals

`SIGHUP`, `SIGTERM`, `SIGINT`, and `SIGQUIT` shall interrupt any
processing.

## Rules

The purpose of a makefile script is to run rules, which describe how
programs act on prerequisites to create targets. There are two types
of rules: target rules and suffix rules.

### Target Rules

Target rules are defined and manipulated with the following commands.

    target-rule name [prerequisites] [commands...]
    : name [prerequisites] [commands ...]
    
`target-rule` (aka `:`) adds a target rule to the target rule
list.  There are 3 components

- NAME is a string that names the target.  If this rule is being used
  to create a file, NAME is the name of the file to be output.
  
  NAME can also be a predicate procedure that maps string->boolean.
  But if NAME is a procedure, this rule cannot be used at the
  top-level target of a build.
- PREREQUISITES, if provided, is a list of strings or procedures of
  zero arguments that evaluate to strings.  Each entry is the
  name of a target that needs to be exist before this target is
  attempted. It may be an empty list, indicating that there are no
  prerequisites.
- COMMANDS, if provided, are recipes that will be executed that are
  intended to cause the target to be created.  The recipe can be
  either a string or a procedure.
  
If the COMMAND recipe is a string, it will be passed to the `system`
procedure for execution by the shell. If any call to system returns a
non-zero return value, processing will end. (This behavior is modified
by the `--ignore-errors` and `--continue-on-error` command-line
arguments.)

If the COMMAND recipe is a procedure, it will be executed.  If it
returns `#f` or a non-zero integer, failure is assumed.  If the
COMMAND recipe returns a string, the resulting string is passed to
`system` and is process as above.

If the COMMAND recipe is a pair, and the CAR of the pair is one of
`'ignore-error`, `'silent`, or `'always-execute`, it will have the
extra effect of ignoring errors, not printing the command line, or
always executing even when the `--no-execution` option is enabled.
The CDR must be a string or procedure as above.

There are a set of helper functions and variables that can be used to
construct recipes.

    string-compose element ...
    ~ element ...
    ignore-error-compose element ...
    ~- element ...
    silent-compose element ...
    ~@ element ...
    always-execute-compose element ...
    ~+ element ...

`string-compose` (aka `~`) takes as arguments one or more elements. It
converts the elements to strings and concatenates the strings,
appending spaces between them. The conversion to strings happens as if
by `display`.

For elements that are procedures, they are executed and their result
is used instead.

It is returned as a pair, where the `car` is the symbol `'default`.
That symbol is interpreted by the builder.

`ignore-error-compose` (aka `~-`) is like string-compose but returns a
pair with the first argument of `'ignore-error`. When passed as a
recipe, it causes the recipe not to end execution, even if an error is
signaled.

`silent-compose` (aka `~@`) is like string-compose, but, it does not
print the resulting string to the output port, except in verbose mode.

`always-execute-compose` (aka `~+`) is like compose, but, it forces
the line to always be executed, even if the `--no-execution` option
was chosen.
        
    target-name
    $@
    
`target-name` (aka `$@`) is a global variable. If called from within the
context of a recipe, it contains as a string the name of the target.
target-name is not thread safe.

    newer-prerequisites
    $?

`newer-prerequisites` (aka `$?`) returns the list of prerequisites that
are newer than the target.

    primary-prerequisite
    $<
    
`primary-prerequisite` (aka `$<`) returns the first prerequisite.

    target-basename
    $*
    
`target-basename` (aka `$*`) returns the target with the suffix elided.
    
    prerequisites
    $^
    
`prerequisites` (aka `$^`) return all the prerequisites.

    %target-rule-list`

`%target-rule-list` is list of targets rules encountered in the build
script in the order in which they were listed, converted into an
internal format.

Here are some example target rules that with recipes meant to be
executed by `system`.

    (: "foo.o" '("foo.c" "foo.h")
       (~ "cc -o" $@ $<))
       
    (: "clean" '()
       "rm *.o"
       "rm *~")
       
Target rules may take advantage of makevars.

    (: "foo.o" '("foo.c" "foo.h")
       (~ ($ CC) ($ CFLAGS)  "-o" $@ $<))

Target rules may also have recipes that execute scheme code

    (: "clean" '()
        (lambda ()
          (delete-file "foo.o")))

### Suffix Rules

Unlike target rules which are for one specific target and may have
multiple prerequisites, suffix rules describe how to create a target
from a single prerequisite with the assumption that they have the same
basename and differ only in the filename suffixes.  The are applied to
implicit prerequisites to other rules, or to explicit prerequisites to
other rules that have no target rules defined.

For example, one could have a suffix rule to convert a `*.c` file into
a `*.o` file.  The syntax for suffix rules are similar to target rules
above.

    suffix-rule source-suffix target-suffix [commands...]
    -> source-suffix target-suffix [commands ...]
    
`suffix-rule` (aka `->` or `→`) adds a suffix rule to the suffix rule
list.  There are 3 components

- SOURCE-SUFFIX is a string that names the filename suffix of the file
  used to create the target. Commonly, this string begins with a
  period.
  
  SOURCE-SUFFIX can also be a conversion procedure that takes
  in a target name string and converts it into a source name string.
  
- TARGET-SUFFIX, is a string that is the filename suffix of the file
  to be created.  The TARGET-SUFFIX could be an empty string,
  indicating that the target is just the basename with no suffix.
  
  TARGET-SUFFIX can also be a predicate procedure that takes in a
  potential target name string and returns `#t` or `#f` if the target
  name string should be processed with this suffix rule.
  
- COMMANDS, if provided, are recipes that will be executed that are
  intended to cause the target to be created.  The recipe can be
  either a string or a procedure.
  
If the COMMAND recipe is a string, it will be passed to the `system`
procedure for execution by the shell. If any call to system returns a
non-zero return value, ending processing.

If the COMMAND recipe is a procedure, it will be executed.  If it
returns #f or a non-zero integer, failure is assumed.  If the COMMAND
recipe returns a string, the resulting string is passed to `system`
and is process as above.

    %suffix-rule-list

`%suffix-rule-list` is list of suffix rules encountered in the build
script in the order in which they were listed, converted into an
internal format.

Example suffix rules are

    (-> ".c" ".o"
        (~ ($ CC) ($ CFLAGS) "-c" "-o" $@ $<))
        
    (-> ".sh" ""
        (~ "cp" $< $@)
        (~ "chmod a+x" $@))


## makevars

Makefile scripts may take advantage of a special variable type
called a makevar.  In scheme terms, makevars are entries in a 
`%makevars` hash table that have special accessor syntax.

- The makevar names -- the keys -- are strings.
- The makevar values are either strings or procedures that take no
  arguments that return strings.
  
There are five ways a makevar can be initialized.

1. Set directly in the script using the `?=` or `:=` syntax.
2. Set in command-line arguments
3. Extracted from the `MAKEFLAGS` environment variable
4. Generated from the environment variables
5. Or be one of the predefined variables built into this library
    
There is a priority to makevars. The variables from category five
above are set first, then four, then three, etc. Each lower category
may overwrite variables set in the higher category.

This priority is modified by the `-e` command-line argument.  If `-e`
is set, category 1 variables *do not* override variables from categories
2, 3, and 4.  They *do* override variables set in category 5.

The library provides the following procedures for makevars

    lazy-assign key [val]
    
> `lazy-assign` sets a entry in the makevars hash table.  KEY must be
> a string or a thunk that evaluates to a string.  Likewise VAL must
> be a string or a thunk that evaluates to a string.

> If KEY is a thunk, it is immediately evaluated to a string to use as
> the key in the hash table entry.

> If VAL is a thunk, it is stored as a *promise* to be evaluated
> later. The promise will be evaluated the first time this key is
> referenced.

> If VAL is not given, the empty string will be used.
        
    ?= key [val]
    
> This is a syntax version of lazy-assign where KEY should be a string
> without quotes, e.g.
        
        (?= foo "bar")  ==>  (lazy-assign "foo" "bar")
    
    assign key [val]
    
> `assign` is the same as `lazy-assign` above, except that if VAL is a
> thunk it is immediately evaluated to a string and that string is
> used as the value in the hash table entry.
        
    := key [val]
    
> This is a syntax version of `assign` where KEY should be a string
> without quotes, e.g.
        
        (:= foo "bar")  ==>  (assign "foo" "bar")

    reference key [transformer]

> `reference` looks up KEY in the `%makevar` hash table.  If it is
> found, VALUE is returned as a string.

> *IMPORTANT!* If it is not found, an empty string is returned.  This
> is because it is a common practice in makefiles to use makevars that
> may or may not be defined by environment variables. In `--verbose`
> mode, a warning will be printed when a key cannot be found.
        
> If the value was stored using `lazy-assign` and is a *promise*, this
> procedure is *forced* to return a string.  Also, the value in the
> hash table is updated to this string.

> The optional `transfomer` should be a function the takes a string
> and returns a string. It will be applied to every space-separated
> word in the value.

    $ key
    
> This is a syntax version of `reference`, where KEY should be a
> string without quotes, e.g.
        
        ($ key) ==> (reference "key")
        
    reference-func key
    
> `reference-func` returns a procedure of zero arguments that will,
> when called, look up a key as described in `reference` above.
        
    $$ key
    
> This is a syntax version of reference-func, where KEY should be a
> string without quotes, e.g.
        
        ($$ key) ==> (reference-func "key")

    %makevars
    
> This is the hash table.  You are not meant to access it directly,
> but, with the functions above.  If you do use it directly, the VALUE
> is a cons where the CAR is string or promise and the CDR is private
> data.
        
## The build algorithm

The initial target is given on the command line. If no target was
given on the command line, the first entry in the target list is used.

For each top-level target, create a n-ary tree of prerequisites.  If a
target doesn't have an explicit rule, but has a suffix that appears in
one or more suffix rules, it searches for possible prerequisites that
would fulfill a suffix rule. Continue until the tree is populated.

Then for each node, try to compute timestamps for each target, if they
exist.

Mark as 'skip' each node that is a real file that is older than the
parent file.

In a depth-first search, build each node unless the node target is
older than the parent.

If a build recipe fails...
If '--ignore-errors', mark current node as 'skip', then keep going.
If '--continue-on-error', mark all siblings as 'skip', and mark the direct ancestors 'skip', keep
going.
Else, quit.

If we're not quit, once reaching the end, start with the next
top-level target (which only happens is multiple targets are given in
the command line).

## Built-in rules and makevars

If the `--builtins` option is given, there are some builtin suffix rules
and *makevars* that are present by default. These include the following.
You can add more builtins by updating the potato/builtins.scm file.

    MAKE=make
    AR=ar
    ARFLAGS=-rv
    YACC=yacc
    YFLAGS=
    LEX=lex
    LFLAGS=
    LDFLAGS=
    CC=gcc
    CFLAGS=-g -O2
    FC=gfortran
    FFLAGS=-g -O2
    
    (-> ".c" ".o"
        (~ ($ CC) ($ CFLAGS) "-c" $<)))

    (-> ".f90" ".o"
        (~ ($ FC) ($ FFLAGS) "-c" $<))
        
    (-> ".y" ".o"
        (~ ($ YACC) ($ YFLAGS) $<)
        (~ ($ CC) ($ CFLAGS) "-c y.tab.c")
        "rm -f y.tab.c"
        (~ "mv y.tab.o" $@))

    (-> ".l" ".o"
        (~ ($ LEX) ($ LFLAGS) $<)
        (~ ($ CC) ($ CFLAGS) "-c lex.yy.c")
        "rm -f lex.yy.c"
        (~ "mv lex.yy.o" $@))

    (-> ".y" ".c"
        (~ ($ YACC) ($ YFLAGS) $<)
        (~ "mv y.tab.c" $@))

    (-> ".l" ".c"
        (~ ($ LEX) ($ LFLAGS) $<)
        (~ "mv lex.yy.c" $@))

## Debug commands

These commands modify how rules are interpreted or executed

FIXME
