# POTATO MAKE

A build tool written in Guile Scheme.

## Description

Potato Make is a scheme library that aims to simplify the task of
maintaining, updating, and regenerating programs.  It is inspired by
the `make` utility in IEEE Std 1003.1-2017 (POSIX).  With this
library, you can write a build script in Guile Scheme.

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

```scheme
#!/usr/bin/env sh
exec guile -s "$0" "$@"
!#

(use-modules (potato make))
(initialize)
```

Rules go between `initialize` and `execute`.  Add this at the bottom
of the script:

```scheme
(execute)
```

`initialize` parses arguments and prepares the build context.  It
reads the process command line and applies options, makevar
assignments (`KEY=VALUE`), and target names.  `execute` then builds
the selected targets.

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

Supported command-line form:

    <your-script-name> [-hvVeEbknAS] [var=value ...] [target_name ...]

         -h, --help
             display help and exit
         -v, --version
             display version and exit
         -V [0,1,2,3], --verbosity=[0,1,2,3]
             choose the verbosity of the output
             (0 = silent, 1 = terse, 2 = default, 3 = verbose)
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
         -A, --ascii
             use ASCII-only output and no colors
         -S, --strict
             throw errors for missing/invalid makevar references

         [var=value ...]
             set makevars from the command line
         [target_name ...]
             set one or more targets to execute; if omitted,
             the first defined target rule is used

Note that in POSIX `make`, it, by default, adds in environment
variables and built-in rules.  With this library, these require
command-line arguments to be enabled to pick up environment variables
and built-in rules.  This is to make this tool more appropriate for
generating *reproducible builds*.

Examples:

```bash
./makefile.scm
./makefile.scm all
./makefile.scm CC=clang CFLAGS='-O3 -g' all
./makefile.scm --environment --verbosity=3 test
./makefile.scm --continue-on-error bad-target good-target
./makefile.scm --no-execution all
```

If you don't want `initialize` to parse the command line, you may call
it with specific command line arguments, like the example below.  The
first string is the name of the script, and then any combination of
flags, macro assignments and targets may follow.

```scheme
(initialize '("makefile.scm" "--verbosity=3" "CC=gcc" "all"))
```

If you call initialize with an empty list, it will use the program
name from `(program-arguments)` but ignore all other flags and
options.

```scheme
;; ignore all command line arguments except the script name
(initialize '())
```

## Environment Variables

Certain environment variables affect the execution of the makefile
script.

`LANG` - affects the current locale

`MAKEFLAGS` - When `--environment` or `--elevate-environment` is
    enabled, `MAKEFLAGS` is parsed as space-separated tokens.  The
    following word tokens are recognized: `silent`, `terse`, `verbose`,
    `builtins`, `ascii`, `ignore-errors`, `continue-on-error`,
    `strict`, `no-execution`.  Tokens of the form `VAR=VALUE` are
    imported as makevars (except `SHELL` and `MAKEFLAGS` themselves).

`SHELL` - The shell environment variable is always ignored and never
    imported as a makevar.

All other environment variables, including those with null values,
initialize makevars when `--environment` or `--elevate-environment` is
active.

## Extensions

Potato Make supports module-based extensions that can decorate
command-line parsing and initialization behavior.

An extension can contribute:

1. Additional `getopt-long` option entries
2. Additional lines in `--help` output
3. An initialization hook that receives parsed options and can update
   makevars or rules

Extensions are registered through `register-extension!` in the
`(potato make)` module.

```scheme
(register-extension!
  #:id 'my-extension
  #:option-spec '((my-flag (value #t)))
  #:help-lines '("    --my-flag=VALUE"
                 "        example extension option")
  #:init-hook (lambda (options)
                ;; inspect options and set makevars/rules
                #t))
```

The extension must be imported before calling `initialize`, so its
option spec and hook are registered first.

```scheme
(use-modules (potato make)
             (my extension))
(initialize)
```

### Configure Extension

The `(potato configure)` module is a built-in extension plugin.
Importing it before `initialize` adds GNU-style install directory
options and applies them to makevars during initialization.

```scheme
(use-modules (potato make)
             (potato configure))

(initialize)
;; rules
(execute)
```

It also exports `add-directory-variables` for direct/manual use.

Supported long options from this extension:

- `--prefix=DIR`
- `--exec-prefix=DIR`
- `--bindir=DIR`, `--sbindir=DIR`, `--libexecdir=DIR`
- `--datarootdir=DIR`, `--datadir=DIR`
- `--sysconfdir=DIR`, `--sharedstatedir=DIR`, `--localstatedir=DIR`,
  `--runstatedir=DIR`
- `--includedir=DIR`, `--oldincludedir=DIR`
- `--docdir=DIR`, `--infodir=DIR`, `--htmldir=DIR`, `--dvidir=DIR`,
  `--pdfdir=DIR`, `--psdir=DIR`
- `--libdir=DIR`, `--lispdir=DIR`, `--localedir=DIR`
- `--mandir=DIR`, `--man1dir=DIR`, `--man2dir=DIR`
- `--srcdir=DIR`

Example:

```bash
./makefile.scm --prefix=/opt/pkg --exec-prefix=/opt/pkg/exec --srcdir=/tmp/src all
```

After initialization, the configure extension sets directory makevars
(`prefix`, `exec_prefix`, `bindir`, `mandir`, etc.) using GNU defaults
unless overridden by one of the extension options above.

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
  name of a target that needs to exist before this target is
  attempted. It may be an empty list, indicating that there are no
  prerequisites.
- COMMANDS, if provided, are recipes that will be executed that are
  intended to cause the target to be created.  The recipe can be
  a string, a procedure, a boolean, or a pair produced by a compose
  helper.

If the COMMAND recipe is a string, it will be passed to the `system`
procedure for execution by the shell. If any call to system returns a
non-zero return value, processing will end. (This behavior is modified
by the `--ignore-errors` and `--continue-on-error` command-line
arguments.)

If the COMMAND recipe is a procedure, it will be executed.  If it
returns `#f` or a non-zero integer, failure is assumed.  If the
COMMAND recipe returns a string, the resulting string is passed to
`system` and is processed as above.

If the COMMAND recipe is `#t`, the target always passes without
executing anything.  If there is no recipe at all, it is shorthand
for `#t`.  If the recipe is `#f`, the target always fails.

If the COMMAND recipe is a pair, and the CAR of the pair is one of
`'ignore-error`, `'continue-on-error`, `'silent`, or
`'always-execute`, it will have the extra effect of ignoring errors,
continuing to the next recipe after failure, not printing the command
line, or always executing even when the `--no-execution` option is
enabled.  The CDR must be a string or procedure as above.

Here are some example target rules:

Recipe as a string to be evaluated by the system:

```scheme
(: "foo.o" '("foo.c")
  "cc -c foo.o")
```

Recipe as a procedure:

```scheme
(: "clean-foo" '()
  (lambda ()
    (delete-file "foo.o")))
```

Recipe as a procedure that returns `#f` to indicate failure:

```scheme
(: "recent" '()
  (lambda ()
    (if condition #t #f)))
```

Recipe as a procedure returning a string to be evaluated by the
system:

```scheme
(: "foo.o" '("foo.c")
  (lambda ()
    (format #f "cc ~A -c foo.c" some-flags)))
```

Recipe using compose helpers, which create a string to be evaluated by
the system:

```scheme
(: "foo.o" '("foo.c")
  (~ ($ CC) ($ CFLAGS) "-c" $<))
```

Recipe as a boolean to indicate pass or failure without doing any
processing.  For example, the rule below tells Potato Make that
the file "foo.c" exists without actually testing for it.

```scheme
(: "foo.c" '() #t)
```

If there is no recipe at all, it is shorthand for the recipe `#t`,
indicating a recipe that always passes.  This is used in
prerequisite-only target rules:

```scheme
(: "all" '("foo.exe"))
(: "all" '("foo.exe") #t)   ;; equivalent
```

Target rules may take advantage of makevars:

```scheme
(: "foo.o" '("foo.c" "foo.h")
  (~ ($ CC) ($ CFLAGS) "-o" $@ $<))
```

Target rules may also have recipes that execute scheme code:

```scheme
(: "clean" '()
  (lambda ()
    (delete-file "foo.o")))
```

### Suffix Rules

Unlike target rules which are for one specific target and may have
multiple prerequisites, suffix rules describe how to create a target
from a single prerequisite with the assumption that they have the same
basename and differ only in the filename suffixes.  They are applied
to implicit prerequisites to other rules, or to explicit prerequisites
to other rules that have no target rules defined.

For example, one could have a suffix rule to convert a `*.c` file into
a `*.o` file.  The syntax for suffix rules is similar to target rules
above.

    suffix-rule source-suffix target-suffix [commands...]
    -> source-suffix target-suffix [commands ...]

`suffix-rule` (aka `->`) adds a suffix rule to the suffix rule
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
non-zero return value, processing ends.

If the COMMAND recipe is a procedure, it will be executed.  If it
returns `#f` or a non-zero integer, failure is assumed.  If the
COMMAND recipe returns a string, the resulting string is passed to
`system` and is processed as above.

Note: a suffix rule where source and target suffixes are identical is
silently ignored.

    %suffix-rules

`%suffix-rules` is the list of suffix rules encountered in the build
script in the order in which they were listed, converted into an
internal format.

    %target-rules

`%target-rules` is the list of target rules encountered in the build
script in the order in which they were listed, converted into an
internal format.

Example suffix rules:

```scheme
(-> ".c" ".o"
    (~ ($ CC) ($ CFLAGS) "-c" "-o" $@ $<))

(-> ".sh" ""
    (~ "cp" $< $@)
    (~ "chmod a+x" $@))
```

## Recipe Helpers

Concatenate elements with `~`.  `~` inserts spaces between the
elements.

Elements can be:
- strings
- procedures that return strings
- `%makevars` hash-table references
- automatic variables
- anything whose string representation as created by
  `(format #f "~A" ...)` makes sense

Any procedures are applied lazily, when the rule is executed.

```scheme
(~ "string" (lambda () "string") ($ KEY) $@ 100)
```

The compose helpers each return a pair where the CAR is a symbol tag
and the CDR is a thunk that produces the composed string.

    string-compose element ...            (alias: ~)
    ignore-error-compose element ...      (alias: ~-)
    silent-compose element ...            (alias: ~@)
    always-execute-compose element ...    (alias: ~+)
    continue-on-error-compose element ... (alias: ~k)

`string-compose` (aka `~`) takes as arguments one or more elements. It
converts the elements to strings and concatenates the strings,
appending spaces between them.  The conversion to strings happens as
if by `display`.  For elements that are procedures, they are executed
and their result is used instead.  It is returned as a pair where the
`car` is the symbol `'default`.

`ignore-error-compose` (aka `~-`) is like `~` but returns a pair with
the tag `'ignore-error`.  When passed as a recipe, it causes the
recipe not to end execution, even if an error is signaled.

`silent-compose` (aka `~@`) is like `~`, but it does not print the
resulting string to the output port, except with verbose output.

`always-execute-compose` (aka `~+`) is like `~`, but it forces the
line to always be executed, even if the `--no-execution` option was
chosen.

`continue-on-error-compose` (aka `~k`) is like `~`, but after a
failure it continues to the next recipe in the rule instead of
stopping.

## Automatic Variables

Recipes can contain the following automatic variables.  These are
procedures of zero arguments that are set before each recipe runs.

    $@    the target name
    $*    the target name with the filename suffix removed
    $<    the first prerequisite
    $^    the prerequisites, as a single space-separated string
    $$^   the prerequisites, as a scheme list of strings
    $?    the prerequisites that are files newer than the target file,
          as a single space-separated string
    $$?   the prerequisites that are files newer than the target file,
          as a scheme list of strings

Note: these are global variables and are not thread safe.

## Makevars

Makefile scripts may take advantage of a special variable type
called a makevar.  In scheme terms, makevars are entries in a
`%makevars` hash table that have special accessor syntax.

- The makevar names — the keys — are strings.
- The makevar values are either strings or procedures that take no
  arguments that return strings.

There are five sources for makevars, listed from highest to lowest
priority (lower number wins):

1. Script assignments (`:=`, `?=`)
2. Command-line assignments (`KEY=VALUE`)
3. `MAKEFLAGS` assignments (when `--environment` or
   `--elevate-environment` is enabled)
4. Environment variables (when `--environment` or
   `--elevate-environment` is enabled)
5. Built-ins (when `--builtins` is enabled)

During initialization, sources are loaded in reverse order (5, 4, 3,
2, 1).  Each higher-priority source overwrites values from
lower-priority sources.

This priority is modified by the `--elevate-environment` flag.  When
set, script assignments (#1) *do not* override values from
command-line (#2), `MAKEFLAGS` (#3), or environment (#4).  They *do*
still override built-ins (#5).

Notes:

1. `SHELL` and `MAKEFLAGS` are never imported as makevars.
2. `MAKEFLAGS` is still parsed for `KEY=VALUE` entries when
   environment import is enabled.
3. In strict mode (`--strict`), referencing an unset makevar raises an
   error.
4. Without strict mode, unset makevars return an empty string.

The library provides the following procedures for makevars:

    lazy-assign key [val]

> `lazy-assign` sets an entry in the makevars hash table.  KEY must be
> a string or a thunk that evaluates to a string.  Likewise VAL must
> be a string or a thunk that evaluates to a string.

> If KEY is a thunk, it is immediately evaluated to a string to use as
> the key in the hash table entry.

> If VAL is a thunk, it is stored as a *promise* to be evaluated
> later. The promise will be evaluated the first time this key is
> referenced.

> If VAL is not given, the empty string will be used.

    ?= key [val]

> This is a syntax version of lazy-assign where KEY should be an
> unquoted symbol, e.g.

        (?= foo "bar")  ==>  (lazy-assign "foo" "bar")

    assign key [val]

> `assign` is the same as `lazy-assign` above, except that if VAL is a
> thunk it is immediately evaluated to a string and that string is
> used as the value in the hash table entry.

    := key [val]

> This is a syntax version of `assign` where KEY should be an unquoted
> symbol, e.g.

        (:= foo "bar")  ==>  (assign "foo" "bar")

    reference key [transformer]

> `reference` looks up KEY in the `%makevars` hash table.  If it is
> found, the value is returned as a string.

> *IMPORTANT!* If it is not found, an empty string is returned (or an
> error is raised in strict mode).  This is because it is a common
> practice in makefiles to use makevars that may or may not be defined
> by environment variables.  With verbose output, a warning will be
> printed when a key cannot be found.

> If the value was stored using `lazy-assign` and is a *promise*, this
> procedure *forces* the promise and returns the resulting string.
> The value in the hash table is also updated to this string.

> The optional `transformer` should be a function that takes a string
> and returns a string.  It will be applied to every space-separated
> token in the value.

    $ key [transformer]

> This is a syntax version of `reference`, where KEY should be an
> unquoted symbol, e.g.

        ($ CC) ==> (reference "CC")

    Q key [transformer]

> Same as `$`, but wraps the returned value in double quotes.

    reference-func key

> `reference-func` returns a procedure of zero arguments that will,
> when called, look up a key as described in `reference` above.
> Returns `#f` if the key is unset.

    $$ key

> This is a syntax version of `reference-func`, where KEY should be
> an unquoted symbol, e.g.

        ($$ CC) ==> (reference-func "CC")

    %makevars

> This is the hash table.  You are not meant to access it directly,
> but with the functions above.  If you do use it directly, the VALUE
> is a cons where the CAR is a string or promise and the CDR is the
> priority level (an integer).

## The Build Algorithm

The initial target is given on the command line. If no target was
given on the command line, the first entry in the target list is used.

For each top-level target, create an n-ary tree of prerequisites.  If
a target doesn't have an explicit rule, but has a suffix that appears
in one or more suffix rules, it searches for possible prerequisites
that would fulfill a suffix rule. Continue until the tree is populated.

Then for each node, try to compute timestamps for each target, if they
exist.

A node is considered up-to-date if it has an mtime and all of its
children have mtimes that are less than or equal to the parent's
mtime.  Up-to-date nodes pass without running recipes.

In a depth-first traversal, build each node:

- For **target rules** (AND logic): all children must pass before the
  parent's recipes run.
- For **suffix rules** (OR logic): the parent's recipes run as soon as
  any one child passes, trying each suffix rule in order.

If a build recipe fails:
- With `--ignore-errors`: mark current node as pass and keep going.
- With `--continue-on-error`: skip the failed target and proceed to
  the next top-level target.
- Otherwise: stop immediately.

Once a top-level target completes (or fails), move on to the next
top-level target (which only happens if multiple targets are given on
the command line).

## `install-alternate-system-driver`

    install-alternate-system-driver proc

Replaces the default `system` procedure used to run shell command
strings with PROC.  Returns the previously installed driver.  This is
useful for testing or for custom command execution.

## Built-in Rules and Makevars

If the `--builtins` option is given, there are some builtin suffix
rules and *makevars* that are present by default.  These include the
following.  You can add more builtins by updating
`potato/builtins.scm`.

### Built-in Makevars

    MAKE=make
    AR=ar
    ARFLAGS=-rv
    YACC=yacc
    YFLAGS=
    LEX=lex
    LFLAGS=
    LDFLAGS=
    CC=cc
    CFLAGS=-g -O2
    FC=gfortran
    FFLAGS=-O1
    GUILE=guile
    GUILD=guild
    GFLAGS=-W2

### Built-in Suffix Rules

```scheme
(-> ".c" ".o"
    (~ ($ CC) ($ CFLAGS) "-c" $<))

(-> ".f" ".o"
    (~ ($ FC) ($ FFLAGS) "-c" $<))

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
    (~ ($ GUILD) "compile" ($ GFLAGS) $<))
```

    (-> ".l" ".c"
        (~ ($ LEX) ($ LFLAGS) $<)
        (~ "mv lex.yy.c" $@))

## Debug commands

These commands modify how rules are interpreted or executed

FIXME
