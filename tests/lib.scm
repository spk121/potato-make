;;; lib.scm -- part of the Potato Make test suite.

(use-modules (potato make)
             (potato lib)
             (srfi srfi-64)
             (tests common))

(test-begin "lib")

(initialize '("test" "--verbosity=0"))

;;;;;;;;;;;;;;;;
;; words

(test-equal "words splits at whitespace."
  '("a" "b" "c") (words "a b c"))
(test-equal "words ignores leading, trailing and repeated whitespace."
  '("a" "b") (words "  a \t\n b  "))
(test-equal "words of an empty string is the empty list."
  '() (words ""))
(test-equal "words of a blank string is the empty list."
  '() (words "   "))
(test-equal "words keeps double-quoted spaces and strips the quotes."
  '("alpha bravo.c" "foo.c") (words "\"alpha bravo.c\" foo.c"))
(test-equal "words keeps single-quoted spaces and strips the quotes."
  '("alpha bravo.c" "foo.c") (words "'alpha bravo.c' foo.c"))
(test-equal "words joins quoted and unquoted parts of one word."
  '("dir/alpha bravo.c") (words "dir/\"alpha bravo\".c"))
(test-equal "words treats a single quote inside double quotes literally."
  '("it's here" "x") (words "\"it's here\" x"))
(test-equal "words treats a double quote inside single quotes literally."
  '("say \"hi\"" "x") (words "'say \"hi\"' x"))
(test-equal "words lets an unmatched quote run to the end of the string."
  '("a" "b c") (words "a \"b c"))
(test-error "words rejects a non-string." #t (words 'a))

(test-equal "W splits the value of a makevar into words."
  '("a.c" "b c.c")
  (begin
    (:= SRCS "a.c 'b c.c'")
    (W SRCS)))

;;;;;;;;;;;;;;;;
;; strip

(test-equal "strip removes leading and trailing whitespace."
  "a b" (strip "  a b  "))
(test-equal "strip collapses internal whitespace."
  "a b c" (strip "a  \t b \n c"))
(test-equal "strip of a blank string is empty."
  "" (strip "  \t "))
(test-equal "strip preserves quoted whitespace and removes the quotes."
  "a  b c" (strip " \"a  b\"   c "))
(test-equal "S strips the value of a makevar."
  "x y"
  (begin
    (:= PADDED "  x   y ")
    (S PADDED)))

;;;;;;;;;;;;;;;;
;; strip-quotes

(test-equal "strip-quotes removes both kinds of quotes."
  "abc" (strip-quotes "\"a\"'b'c"))

;;;;;;;;;;;;;;;;
;; findstring

(test-equal "findstring returns the needle when it is present."
  "b c" (findstring "b c" "a b c d"))
(test-equal "findstring returns an empty string when it is absent."
  "" (findstring "x" "a b c d"))
(test-equal "F looks up both of its arguments as makevars."
  "bar"
  (begin
    (:= NEEDLE "bar")
    (:= HAYSTACK "foo bar baz")
    (F NEEDLE HAYSTACK)))

;;;;;;;;;;;;;;;;
;; subst

(test-equal "subst replaces every occurrence."
  "fEEt on the strEEt" (subst "ee" "EE" "feet on the street"))
(test-equal "subst can delete text."
  "ac" (subst "b" "" "abbc"))
(test-equal "subst with an empty FROM returns the text unchanged."
  "abc" (subst "" "x" "abc"))
(test-equal "subst does not rescan its replacement."
  "aaaa" (subst "a" "aa" "aa"))
(test-equal "subst with no match returns the text unchanged."
  "abc" (subst "z" "y" "abc"))
(test-error "subst rejects a non-string." #t (subst 1 "a" "b"))

;;;;;;;;;;;;;;;;
;; patsubst

(test-equal "patsubst rewrites matching words."
  "x.c.o bar.o" (patsubst "%.c" "%.o" "x.c.c bar.c"))
(test-equal "patsubst leaves non-matching words alone."
  "foo.o bar.h" (patsubst "%.c" "%.o" "foo.c bar.h"))
(test-equal "patsubst without a wildcard matches whole words only."
  "bar foobar" (patsubst "foo" "bar" "foo foobar"))
(test-equal "patsubst with a prefix and a suffix in the pattern."
  "obj/foo.o" (patsubst "src/%.c" "obj/%.o" "src/foo.c"))
(test-equal "patsubst with no wildcard in the replacement."
  "X X" (patsubst "%.c" "X" "a.c b.c"))
(test-equal "patsubst normalizes whitespace."
  "a.o b.o" (patsubst "%.c" "%.o" "  a.c   b.c "))

;;;;;;;;;;;;;;;;
;; filter-in / filter-out

(test-equal "filter-in keeps words that match any pattern."
  "foo.c bar.c ugh.h"
  (filter-in "%.c %.h" "foo.c bar.c baz.s ugh.h"))
(test-equal "filter-in with a literal pattern."
  "baz.s" (filter-in "baz.s" "foo.c baz.s"))
(test-equal "filter-in with no match is empty."
  "" (filter-in "%.x" "foo.c baz.s"))
(test-equal "filter-out removes words that match any pattern."
  "baz.s"
  (filter-out "%.c %.h" "foo.c bar.c baz.s ugh.h"))
(test-equal "filter-out with no match keeps everything."
  "foo.c baz.s" (filter-out "%.x" "foo.c baz.s"))

;;;;;;;;;;;;;;;;
;; word functions

(test-equal "word returns the Nth word, counting from one."
  "bar" (word 2 "foo bar baz"))
(test-equal "word is empty when N is too large."
  "" (word 4 "foo bar baz"))
(test-equal "word is empty when N is less than one."
  "" (word 0 "foo bar baz"))
(test-equal "firstword." "foo" (firstword "foo bar baz"))
(test-equal "firstword of an empty string." "" (firstword ""))
(test-equal "lastword." "baz" (lastword "foo bar baz"))
(test-equal "lastword of an empty string." "" (lastword ""))
(test-equal "wordlist returns an inclusive range."
  "bar baz" (wordlist 2 3 "foo bar baz qux"))
(test-equal "wordlist clamps the end of the range."
  "baz qux" (wordlist 3 99 "foo bar baz qux"))
(test-equal "wordlist clamps the start of the range."
  "foo bar" (wordlist 0 2 "foo bar baz qux"))
(test-equal "wordlist is empty when the range is empty."
  "" (wordlist 3 2 "foo bar baz qux"))
(test-equal "sort-list sorts and removes duplicates."
  "bar foo lose" (sort-list "foo bar lose foo"))
(test-equal "sort-list of an empty string." "" (sort-list ""))

;;;;;;;;;;;;;;;;
;; filename functions

(test-equal "file-dir returns the directory part with a trailing slash."
  "src/ ./" (file-dir "src/foo.c hacks"))
(test-equal "file-dir of a nested path."
  "/usr/local/lib/" (file-dir "/usr/local/lib/libfoo.a"))
(test-equal "file-notdir returns the part after the last slash."
  "foo.c hacks" (file-notdir "src/foo.c hacks"))
(test-equal "file-notdir of a path with a trailing slash is empty."
  "" (file-notdir "src/"))
(test-equal "file-suffix returns the suffix including the dot."
  ".c .c" (file-suffix "src/foo.c src-1.0/bar.c"))
(test-equal "file-suffix ignores dots in directory names."
  "" (file-suffix "src-1.0/hacks"))
(test-equal "file-basename removes the suffix."
  "src/foo src-1.0/bar hacks" (file-basename "src/foo.c src-1.0/bar hacks"))
(test-equal "file-basename removes only the last suffix."
  "foo.tar" (file-basename "foo.tar.gz"))
(test-equal "addprefix." "src/foo src/bar" (addprefix "src/" "foo bar"))
(test-equal "addprefix of an empty list." "" (addprefix "src/" ""))
(test-equal "addsuffix." "foo.c bar.c" (addsuffix ".c" "foo bar"))
(test-equal "join-two concatenates pairwise."
  "a.c b.o" (join-two "a b" ".c .o"))
(test-equal "join-two keeps the extra words of the longer first list."
  "a.c b c" (join-two "a b c" ".c"))
(test-equal "join-two keeps the extra words of the longer second list."
  "a.c .o .h" (join-two "a" ".c .o .h"))

;;;;;;;;;;;;;;;;
;; filesystem functions

(test-equal "file-abspath leaves a clean absolute path alone."
  "/usr/lib" (file-abspath "/usr/lib"))
(test-equal "file-abspath resolves dot and dot-dot components."
  "/usr/lib" (file-abspath "/usr/./share/../lib"))
(test-equal "file-abspath does not climb above the root."
  "/etc" (file-abspath "/../etc"))
(test-equal "file-abspath of the root."
  "/" (file-abspath "/"))
(test-equal "file-abspath of dot-dot at the root is the root."
  "/" (file-abspath "/.."))
(test-equal "file-abspath removes repeated and trailing slashes."
  "/usr/lib" (file-abspath "/usr//lib/"))
(test-equal "file-abspath handles several words."
  "/a /b" (file-abspath "/a /b/c/.."))

(in-temp-dir
 (let ((cwd (getcwd)))
   (mkdir "sub")
   (touch "b.c")
   (touch "a.c")
   (touch "a.h")
   (touch "ab.c")
   (touch ".hidden.c")
   (touch "sub/z.c")
   (touch "sub/y.c")
   (touch "two words.c")
   (symlink "a.c" "link.c")

   (test-equal "file-abspath makes a relative path absolute."
     (string-append cwd "/sub/z.c")
     (file-abspath "sub/z.c"))
   (test-equal "file-abspath resolves a relative dot-dot."
     (string-append cwd "/a.c")
     (file-abspath "sub/../a.c"))
   (test-equal "file-abspath does not resolve symlinks."
     (string-append cwd "/link.c")
     (file-abspath "link.c"))
   (test-equal "file-abspath does not require the file to exist."
     (string-append cwd "/nope")
     (file-abspath "nope"))

   (test-equal "file-realpath resolves symlinks."
     (canonicalize-path "a.c")
     (file-realpath "link.c"))
   (test-equal "file-realpath omits files that do not exist."
     (canonicalize-path "a.c")
     (file-realpath "nope a.c nope2"))

   (test-equal "wildcard expands a star, sorted, without hidden files."
     "a.c ab.c b.c link.c two words.c" (wildcard "*.c"))
   (test-equal "wildcard matches hidden files when the pattern asks for them."
     ".hidden.c" (wildcard ".*.c"))
   (test-equal "wildcard expands a question mark."
     "a.c b.c" (wildcard "?.c"))
   (test-equal "wildcard expands in a subdirectory."
     "sub/y.c sub/z.c" (wildcard "sub/*.c"))
   (test-equal "wildcard keeps pattern order across several patterns."
     "a.h sub/y.c sub/z.c" (wildcard "*.h sub/*.c"))
   (test-equal "wildcard keeps a literal name only if the file exists."
     "a.c" (wildcard "a.c nope.c"))
   (test-equal "wildcard accepts a quoted pattern containing a space."
     "two words.c" (wildcard "'two w*.c'"))
   (test-equal "wildcard with no match is empty."
     "" (wildcard "*.xyz"))
   (test-equal "wildcard in a missing directory is empty."
     "" (wildcard "nodir/*.c"))))

(test-exit "lib")
