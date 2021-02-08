(define-module (potato builtins)
  #:export (builtin-makevars
            builtin-rules))

(define builtin-makevars
  '(("MAKE" . "make")
    ("AR"   . "ar")
    ("ARFLAGS" . "-rv")
    ("YACC" . "yacc")
    ("YFLAGS" . "")
    ("LEX" . "lex")
    ("LFLAGS" . "")
    ("LDFLAGS" . "")
    ("CC" . "cc")
    ("CFLAGS" . "-g -O2")
    ("FC" . "gfortran")
    ("FFLAGS" . "-O1")
    ("GUILE" . "guile")
    ("GUILD" . "guild")
    ("GFLAGS" . "-W2")))

#;(define builtin-rules
  `((".c"
     ""
     (,~ ,($ CC) ,($ CFLAGS) ,($ LDFLAGS) "-o" ,$@ ,$<))
    (".f"
     ""
     (,~ ,($ FC) ,($ FFLAGS) ,($ LDFLAGS) "-o" ,$@ ,$<))
    (".sh"
     ""
     (,~ "cp" ,$< ,$@)
     (,~ "chmod a+x" ,$@))
    (".c"
     ".o"
     (,~ ,($ CC) ,($ CFLAGS) -c ,$<))
    (".f"
     ".o"
     (,~ ,($ FC) ,($ FFLAGS) -c ,$<))
    (".y"
     ".o"
     (,~ ,($ YACC) ,($ YFLAGS) ,$<))
    (".l"
     ".o"
     (,~ ,($ LEX) ,($ LFLAGS) ,$<)
     (,~ ,($ CC) ,($ CFLAGS) "-c" lex.yy.c)
     "rm -f lex.yy.c"
     (,~ "mv lex.yy.o" ,$@))
    (".y"
     ".c"
     (,~ ,($ YACC) ,($ YFLAGS) ,$<)
     (,~ "mv y.tab.c" ,$@))
    (".l"
     ".c"
     (,~ ,($ LEX) ,($ LDFLAGS) ,$<)
     ("mv lex.yy.c" ,$@))
    (".scm"
     ".go"
     (,~ ,($ GUILD) "compile" ,($ GFLAGS) ,$<))))
