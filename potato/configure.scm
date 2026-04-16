(define-module (potato configure)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
        #:use-module (ice-9 getopt-long)
        #:use-module (potato make)
  #:use-module (potato makevars)
  #:export (add-directory-variables))

(define %configure-option-spec
        '((prefix (value #t))
                (exec-prefix (value #t))
                (bindir (value #t))
                (sbindir (value #t))
                (libexecdir (value #t))
                (datarootdir (value #t))
                (datadir (value #t))
                (sysconfdir (value #t))
                (sharedstatedir (value #t))
                (localstatedir (value #t))
                (runstatedir (value #t))
                (includedir (value #t))
                (oldincludedir (value #t))
                (docdir (value #t))
                (infodir (value #t))
                (htmldir (value #t))
                (dvidir (value #t))
                (pdfdir (value #t))
                (psdir (value #t))
                (libdir (value #t))
                (lispdir (value #t))
                (localedir (value #t))
                (mandir (value #t))
                (man1dir (value #t))
                (man2dir (value #t))
                (srcdir (value #t))))

(define %configure-help-lines
        '("    --prefix=DIR"
                "    --exec-prefix=DIR"
                "    --bindir=DIR --sbindir=DIR --libexecdir=DIR"
                "    --datarootdir=DIR --datadir=DIR"
                "    --sysconfdir=DIR --sharedstatedir=DIR --localstatedir=DIR --runstatedir=DIR"
                "    --includedir=DIR --oldincludedir=DIR"
                "    --docdir=DIR --infodir=DIR --htmldir=DIR --dvidir=DIR --pdfdir=DIR --psdir=DIR"
                "    --libdir=DIR --lispdir=DIR --localedir=DIR"
                "    --mandir=DIR --man1dir=DIR --man2dir=DIR"
                "    --srcdir=DIR"
                "        configure extension: set GNU directory variables"))

(define (trim-trailing-slashes s)
  (let loop ((str s))
    (let ((len (string-length str)))
      (if (and (> len 1)
               (char=? (string-ref str (- len 1)) #\/))
          (loop (substring str 0 (- len 1)))
          str))))

(define (join-dir base leaf)
  (let* ((base2 (if (string? base)
                    base
                    (format #f "~a" base)))
         (leaf2 (if (string? leaf)
                    leaf
                    (format #f "~a" leaf)))
         (base3 (trim-trailing-slashes base2)))
    (if (string=? base3 "/")
        (string-append "/" leaf2)
        (string-append base3 "/" leaf2))))

(define* (add-directory-variables
          #:key
          (prefix #f)
          (exec-prefix #f)
          (bindir #f)
          (sbindir #f)
          (libexecdir #f)
          (datarootdir #f)
          (datadir #f)
          (sysconfdir #f)
          (sharedstatedir #f)
          (localstatedir #f)
          (runstatedir #f)
          (includedir #f)
          (oldincludedir #f)
          (docdir #f)
          (infodir #f)
          (htmldir #f)
          (dvidir #f)
          (pdfdir #f)
          (psdir #f)
          (libdir #f)
          (lispdir #f)
          (localedir #f)
          (mandir #f)
          (man1dir #f)
          (man2dir #f)
          (srcdir #f)
          (package-name "yourpkg"))
  "Populate GNU-style installation directory makevars.
Keyword arguments override defaults from the GNU make manual."

  (assign "prefix" (if prefix prefix "/usr/local"))
  (assign "exec_prefix"
          (if exec-prefix exec-prefix (reference "prefix" #f)))
  (assign "bindir"
          (if bindir bindir (join-dir (reference "exec_prefix" #f) "bin")))
  (assign "sbindir"
          (if sbindir sbindir (join-dir (reference "exec_prefix" #f) "sbin")))
  (assign "libexecdir"
          (if libexecdir libexecdir (join-dir (reference "exec_prefix" #f) "libexec")))
  (assign "datarootdir"
          (if datarootdir datarootdir (join-dir (reference "prefix" #f) "share")))
  (assign "datadir"
          (if datadir datadir (reference "datarootdir" #f)))
  (assign "sysconfdir"
          (if sysconfdir sysconfdir (join-dir (reference "prefix" #f) "etc")))
  (assign "sharedstatedir"
          (if sharedstatedir sharedstatedir (join-dir (reference "prefix" #f) "com")))
  (assign "localstatedir"
          (if localstatedir localstatedir (join-dir (reference "prefix" #f) "var")))
  (assign "runstatedir"
          (if runstatedir runstatedir (join-dir (reference "localstatedir" #f) "run")))
  (assign "includedir"
          (if includedir includedir (join-dir (reference "prefix" #f) "include")))
  (assign "oldincludedir"
          (if oldincludedir oldincludedir "/usr/include"))
  (assign "docdir"
          (if docdir docdir
              (join-dir (join-dir (reference "datarootdir" #f) "doc") package-name)))
  (assign "infodir"
          (if infodir infodir (join-dir (reference "datarootdir" #f) "info")))
  (assign "htmldir"
          (if htmldir htmldir (reference "docdir" #f)))
  (assign "dvidir"
          (if dvidir dvidir (reference "docdir" #f)))
  (assign "pdfdir"
          (if pdfdir pdfdir (reference "docdir" #f)))
  (assign "psdir"
          (if psdir psdir (reference "docdir" #f)))
  (assign "libdir"
          (if libdir libdir (join-dir (reference "exec_prefix" #f) "lib")))
  (assign "lispdir"
          (if lispdir lispdir
              (join-dir (join-dir (reference "datarootdir" #f) "emacs") "site-lisp")))
  (assign "localedir"
          (if localedir localedir (join-dir (reference "datarootdir" #f) "locale")))
  (assign "mandir"
          (if mandir mandir (join-dir (reference "datarootdir" #f) "man")))
  (assign "man1dir"
          (if man1dir man1dir (join-dir (reference "mandir" #f) "man1")))
  (assign "man2dir"
          (if man2dir man2dir (join-dir (reference "mandir" #f) "man2")))
  (assign "srcdir"
          (if srcdir srcdir ".")))

(define (configure-init-hook options)
        (add-directory-variables
         #:prefix (option-ref options 'prefix #f)
         #:exec-prefix (option-ref options 'exec-prefix #f)
         #:bindir (option-ref options 'bindir #f)
         #:sbindir (option-ref options 'sbindir #f)
         #:libexecdir (option-ref options 'libexecdir #f)
         #:datarootdir (option-ref options 'datarootdir #f)
         #:datadir (option-ref options 'datadir #f)
         #:sysconfdir (option-ref options 'sysconfdir #f)
         #:sharedstatedir (option-ref options 'sharedstatedir #f)
         #:localstatedir (option-ref options 'localstatedir #f)
         #:runstatedir (option-ref options 'runstatedir #f)
         #:includedir (option-ref options 'includedir #f)
         #:oldincludedir (option-ref options 'oldincludedir #f)
         #:docdir (option-ref options 'docdir #f)
         #:infodir (option-ref options 'infodir #f)
         #:htmldir (option-ref options 'htmldir #f)
         #:dvidir (option-ref options 'dvidir #f)
         #:pdfdir (option-ref options 'pdfdir #f)
         #:psdir (option-ref options 'psdir #f)
         #:libdir (option-ref options 'libdir #f)
         #:lispdir (option-ref options 'lispdir #f)
         #:localedir (option-ref options 'localedir #f)
         #:mandir (option-ref options 'mandir #f)
         #:man1dir (option-ref options 'man1dir #f)
         #:man2dir (option-ref options 'man2dir #f)
         #:srcdir (option-ref options 'srcdir #f)))

(register-extension!
 #:id 'configure
 #:option-spec %configure-option-spec
 #:help-lines %configure-help-lines
 #:init-hook configure-init-hook)
