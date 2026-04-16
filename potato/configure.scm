(define-module (potato configure)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (potato makevars)
  #:export (add-directory-variables))

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
