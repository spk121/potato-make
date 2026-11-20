dnl Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([Potato Make], [1.0.1], [https://github.com/spk121/potato-make/issues],
        [potato-make], [https://github.com/spk121/potato-make])

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_SRCDIR([potato/make.scm])
AM_INIT_AUTOMAKE([1.16 foreign subdir-objects color-tests -Wall -Wno-portability])
AM_SILENT_RULES([yes])

################
# Check for programs

AC_PROG_INSTALL
AC_PROG_MKDIR_P

# From guile.m4, which is shipped with the Guile development files.
GUILE_PKG([3.0 2.2])
GUILE_PROGS

AC_ARG_ENABLE([guild],
              [AS_HELP_STRING([--disable-guild],
                              [don't byte-compile the Guile modules])],
              [AS_CASE(["${enableval}"],
                       [yes], [guild=true],
                       [no], [guild=false],
                       [AC_MSG_ERROR([bad value ${enableval} for --enable-guild])])],
              [guild=true])
AM_CONDITIONAL([GUILD], [test x$guild = xtrue])

################
# Installation directories
#
# The modules are installed relative to $prefix so that non-root
# installs and 'make distcheck' work.  To install into the directories
# that Guile searches by default, configure with the same prefix as
# Guile itself, e.g. --prefix=/usr.

guilesitedir='${datadir}/guile/site/${GUILE_EFFECTIVE_VERSION}'
guileobjectdir='${libdir}/guile/${GUILE_EFFECTIVE_VERSION}/site-ccache'
AC_SUBST([guilesitedir])
AC_SUBST([guileobjectdir])

################
# Output

AC_CONFIG_FILES([Makefile doc/Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_OUTPUT
