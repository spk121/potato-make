## Makefile.am -- top level

SUBDIRS = . doc

# 'make distcheck' builds the manual; use HTML so TeX is not required.
AM_DISTCHECK_DVI_TARGET = html

#################
# Guile Modules #
#################

pkgguilesitedir = $(guilesitedir)/potato
pkgguileobjectdir = $(guileobjectdir)/potato

# Listed in dependency order.
SOURCES = \
  potato/builtins.scm \
  potato/exceptions.scm \
  potato/text.scm \
  potato/makevars.scm \
  potato/rules.scm \
  potato/make.scm \
  potato/configure.scm \
  potato/lib.scm

dist_pkgguilesite_DATA = $(SOURCES)

if GUILD
GOBJECTS = $(SOURCES:%.scm=%.go)

pkgguileobject_DATA = $(GOBJECTS)

GUILEC_FLAGS = \
  -Warity-mismatch \
  -Wformat \
  -Wmacro-use-before-definition \
  -Wunbound-variable

SUFFIXES = .scm .go

.scm.go:
	$(AM_V_GEN)$(MKDIR_P) "$(@D)" && \
	GUILE_AUTO_COMPILE=0 \
	$(GUILD) compile $(GUILEC_FLAGS) \
	  -L "$(abs_top_srcdir)" -o "$@" "$<"

# Each module is compiled after the modules it imports.
potato/makevars.go: potato/builtins.go potato/exceptions.go potato/text.go
potato/rules.go: potato/makevars.go
potato/make.go: potato/rules.go
potato/configure.go: potato/make.go
potato/lib.go: potato/makevars.go

# Install the compiled files after the sources, so that their
# timestamps are newer and Guile doesn't consider them stale.
install-data-hook:
	@for f in $(GOBJECTS); do \
	  touch "$(DESTDIR)$(guileobjectdir)/$$f"; \
	done
endif

#################
#     Tests     #
#################

TEST_EXTENSIONS = .scm

SCM_LOG_COMPILER = $(top_builddir)/pre-inst-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -s

AM_TESTS_ENVIRONMENT = \
  abs_top_srcdir="$(abs_top_srcdir)"; export abs_top_srcdir; \
  POTATO_MAKE_VERSION="$(VERSION)"; export POTATO_MAKE_VERSION; \
  GUILE="$(GUILE)"; export GUILE;

TESTS = \
  tests/exceptions.scm \
  tests/text.scm \
  tests/makevars.scm \
  tests/recipe-helpers.scm \
  tests/automatic-variables.scm \
  tests/target-rules.scm \
  tests/suffix-rules.scm \
  tests/build.scm \
  tests/make.scm \
  tests/configure.scm \
  tests/lib.scm \
  tests/examples.scm

test: check

.PHONY: test

#################
#   Variables   #
#################

EXTRA_DIST = \
  LICENSE \
  NEWS \
  README \
  INSTALL \
  pre-inst-env.in \
  tests/common.scm \
  $(TESTS) \
  examples/hello.scm \
  examples/simple.scm

# The example sources include file names with spaces, which Automake
# variables can't express, so that directory is copied by hand.
dist-hook:
	cp -R "$(srcdir)/examples/src" "$(distdir)/examples/src"
	find "$(distdir)/examples/src" -type f ! -name '*.c' ! -name '*.h' \
	  ! -name '*.scm' -exec rm -f '{}' +

# SRFI-64 writes a detailed log for each test group into the
# directory the tests are run from.
CLEANFILES = \
  $(GOBJECTS) \
  $(TESTS:tests/%.scm=%.log)
