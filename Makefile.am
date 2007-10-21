CONTRIB = contrib/atom.cgi contrib/atom.sh
EXTRA_DIST = INSTALL-CVS ChangeLog @MODULES_ALL@ $(CONTRIB) \
	infohack.el ptexinfmt.el texinfo.tex
CLEANFILES = auto-autoloads.el custom-load.el *.elc
DISTCLEANFILES = COMPILE
AUTOMAKE_OPTIONS = no-dependencies

info_TEXINFOS = mixi-ja.texi

FLAGS ?= -batch -q -no-site-file

all: elc

elc:
	$(EMACS) $(FLAGS) -l COMPILE -f mixi-compile

install: elc install-info
	$(EMACS) $(FLAGS) -l COMPILE -f mixi-install $(lispdir)

package:
	$(XEMACS) $(FLAGS) -l COMPILE -f mixi-compile-package

install-package: package
	$(XEMACS) $(FLAGS) -l COMPILE -f mixi-install-package $(PACKAGEDIR)
	$(MAKE) infodir=$(PACKAGEDIR)/info install

compile-individually:
	@for i in `$(EMACS) $(FLAGS) -l COMPILE -f mixi-examine`; do \
		echo $(EMACS) $(FLAGS) -l COMPILE \
			-f mixi-compile-module $$i; \
		$(EMACS) $(FLAGS) -l COMPILE \
			-f mixi-compile-module $$i; \
	done

%.info: %.texi
	$(EMACS) $(FLAGS) -l ./infohack.el -f batch-makeinfo $<
