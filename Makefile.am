MODULES_MIXI = mixi.el mixi-utils.el mixi-atom.el
MODULES_ALL = $(MODULES_MIXI) sb-mixi.el mixi-gnus.el mixi-wl.el
EXTRA_DIST = COMPILE ChangeLog $(MODULES_ALL)
CLEANFILES = auto-autoloads.el custom-load.el *.elc
AUTOMAKE_OPTIONS = no-dependencies

FLAGS ?= -batch -q -no-site-file

if HAVE_SHIMBUN
MODULES = $(MODULES_ALL)
else
MODULES = $(MODULES_MIXI)
endif

all: elc

elc:
	$(EMACS) $(FLAGS) -l COMPILE -f mixi-compile $(MODULES)

install: elc
	$(EMACS) $(FLAGS) -l COMPILE -f mixi-install $(lispdir) $(MODULES)

package:
	$(XEMACS) $(FLAGS) -l COMPILE -f mixi-compile-package $(MODULES)

install-package: package
	$(XEMACS) $(FLAGS) -l COMPILE -f mixi-install-package $(PACKAGEDIR) \
		$(MODULES)
