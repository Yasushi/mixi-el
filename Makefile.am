MODULES_MIXI = mixi.el mixi-utils.el mixi-atom.el mixi-ticker.el
MODULES_SHIMBUN = sb-mixi.el
MODULES_GNUS = mixi-gnus.el
MODULES_WL = mixi-wl.el
MODULES_RIECE = riece-mixi.el
CONTRIB = contrib/atom.cgi contrib/atom.sh
EXTRA_DIST = COMPILE ChangeLog $(MODULES_MIXI) $(MODULES_SHIMBUN) \
             $(MODULES_GNUS) $(MODULES_WL) $(MODULES_RIECE) $(CONTRIB)
CLEANFILES = auto-autoloads.el custom-load.el *.elc
AUTOMAKE_OPTIONS = no-dependencies

FLAGS ?= -batch -q -no-site-file

MODULES = $(MODULES_MIXI)

if HAVE_SHIMBUN
MODULES += $(MODULES_SHIMBUN)

if HAVE_GNUS
MODULES += $(MODULES_GNUS)
endif

if HAVE_WL
MODULES += $(MODULES_WL)
endif

endif

if HAVE_RIECE
MODULES += $(MODULES_RIECE)
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
