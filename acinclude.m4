AC_DEFUN([AC_CHECK_EMACS],
 [dnl Check for Emacsen.

  dnl Apparently, if you run a shell window in Emacs, it sets the EMACS
  dnl environment variable to 't'.  Lets undo the damage.
  test "$EMACS" = t && EMACS=

  dnl Ignore cache.
  unset ac_cv_prog_EMACS; unset ac_cv_prog_XEMACS;

  AC_ARG_WITH(emacs,
   [  --with-emacs=EMACS      compile with EMACS [EMACS=emacs, mule...]],
   [if test "$withval" = yes -o -z "$withval"; then
      AC_CHECK_PROGS(EMACS, emacs xemacs mule, emacs)
    else
      AC_CHECK_PROG(EMACS, $withval, $withval, emacs)
    fi])
  AC_ARG_WITH(xemacs,
   [  --with-xemacs=XEMACS    compile with XEMACS [XEMACS=xemacs]],
   [if test "$withval" = yes -o -z "$withval"; then
      AC_CHECK_PROG(XEMACS, xemacs, xemacs, xemacs)
    else
      AC_CHECK_PROG(XEMACS, $withval, $withval, xemacs)
    fi
    EMACS=$XEMACS],
   [XEMACS=xemacs
    test -z "$EMACS" && AC_CHECK_PROGS(EMACS, emacs xemacs mule, emacs)])
  AC_SUBST(EMACS)
  AC_SUBST(XEMACS)])

AC_DEFUN([AC_EMACS_LISP], [
elisp="$2"
if test -z "$3"; then
	AC_MSG_CHECKING(for $1)
fi
AC_CACHE_VAL(EMACS_cv_SYS_$1,[
	OUTPUT=./conftest-$$
	echo ${EMACS}' -batch -eval '\''(let ((x '${elisp}')) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil "'${OUTPUT}'" nil 5))'\' >& AC_FD_CC 2>&1
	eval ${EMACS}' -batch -eval '\''(let ((x '${elisp}')) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil "'${OUTPUT}'" nil 5))'\' >& AC_FD_CC 2>&1
	if test -f ${OUTPUT}; then
		retval=`cat ${OUTPUT}`
		echo "=> ${retval}" >& AC_FD_CC 2>&1
		rm -f ${OUTPUT}
		EMACS_cv_SYS_$1=$retval
	else
		EMACS_cv_SYS_$1=
	fi
])
$1=${EMACS_cv_SYS_$1}
if test -z "$3"; then
	AC_MSG_RESULT($$1)
fi
])

AC_DEFUN([AC_CHECK_EMACS_FLAVOR],
 [AC_MSG_CHECKING([what flavor does $EMACS have])

  dnl Ignore cache.
  unset EMACS_cv_SYS_flavor;

  AC_EMACS_LISP(flavor,
    (cond ((featurep (quote xemacs)) \"XEmacs\")\
          ((boundp (quote MULE)) \"MULE\")\
          (t \"FSF Emacs\")),
    "noecho")
  case $EMACS_cv_SYS_flavor in
  XEmacs)
    EMACS_FLAVOR=xemacs;;
  MULE)
    EMACS_FLAVOR=mule;;
  *)
    EMACS_FLAVOR=emacs;;
  esac
  AC_MSG_RESULT($EMACS_cv_SYS_flavor)])

AC_DEFUN([AC_PATH_LISPDIR], [
  AC_CHECK_EMACS_FLAVOR
  if test "$prefix" = NONE; then
	AC_MSG_CHECKING([prefix for your Emacs])
	AC_EMACS_LISP(prefix,(expand-file-name \"..\" invocation-directory),"noecho")
	prefix=${EMACS_cv_SYS_prefix}
	AC_MSG_RESULT($prefix)
  fi
  AC_ARG_WITH(lispdir,
    [  --with-lispdir=DIR      Where to install lisp files
                          (for XEmacs package, use --with-packagedir instead)],
    lispdir=${withval})
  AC_MSG_CHECKING([where lisp files should go])
  if test -z "$lispdir"; then
    dnl Set default value
    theprefix=$prefix
    if test "$theprefix" = NONE; then
	theprefix=$ac_default_prefix
    fi
    lispdir="\$(datadir)/${EMACS_FLAVOR}/site-lis"
    for thedir in share lib; do
	potential=
	if test -d ${theprefix}/${thedir}/${EMACS_FLAVOR}/site-lisp; then
	   lispdir="\$(prefix)/${thedir}/${EMACS_FLAVOR}/site-lisp"
	   break
	fi
    done
  fi
  if test ${EMACS_FLAVOR} = xemacs; then
    AC_MSG_RESULT([$lispdir
         (it will be ignored when \"make install-package[[-ja]]\" is done)])
  else
    AC_MSG_RESULT([$lispdir])
  fi
  AC_SUBST(lispdir)
])

dnl
dnl Perform sanity checking and try to locate the Shimbun package
dnl
AC_DEFUN([AC_CHECK_SHIMBUN], [
  AC_MSG_CHECKING(have shimbun)

  dnl Ignore cache.
  unset EMACS_cv_SYS_shimbun_dir;

  AC_ARG_WITH(shimbun,[  --with-shimbun[[=ARG]]    Use shimbun [[ARG=yes]]],
    [if test "$withval" = yes -o -z "$withval"; then
       HAVE_SHIMBUN=yes
     else
       HAVE_SHIMBUN=$withval
     fi], HAVE_SHIMBUN=yes)

  if test "${HAVE_SHIMBUN}" = yes; then
    AC_EMACS_LISP(shimbun_dir,(file-name-directory (locate-library \"shimbun\")),"noecho")
    SHIMBUN_DIR=$EMACS_cv_SYS_shimbun_dir
  fi

  if test "${HAVE_SHIMBUN}" != yes; then
    AC_MSG_RESULT(no)
  elif test -z "${SHIMBUN_DIR}"; then
    HAVE_SHIMBUN=no
    AC_MSG_RESULT(not found)
  else
    AC_MSG_RESULT(${HAVE_SHIMBUN})
 fi
])

dnl
dnl Perform sanity checking and try to locate the Gnus package
dnl
AC_DEFUN([AC_CHECK_GNUS], [
  AC_MSG_CHECKING(have gnus)

  dnl Ignore cache.
  unset EMACS_cv_SYS_gnus_dir;

  AC_ARG_WITH(gnus,[  --with-gnus[[=ARG]]       Use gnus [[ARG=yes]]],
    [if test "$withval" = yes -o -z "$withval"; then
       HAVE_GNUS=yes
     else
       HAVE_GNUS=$withval
     fi], HAVE_GNUS=yes)

  if test "${HAVE_GNUS}" = yes; then
    AC_EMACS_LISP(gnus_dir,(file-name-directory (locate-library \"gnus\")),"noecho")
    GNUS_DIR=$EMACS_cv_SYS_gnus_dir
  fi

  if test "${HAVE_GNUS}" != yes; then
    AC_MSG_RESULT(no)
  elif test -z "${GNUS_DIR}"; then
    HAVE_GNUS=no
    AC_MSG_RESULT(not found)
  else
    AC_MSG_RESULT(${HAVE_GNUS})
 fi
])

dnl
dnl Perform sanity checking and try to locate the Wanderlust package
dnl
AC_DEFUN([AC_CHECK_WL], [
  AC_MSG_CHECKING(have wanderlust)

  dnl Ignore cache.
  unset EMACS_cv_SYS_wl_dir;

  AC_ARG_WITH(wl,[  --with-wl[[=ARG]]         Use wanderlust [[ARG=yes]]],
    [if test "$withval" = yes -o -z "$withval"; then
       HAVE_WL=yes
     else
       HAVE_WL=$withval
     fi], HAVE_WL=yes)

  if test "${HAVE_WL}" = yes; then
    AC_EMACS_LISP(wl_dir,(file-name-directory (locate-library \"wl\")),"noecho")
    WL_DIR=$EMACS_cv_SYS_wl_dir
  fi

  if test "${HAVE_WL}" != yes; then
    AC_MSG_RESULT(no)
  elif test -z "${WL_DIR}"; then
    HAVE_WL=no
    AC_MSG_RESULT(not found)
  else
    AC_MSG_RESULT(${HAVE_WL})
 fi
])

dnl
dnl Perform sanity checking and try to locate the Riece package
dnl
AC_DEFUN([AC_CHECK_RIECE], [
  AC_MSG_CHECKING(have riece)

  dnl Ignore cache.
  unset EMACS_cv_SYS_riece_dir;

  AC_ARG_WITH(riece,[  --with-riece[[=ARG]]      Use riece [[ARG=yes]]],
    [if test "$withval" = yes -o -z "$withval"; then
       HAVE_RIECE=yes
     else
       HAVE_RIECE=$withval
     fi], HAVE_RIECE=yes)

  if test "${HAVE_RIECE}" = yes; then
    AC_EMACS_LISP(riece_dir,(file-name-directory (locate-library \"riece\")),"noecho")
    RIECE_DIR=$EMACS_cv_SYS_riece_dir
  fi

  if test "${HAVE_RIECE}" != yes; then
    AC_MSG_RESULT(no)
  elif test -z "${RIECE_DIR}"; then
    HAVE_RIECE=no
    AC_MSG_RESULT(not found)
  else
    AC_MSG_RESULT(${HAVE_RIECE})
 fi
])

AC_DEFUN([AC_EXAMINE_PACKAGEDIR],
 [dnl Examine PACKAGEDIR.
  AC_EMACS_LISP(PACKAGEDIR,
    (let (package-dir)\
      (if (boundp (quote early-packages))\
	  (let ((dirs (delq nil (append (if early-package-load-path\
					    early-packages)\
					(if late-package-load-path\
					    late-packages)\
					(if last-package-load-path\
					    last-packages)))))\
	    (while (and dirs (not package-dir))\
	      (if (file-directory-p (car dirs))\
		  (setq package-dir (car dirs)\
			dirs (cdr dirs))))))\
      (or package-dir \"\")),
    "noecho")])

AC_DEFUN([AC_PATH_PACKAGEDIR],
 [dnl Check for PACKAGEDIR.
  if test ${EMACS_FLAVOR} = xemacs; then
    AC_MSG_CHECKING([where the XEmacs package is])
    AC_ARG_WITH(packagedir,
      [  --with-packagedir=DIR   package DIR for XEmacs],
      [if test "$withval" != yes -a -n "$withval"; then
	PACKAGEDIR=$withval
      else
	AC_EXAMINE_PACKAGEDIR
      fi],
      AC_EXAMINE_PACKAGEDIR)
    if test -z "$PACKAGEDIR"; then
      AC_MSG_RESULT(not found)
    else
      AC_MSG_RESULT($PACKAGEDIR)
    fi
  else
    PACKAGEDIR=
  fi
  AC_SUBST(PACKAGEDIR)])

AC_DEFUN([AC_ADD_LOAD_PATH],
 [dnl Check for additional load path.
  AC_ARG_WITH(addpath,
   [  --with-addpath=PATH     search Emacs-Lisp libraries with PATH
                          use colons to separate directory names],
   [if test "$withval" != yes -a -n "$withval"; then
      AC_MSG_CHECKING([where to find the additional elisp libraries])
      ADDITIONAL_LOAD_PATH=$withval
      AC_MSG_RESULT($ADDITIONAL_LOAD_PATH)
    fi],
    ADDITIONAL_LOAD_PATH=)
  AC_SUBST(ADDITIONAL_LOAD_PATH)])

AC_DEFUN([AC_EXAMINE_MODULES],
 [dnl Examine install files.

  MODULES_MIXI="mixi.el mixi-utils.el mixi-atom.el mixi-ticker.el"
  MODULES_SHIMBUN="sb-mixi.el"
  MODULES_GNUS="mixi-gnus.el"
  MODULES_WL="mixi-wl.el"
  MODULES_RIECE="riece-mixi.el"

  MODULES=${MODULES_MIXI}
  MODULES_ALL="${MODULES} ${MODULES_SHIMBUN} ${MODULES_GNUS} ${MODULES_WL} ${MODULES_RIECE}"

  if test ${HAVE_SHIMBUN} = yes; then
    MODULES="${MODULES} ${MODULES_SHIMBUN}"
  fi
  if test ${HAVE_GNUS} = yes; then
    MODULES="${MODULES} ${MODULES_GNUS}"
  fi
  if test ${HAVE_WL} = yes; then
    MODULES="${MODULES} ${MODULES_WL}"
  fi
  if test ${HAVE_RIECE} = yes; then
    MODULES="${MODULES} ${MODULES_RIECE}"
  fi

  AC_SUBST(MODULES)
  AC_SUBST(MODULES_ALL)])
