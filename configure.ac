#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([blip], [0.08], [cjm@fruitfly.org])
AC_CONFIG_AUX_DIR([tools])

AC_PROG_INSTALL

AC_SUBST(SWIPL)
AC_ARG_WITH(swipl,
	[  --with-swipl=PATH       location of SWI-Prolog executable ],
	[ SWIPL="$withval"], [SWIPL=])
if test -z "$SWIPL"
then
	AC_PATH_PROGS([SWIPL], swipl swiprolog pl prolog)
fi
if test -z "$SWIPL"
then
	AC_MSG_ERROR([SWI-Prolog binary not found])
fi

AC_SUBST(OBO)
AC_ARG_WITH(find-obo-dir,
	[  --with-obo=PATH       location of OBO ontologies ],
	[ OBO="$withval"], [OBO="$HOME/cvs/obo"])

AC_MSG_NOTICE([Running $PL -dump-runtime-variables])
eval `$SWIPL -dump-runtime-variables`
AC_MSG_RESULT([         PLBASE=$PLBASE])
AC_MSG_RESULT([         PLVERSION=$PLVERSION])


AC_SUBST(PLLIBDIR)
AC_ARG_WITH(pllibdir,
	[  --with-pllibdir=DIR     installation directory for prolog modules ],
	[ PLLIBDIR="$withval"], [PLLIBDIR="$PLBASE/library"])

AC_CONFIG_FILES([ Makefile
                  bin/bpr-test ])


AC_OUTPUT
