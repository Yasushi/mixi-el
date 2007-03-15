#!/bin/sh

EMACS=emacs
FLAGS="-batch -q -no-site-file"
ARGS="-l mixi-atom -f mixi-atom-file"

while read line
do
  LISP="${LISP} ${line}"
done <<EOF
(setq mixi-backend 'url)
(setq mixi-atom-file "~/Sites/atom.xml")
(setq mixi-atom-title "[mixi] New BBSes")
(setq mixi-atom-syndication-list '((mixi-get-new-bbses . 10)))
EOF

${EMACS} ${FLAGS} -eval "(progn ${LISP})" ${ARGS} 2> /dev/null
