#!/bin/sh

PACKAGE="sawfish"

if [ "x$1" = "x--help" ]; then
    echo Usage: ./update.sh langcode[.po]
    echo --help                  display this help and exit
    echo
    echo Examples of use:
    echo ./update.sh da[.po] -- updates the da.po file
    exit 0
elif [ "x$1" = "x" ]; then 
    echo "Type language code, e.g. da / da.po for Danish, etc."
    exit 0
fi

PO="${1}"

if [ -e "${PO}.po" ]; then
    PO="${PO}.po"
fi

echo "Now merging $PO with $PACKAGE.pot, and creating an updated $PO ..."

mv ${PO} ${PO}.old && msgmerge ${PO}.old ${PACKAGE}.pot -o ${PO} \
    && rm ${PO}.old;

msgfmt $PO --statistics -o /dev/null
