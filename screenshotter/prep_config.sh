#!/bin/bash
THEME=$1
echo
echo Theme to test: "$THEME"
echo
# Edit this script to remove that stupid confirmation.
#echo Really delete ~/.sawfish/custom and ~/.sawfishrc for theme testing?
#echo -n "Make sure you don't delete something important! (enter 'n' to abort) : ";
#read x; if [ "$x" = "n" ]; then exit; fi

rm -f ~/.sawfish/rc
rm -rf ~/.sawfish/custom

# TODO: create a sawfish rc

#echo "(custom-set-typed-variable (quote default-frame-style) (quote $1) \
#(quote frame-style))" > ~/.sawfish/custom
echo "(defvar-setq default-frame-style '$THEME \"Default frame style\")" > ~/.sawfish/rc
cat >> ~/.sawfish/rc << EOF
(custom-set-typed-variable
  (quote match-window-profile)
  (quote ((((WM_NAME . "^Normal$")) (frame-type . normal))
          (((WM_NAME . "^Shaded$")) (shaded . #t))
          (((WM_NAME . "^Title-only$")) (frame-type . title-only))
          (((WM_NAME . "^Border-only$")) (frame-type . border-only))
          (((WM_NAME . "^Top-border$")) (frame-type . top-border))
          (((WM_NAME . "^None$")) (frame-type . none))
          ))
  (quote match-window)
  (quote sawfish.wm.ext.match-window))
  )

EOF
