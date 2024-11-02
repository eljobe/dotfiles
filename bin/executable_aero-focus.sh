#!/bin/zsh
focused=$(aerospace list-windows --focused --format "%{app-bundle-id}")
if [[ "$focused" == "org.gnu.Emacs" ]]; then
    aerospace mode emacs
else
    aerospace mode main
fi
echo "" >> $LOG
