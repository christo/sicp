#!/usr/bin/env zsh

which petite >/dev/null
RET=$?
if (( $RET != 0 )); then
    echo chezscheme is missing, looking for petite to run the script
else
    petite -q --script main.scm
fi
