#!/bin/sh

# no argument .stc to .st translator (for stc.el)

if test $# != 0 ; then echo "stc-to-st < stc-file > st-file" ; exit 1 ; fi
stsc3 translate stc st
