#!/bin/sh

# no argument .stc to .sc translator

if test $# != 0 ; then echo "stc-to-sc < stc-file > sc-file" ; exit 1 ; fi
stsc3 translate stc sc
