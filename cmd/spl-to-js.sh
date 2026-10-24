#!/bin/sh

# no argument .stc to .js translator

if test $# != 0 ; then echo "stc-to-js < stc-file > js-file" ; exit 1 ; fi
stsc3 translate stc js
