#!/bin/sh

# no argument .stc to .scm translator

if test $# != 0 ; then echo "stc-to-scm < stc-file > scm-file" ; exit 1 ; fi
stsc3 translate stc scm
