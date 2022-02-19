#!/usr/bin/python3

import cgi
import cgitb
import os
import subprocess
import sys

sys.stdout.write("Content-type: text/plain; charset=utf-8\n\n")
dir = os.path.dirname(os.path.realpath(__file__))
fs = cgi.FieldStorage()
stcText = fs['stc'].value
process = subprocess.Popen([dir + '/stc-to-st'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, universal_newlines=True)
process.stdin.write(stcText);
stText = process.communicate()[0]
sys.stdout.write(stText)
sys.stdout.flush()
process.stdin.close()
