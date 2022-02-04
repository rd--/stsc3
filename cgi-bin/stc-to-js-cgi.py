#!/usr/bin/python3

import cgi
import cgitb
import subprocess
import sys

sys.stdout.write("Content-type: text/plain; charset=utf-8\n\n")
fs = cgi.FieldStorage()
stcText = fs['stc'].value
process = subprocess.Popen(['stc-to-js'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, universal_newlines=True)
process.stdin.write(stcText);
jsText = process.communicate()[0]
sys.stdout.write(jsText)
sys.stdout.flush()
process.stdin.close()
