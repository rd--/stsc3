Derived from Chris Rathman's Terse Guide to Smalltalk

- <http://wiki.squeak.org/squeak/5699>
- <https://www.angelfire.com/tx4/cus/notes/smalltalk.html>

[sc3-rdl](https://gitlab.com/rd--/sc3-rdl) has a method _String>>terseGuideSummary_ that can evaluate 'Terse Guides' as simple test suites.

Files are organised into paragraphs, separated by blank lines.

Each paragraph has a title or subject line.
If the subject line contains the string " - !" the paragraph is skipped over.
Otherwise subsequent lines are all simple expressions that are expected to evaluate to true.
Lines are evaluated in sequence.

The summary indicates the number of tests per section, and how many of these passed.
If the task completes a summary of total tests and total passes is printed.

```
// String>>terseGuideSummary ; Total => 765 / 765
"/home/rohan/sw/stsc3/help/terse/terse.scd".terseGuideSummary
```

The terse.scd file is written to be comprehensible to the .stc parser (ie. it is in the .stc subset of .scd)
