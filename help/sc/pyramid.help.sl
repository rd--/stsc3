# pyramid -- counting

- _pyramid(aSequence, patternType)_

Return a new sequence whose elements are those of _aSequence_ reordered using a "counting" algorithm.
_patternType_ is an integer and selects the counting algorithm.

Print counting algorithms:

```
[1, 6].do { :each | [1 .. 5].pyramid(each).postLine }
```
