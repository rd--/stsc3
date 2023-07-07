# withCrossedCollect -- arithmetic

A variant on _withTableCollect_ that writes folds away one layer of stucture.

```
[10, 20, 30, 40, 50].withCrossedCollect([1, 2, 3], plus:/2) = [11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53]
```

The matrix form is likewise folded.

```
var x = [[4, 4.5], [2, 3, 5, 6]];
 x.withCrossedCollect(x, times:/2) = [[16, 20.25], [8, 13.5, 20, 27], [8, 13.5, 20, 27], [4, 9, 25, 36]]
```

* * *

See also: withTableCollect
