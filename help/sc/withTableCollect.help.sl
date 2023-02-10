# withTableCollect -- arithmetic

Given vectors _u_ & _v_ generates the table _[[u1v1, u1v2 ...], [u2v1, u2v2 ...], ...]_.
Can calculate the _outer product_ (⊗) of two vectors.

```
[10, 20, 30, 40, 50].withTableCollect([1, 2, 3], plus:/2) = [[11, 12, 13], [21, 22, 23], [31, 32, 33], [41, 42, 43], [51, 52, 53]]
```

The behaviour when each side is a matrix is quite subtle.

```
var x = [4, 4.5], y = [2, 3, 5, 6], z = [x, y]; z.withTableCollect(z, times:/2) = [x * [x, y], y * [x, y]]
var x = [4, 4.5], y = [2, 3, 5, 6]; [x * [x, y], y * [x, y]] = [[[16, 18], [9, 13.5, 22.5, 27]], [[8, 9], [6, 9, 15, 18], [20, 22.5], [12, 18, 30, 36]]]
```

* * *

See also: _withCrossedCollect_
