# crossedMultiply -- arithmetic

Calculate the _outer product_ (⊗) of two vectors.

```
[10, 20, 30, 40, 50].crossedMultiply([1, 2, 3]) = [10, 20, 30, 20, 40, 60, 30, 60, 90, 40, 80, 120, 50, 100, 150]
[10, 20, 30, 40, 50].crossedMultiply([1, 2, 3]) = [10, 20, 30, 40, 50].withCrossedCollect([1, 2, 3], times:/2)
```

* * *

Unicode: ⊗ U+2297 Circled Times
