# repeatEvery -- scheduling

- _repeatEvery(aClock, aProcedure, aDelay)_

Schedule _aProcedure_ at intervals given by _aDelay_ on _aClock_.
If _aProcedure_ accepts an argument it will be the delay interval before the procedure will execute next.

Print a message every few seconds indefinitely:

```
workspace::clock.repeatEvery({ :interval |
	['About to delay for', interval].postLine
}, {
	(1 .. 5).atRandom
})
```

Clear clock to end:

```
workspace::clock.clear
```
