# Terse Guide to SuperCollider

## Clock -- temporal type
```
Clock().isClock
Clock().priorityQueue.isEmpty
var c = Clock(); c.schedule(3, { :t | [t, 3].postLine; nil }); c.schedule(2, { :t | [t, 2].postLine; c.clear; nil }); c.schedule(1, { :t | [t, 1].postLine; nil })
var c = Clock(); var k = 1; c.schedule(0, { :t | [t, k].postLine; k := k + 1; 1 }); c.schedule(9, { :t | c.clear; nil })
workspace::clock.clear
workspace::clock.schedule(0) { :t | var x = 1.randomFloat; [t, x].postLine; if(x > 0.1) { x } { 'end'.postLine; nil } }
workspace::clock.scheduleInjecting(0, 1) { :t :i | var x = 1.randomFloat; [t, i, x].postLine; if(x > 0.1) { [x, i + 1] } { nil } }
var f = { var x = 1.randomFloat; x.postLine; if(x > 0.1) { x } { nil } }; workspace::clock.repeatEvery({ :t :d | [t, d].postLine }, f)
var f = { var x = 1.randomFloat; if(x > 0.1) { x } { nil } }; workspace::clock.repeatEvery({ :t :d | [t, d].postLine }, f)
var f = { var x = 1.randomFloat; if(x > 0.1) { x } { 't'.postLine; nil } }; { :t :x | [t, x].postLine; if(x < 7) { x + 1 } { 'r'.postLine; nil } }.recurseEvery(1, f)
```

## Nil -- extensions
```
nil ? 'x' = 'x' (* operand if receiver is nil *)
nil ? 1 = 1 (* operand if receiver is nil *)
'x' ? 'y' = 'x' (* receiver unless nil *)
1 ? 2 = 1 (* receiver unless nil *)
nil ~? 1 = nil (* receiver if nil, else operand *)
1 ~? 2 = 2 (* receiver if nil, else operand *)
```

## Number -- extensions
```
{ 0.5.coin }.dup(9).allSatisfy(isBoolean:/1)
var c = if(0.5.coin) { 't' } { 'f' }; c = 't' | { c = 'f' }
(0 .. 14).collect { :each | each.degreeToKey([0, 2, 4, 5, 7, 9, 11], 12) } = [0,  2,  4,  5,  7,  9, 11, 12, 14, 16, 17, 19, 21, 23, 24]
(-3 .. 13).collect { :each | each.foldOnce(1, 9) } = [5, 4, 3, 2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5]
(-3 .. 13).collect { :each | each.fold(1, 9) } = [5, 4, 3, 2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5]
(-3 .. 13).collect { :each | each.foldOnce(4, 7) } = [11, 10, 9, 8, 7, 6, 5, 4, 5, 6, 7, 6, 5, 4, 3, 2, 1]
(-3 .. 13).collect { :each | each.fold(4, 7) } = [5, 4, 5, 6, 7, 6, 5, 4, 5, 6, 7, 6, 5, 4, 5, 6, 7]
```

## Object -- extensions
```
dup('3', 3) = ['3', '3', '3']
'3' ! 3 = ['3', '3', '3']
var m = { randomFloat() }.dup(9).mean; m > 0 & { m < 1 }
({ randomFloat() } ! 9).size = 9
({ randomFloat() } ! 3).allSatisfy(isNumber:/1) = true
```

## SequenceableCollection -- extensions
```
[1, 2, 3] * 4 = [4, 8, 12]
[-2 .. 2].max(0) = [0, 0, 0, 1, 2]
[-2 .. 2].max(0) = 0.max([-2 .. 2])
[-2 .. 2].min(0) = [-2, -1, 0, 0, 0]
0.min([-2 .. 2]) = [-2 .. 2].min(0)
[-3 .. 3].roundTo(2) = [-2, -2, -0, 0, 2,  2,  4]
[9, 25, 81].collect(sqrt:/1) * [2, 3, 4] = [ 6, 15, 36 ]
[1, 2, 3, 4].withExtendingCollect([5, 6, 7, 8, 9], times:/2) = [5, 12, 21, 32, 9]
[1, 2, 3, 4, 5] * [6, 7, 8, 9] = [6, 14, 24, 36, 30]
[1, 2, 3].cos.rounded = [1, -0, -1]
[1, 2, 3].cubed = [1, 8, 27]
[1 .. 5].stutter(2) = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5]
1.to(9).collect(squared:/1) * 2 = [2, 8, 18, 32, 50,72, 98, 128, 162]
1.to(9).collect(squared:/1) / 2 = [0.5, 2, 4.5, 8, 12.5, 18,24.5, 32, 40.5]
2 * 1.to(9).collect(squared:/1) / 2 = [1, 4, 9, 16, 25,36, 49, 64, 81]
2 * 1.to(9).collect(squared:/1) / 2 * 9 / 9 = [1, 4, 9, 16, 25,36, 49, 64, 81]
[1, 2].extendTo(5) = [1, 2, 1, 2, 1]
[[1, 2], [3, 4, 5]].extendToBeOfEqualSize = [[1, 2, 1], [3, 4, 5]]
[[1, 2], [3, 4, 5], 6].extendToBeOfEqualSize = [[1, 2, 1], [3, 4, 5], [6, 6, 6]]
[[1, 2], [3, 4, 5]].flop = [[1, 3], [2, 4], [1, 5]]
[0, [1, 2], [3, 4, 5]].flop = [[0, 1, 3], [0, 2, 4], [0, 1, 5]]
[[1, 2], [3, 4]].allTuples = [[1, 3], [1, 4], [2, 3], [2, 4]]
[[1, 2], [3, 4], [4, 5], [6, 7, 8, 9]].allTuples.size = 32
[10, 20, 30, 40, 50].withCrossedCollect([1, 2, 3], plus:/2) =  [11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43, 51, 52, 53]
var x = [4, 4.5], y = [2, 3, 5, 6], z = [x, y]; z.withTableCollect(z, times:/2) = [x * [x, y], y * [x, y]]
var x = [4, 4.5], y = [2, 3, 5, 6]; [x * [x, y], y * [x, y]] = [[[16, 18], [9, 13.5, 22.5, 27]], [[8, 9], [6, 9, 15, 18], [20, 22.5], [12, 18, 30, 36]]]
[10, 20, 30, 40, 50].crossMultiply([1, 2, 3]) = [10, 20, 30, 20, 40, 60, 30, 60, 90, 40, 80, 120, 50, 100, 150]
[10, 20, 30, 40, 50].crossMultiply([1, 2, 3]) = [10, 20, 30, 40, 50].withCrossedCollect([1, 2, 3], times:/2)
```

## String -- extensions
```
'x' ++? 'y' = 'xy' (* optional (non-nil) append *)
'x' ++? { 'y' } = 'xy' (* optional (non-nil) append *)
'x' ++? nil = 'x' (* optional (non-nil) append *)
```

## Ugen -- sound type
```
SinOsc(440, 0).isUgen = true
encodeUgen('g', SinOsc(440, 0) * 0.1).isByteArray = true
23.isOutputSignal = false
```

## SoundFile
```
SfAcquire('piano-c5', 2, [1, 2])
```
