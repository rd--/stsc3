'sc/lang/Clock'
Clock().isClock
Clock().priorityQueue.isEmpty
var c = Clock(); c.schedule(3, { 3.postLine; nil }); c.schedule(2, { 2.postLine; c.clear; nil }); c.schedule(1, { 1.postLine; nil })
var c = Clock(); var k = 1; c.schedule(0, { k.postLine; k := k + 1; 1 }); c.schedule(9, { c.clear; nil })
system::clock.clear
system::clock.schedule(0) { var x = 1.randomFloat; x.postLine; if(x > 0.1) { x } { 'end'.postLine; nil } }
system::clock.scheduleInjecting(0, 1) { :i | var x = 1.randomFloat; [i, x].postLine; if(x > 0.1) { [x, i + 1] } { nil } }
var f = { var x = 1.randomFloat; x.postLine; if(x > 0.1) { x } { nil } }; { 'x'.postLine }.repeatEvery(system::clock, f)
var f = { var x = 1.randomFloat; if(x > 0.1) { x } { nil } }; { :x | x.postLine }.repeatEvery(system::clock, f)
var f = { var x = 1.randomFloat; if(x > 0.1) { x } { 't'.postLine; nil } }; { :x | x.postLine; if(x < 7) { x + 1 } { 'r'.postLine; nil } }.recurseEvery(system::clock, 1, f)

'sc/lang/Number.ext'
{ 0.5.coin }.dup(9).allSatisfy(isBoolean:/1)
var c = if(0.5.coin) { 't' } { 'f' }; c = 't' | { c = 'f' }
(0 .. 14).collect { :each | each.degreeToKey([0, 2, 4, 5, 7, 9, 11], 12) } = [0,  2,  4,  5,  7,  9, 11, 12, 14, 16, 17, 19, 21, 23, 24]
(-3 .. 13).collect { :each | each.foldOnce(1, 9) } = [5, 4, 3, 2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5]
(-3 .. 13).collect { :each | each.fold(1, 9) } = [5, 4, 3, 2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5]
(-3 .. 13).collect { :each | each.foldOnce(4, 7) } = [11, 10, 9, 8, 7, 6, 5, 4, 5, 6, 7, 6, 5, 4, 3, 2, 1]
(-3 .. 13).collect { :each | each.fold(4, 7) } = [5, 4, 5, 6, 7, 6, 5, 4, 5, 6, 7, 6, 5, 4, 5, 6, 7]

'sc/lang/Object.ext'
dup('3', 3) = ['3', '3', '3']
'3' ! 3 = ['3', '3', '3']
var m = { randomFloat() }.dup(9).mean; m > 0 & { m < 1 }
({ randomFloat() } ! 9).size = 9
({ randomFloat() } ! 3).allSatisfy(isNumber:/1) = true

'sc/lang/SequenceableCollection.ext'
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

'sc/Ugen'
SinOsc(440, 0).isUgen = true
encodeUgen('g', SinOsc(440, 0) * 0.1).isByteArray = true
23.isOutputSignal = false

'sc/buffer'
SfAcquire('piano-c5', 2, [1, 2])