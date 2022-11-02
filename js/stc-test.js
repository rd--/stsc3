import { stc } from './stc-common.js'
import { } from './stc-as-stc.js'

stc.test = {};

stc.test.expr = [
	'5', '2.3',
	'"string"', "'symbol'",
	'true', 'nil',
	'p', 'sqrt', 'SinOsc',
	'(5)', '("x")','(1 + 2)',
	'[]', '[1]', '[1, 2.3, four]',
	'i + j', 'i * j + k', 'i + j * k / 2',
	'i == j', 'i <= j', 'i == j && k',
	'p = q', 'p = q = r', 'p = q = r = 1',
	'p.q', 'p.q.r', 'p.q(r)', 'p.q(r, s)', 'p.q(r).s(t)', 'p.q(r.s(t))', 'p.q(r, s)',
	'(i + j).m', 'i + j.m', 'i.j + m', 'i.j + k.l',
	'f(p) + q', 'p + f(q)',
	'3.sqrt', '9.sqrt.squared',
	'SinOsc(440, 0)', 'Point(x, y).isPoint', 'Float.e',
	'{ Rand(0, 1) }', '{ var x, y; x = Rand(0, 1); y = [x, x]; y }',
	'{ arg x; x * x }', '{ arg x, y; x + y * x }', '{ arg x, y; (x.squared + y.squared).sqrt }',
	'{ arg x; x * x}.value(3)',
	'{ <primitive: 63> }',
	'var x; x = 9; x.sqrt.postln', 'var x = 9; x.sqrt.postln',
	'C { }', 'C { m { } }', 'C { + { } }', '+ C { x { } }',
	'C { var i, j; p { arg x; ^ i + x * j } q { arg y, z; y - z } }',
	'Void { random { <primitive: 0> } }',
];

stc.test.asStc = function() {
	stc.test.expr.forEach(str => console.log(str, '=>', stc.parse(str).asStc));
};

export { stc } from './stc-common.js'
