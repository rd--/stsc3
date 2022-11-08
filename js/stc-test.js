import * as stc from './stc-grammar.js'
import { } from './stc-as-stc.js'

export const testExpr = [
	'5', '2.3',
	'"string"', "'symbol'",
	'true', 'nil',
	'p', 'sqrt', 'SinOsc',
	'(5)', '("x")','(1 + 2)',
	'[]', '[1]', '[1, 2.3, four]',
	'p[q]', 'p.q[r]', 'p[q] == r[s]',
	'p[q] = r', 'p.q[r] = s',
	'()', '(x: 1)', '(x: 23, y: 3.141)',
	'p:q', 'p.q:r',
	'p:q == r', 'p:q == r:s',
	'p:q = r', 'p.q:r = s',
	'i + j', 'i * j + k', 'i + j * k / 2',
	'i == j', 'i <= j', 'i == j && k',
	'p = q', 'p = q = r', 'p = q = r = 1',
	'p.q', 'p.q.r', 'p.q(r)', 'p.q(r, s)', 'p.q(r).s(t)', 'p.q(r.s(t))', 'p.q(r, s)',
	'(i + j).m', 'i + j.m', 'i.j + m', 'i.j + k.l',
	'f(p) + q', 'p + f(q)',
	'3.sqrt', '9.sqrt.squared',
	'PinkNoise()', 'SinOsc(440, 0)', 'Point(x, y).isPoint',
	'if(p) { q } { r }', 'while { p } { q }',
	'Float.e',
	'{ Rand(0, 1) }', '{ var x, y; x = Rand(0, 1); y = [x, x]; y }',
	'{  }', '{ :x | x * x }', '{ :x | | y z | z = y = x; z }', '{ | x = 1; | nil }', '{ :x | | y = x, z = 1; | y + z }',
	'{ arg x; x * x }', '{ arg x, y; x + y * x }', '{ arg x, y; (x.squared + y.squared).sqrt }',
	'{ arg p; p }', '{ arg p; var q; nil }', '{ arg p, q; var r, s; nil }', '{ arg p, q; var r = p, s = q; nil }',
	'{ :p | p }', '{ | p | nil }', '{ :p | | q | nil }', '{ :p :q | | r s | nil }', '{ | p q | nil }', '{ :p | | q = p; | nil }',
	'{ arg x; var y = x * x; var z = y * y * y; z }.value(3)',
	'{ <primitive: 63> }',
	'var x; x = 9; x.sqrt.postln', 'var x = 9; x.sqrt.postln', 'var x = 3, y = 4; x + y',
	'C { }', 'C { m { } }', 'C { + { } }', '+ C { x { } }',
	'C { var i, j; p { arg x; ^ i + x * j } q { arg y, z; y - z } }',
	'Void { random { <primitive: 0> } }',
];

export function testAsStc() {
	testExpr.forEach(function(str) {
		console.log(str, '=>');
		const rw = stc.parse(str).asStc;
		console.log(rw, '==', str == rw)
	});
};
