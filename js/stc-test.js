import { stc } from './stc-grammar.js'
import { } from './stc-as-stc.js'

stc.test = {};

stc.test.expr = [
	'5', '2.3',
	'"string"', "'symbol'",
	'<primitive: 63>',
	'true', 'nil',
	'p', 'sqrt', 'SinOsc',
	'(5)', '("x")','(1 + 2)',
	'[]', '[1]', '[1, 2.3, four]',
	'i + j', 'i * j + k', 'i + j * k / 2',
	'p.q', 'p.q.r', 'p.q(r)', 'p.q(r).s(t)', 'p.q(r.s(t))',
	'SinOsc(440, 0)', 'Float.e',
	'{ Rand(0, 1) }', '{ var x, y; x = Rand(0, 1); y = [x, x]; y }',
	'{ arg x; x * x }', '{ arg x, y; (x.squared + y.squared).sqrt }',
	'var x; x = 9; x.sqrt.postln', 'var x = 9; x.sqrt.postln'
];

stc.test.asStc = function() {
	stc.test.expr.forEach(str => console.log(str, '=>', stc.parse(str).asStc));
};

export { stc } from './stc-grammar.js'
