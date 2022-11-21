import * as stc from './stc-grammar.js'

stc.semantics.addAttribute('asStc', {
    Program(tmp, stm) { return tmp.asStc + stm.asStc; },
    TemporaryWithIdentifierInitializer(nm, _, exp) { return nm.asStc + ' = ' + exp.asStc; },
    TemporariesVarSyntax(_l, tmp, _r) { return 'var ' + commaList(tmp.asIteration().children) + '; '; },
    Assignment(lhs, _, rhs) { return lhs.asStc + ' = ' + rhs.asStc; },
    BinaryExpression(lhs, ops, rhs) {
		let left = lhs.asStc;
		const opsArray = ops.children.map(c => c.asStc);
		const rhsArray = rhs.children.map(c => c.asStc);
		while (opsArray.length > 0) {
			const op = opsArray.shift();
			const right = rhsArray.shift();
			left = `${left} ${op} ${right}`;
		}
		return left;
	},

	PutSyntax(c, _l, k, _r, _e, v) { return `${c.asStc}.put(${k.asStc}, ${v.asStc})`; },
	AtSyntax(c, _l, k, _r) { return `${c.asStc}.at(${k.asStc})`; },
	ValueSyntax(p, _d, a) { return `${p.asStc}.value(${a.asStc})`; },
    NonEmptyParameterList(_l, sq, _r) { return commaList(sq.asIteration().children); },

    DotExpressionWithTrailingClosuresSyntax(lhs, _dot, nm, args, tc) {
		return `${lhs.asStc}.${nm.asStc}(${args.children.map(c => c.asStc).concat(tc.children.map(c => c.asStc))})`;
	},
    DotExpressionWithAssignmentSyntax(lhs, _dot, nm, _asg, rhs) { return `${lhs.asStc}.${nm.asStc}_(${rhs.asStc})`; },
    DotExpression(lhs, _dot, nms, args) {
		let rcv = lhs.asStc;
		const namesArray = nms.children.map(c => c.asStc);
		const argsArray = args.children.map(c => c.asStc);
		while (namesArray.length > 0) {
			const nm = namesArray.shift();
			const arg = argsArray.shift();
			if(arg.length === 0) {
				rcv = `${rcv}.${nm}`;
			} else {
				rcv = `${rcv}.${nm}(${arg})`;
			}
		}
		return rcv;
	},

    Block(_l, blk, _r) { return blk.asStc; },
    BlockBody(arg, tmp, prm, stm) {
		return `{ arg ${arg.asStc}; ${tmp.asStc} ${prm.asStc} ${stm.asStc} }`;
	},
    ArgumentList(arg, _r) { return commaList(arg.children); },
    ArgumentName(_c, nm) { return nm.asStc; },
    Primitive(_l, _s, _r) { return ''; },
    NonFinalExpression(e, _, stm) { return e.asStc + '; ' + stm.asStc; },
    FinalExpression(e, _) { return e.asStc + ';'; },

});

function commaList(node) {
    return node.asIteration().children.map(e => e.asStc).join(', ');
}
