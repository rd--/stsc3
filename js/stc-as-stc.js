import { stc } from './stc-common.js'
import { } from './stc-grammar.js'

function stc_comma_list(node) {
    return stc.as_iter_map(node, e => e.asStc).join(', ');
}

stc.semantics.addAttribute('asStc', {
    InitializerDefinition(tmp, stm) { return tmp.asStc + stm.asStc; },
    Expression(e) { return e.asStc; },
    Primary(e) { return e.asStc; },
    ParenthesisedExpression(_l, e, _r) { return '(' + e.asStc + ')'; },
    Assignment(lhs, _, rhs) { return lhs.asStc + ' = ' + rhs.asStc; },
    ParameterList(_l, sq, _r) { return '(' + stc_comma_list(sq) + ')'; },
    BinaryExpression(lhs, ops, rest) { return makeBinary(lhs.asStc, ops.children.map(c => c.asStc), rest.children.map(c => c.asStc)); },
    DotExpression(lhs, _dots, nms, args) { return makeDot(lhs.asStc, nms.children.map(c => c.asStc), args.children.map(c => c.asStc)); },
    Temporaries(_l, tmp, _r) { return 'var ' + stc_comma_list(tmp) + '; '; },
    TemporaryWithInitializer(nm, _, e) { return nm.asStc + ' = ' + e.asStc; },
    Temporary(tmp) { return tmp.asStc; },
    NonFinalExpression(e, _, stm) { return e.asStc + '; ' + stm.asStc; },
    FinalExpression(e, _) { return e.asStc; },
    ReturnStatement(_l, e, _r) { return '^ ' + e.asStc; },
    Statements(stm) { return stm.asStc; },
    ExpressionSequence(exp) { return exp.asStc; },
    NonemptyListOf(first, _sep, rest) { return first.asStc + rest.children.map(c => c.asStc); },
    Block(_l, blk, _r) { return blk.asStc; },
    BlockBody(arg, tmp, prm, stm) { return '{ ' + arg.asStc + tmp.asStc + prm.asStc + stm.asStc + ' }'; },
    BlockArguments(_l, arg, _r) { return 'arg ' + stc_comma_list(arg) + '; '; },
    Primitive(_l, _s, _r) { return this.sourceString; },
    ArrayExpression(_l, array, _r) { return '[' + stc_comma_list(array) + ']'; },
    ImplicitMessage(rcv, _l, arg, _r) { return rcv.asStc + '(' + stc_comma_list(arg) + ')'; },
    literal(lit) { return lit.asStc; },
    stringLiteral(_l, _s, _r) { return this.sourceString; },
    symbolLiteral(_l, _s, _r) { return this.sourceString; },
    numberLiteral(n) { return n.asStc; },
    integerLiteral(s,i) { return s.sourceString + i.sourceString; },
    floatLiteral(s,i,_,f) { return s.sourceString + i.sourceString + '.' + f.sourceString; },
    identifier(_l, _r) { return this.sourceString; },
    reservedIdentifier(id) { return id.sourceString; },
    _iter(...children) { return children.map(c => c.asStc).join(''); },
    _terminal() { return this.sourceString; }
});

function makeBinary(left, ops, rights) {
	while (ops.length > 0) {
		const op = ops.shift();
		const right = rights.shift();
		left = `(${left} ${op} ${right})`;
	}
	return left;
}

function makeDot(rcv, nms, args) {
	while (nms.length > 0) {
		const nm = nms.shift();
		const arg = args.shift();
		rcv = `${rcv}.${nm}${arg}`;
	}
	return rcv;
}
