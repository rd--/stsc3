import * as stc from './stc-grammar.js'

const desugar = false;

stc.semantics.addAttribute('asStc', {
    TopLevel(e) { return e.asStc; },
    InitializerDefinition(tmp, stm) { return tmp.asStc + stm.asStc; },
    Expression(e) { return e.asStc; },
    Primary(e) { return e.asStc; },
    ParenthesisedExpression(_l, e, _r) { return '(' + e.asStc + ')'; },
    Assignment(lhs, _, rhs) { return lhs.asStc + ' = ' + rhs.asStc; },
    NonEmptyParameterList(_l, sq, _r) { return '(' + commaList(sq) + ')'; },
    BinaryExpression(lhs, ops, rest) { return makeBinaryExpression(lhs.asStc, ops.children.map(c => c.asStc), rest.children.map(c => c.asStc)); },
    DotExpression(lhs, _dots, nms, args) { return makeDotExpression(lhs.asStc, nms.children.map(c => c.asStc), args.children.map(c => c.asStc)); },
    Temporaries(tmp) { return tmp.asStc; },
    TemporariesKeyword(_l, tmp, _r) { return 'var ' + commaList(tmp) + '; '; },
    TemporariesSyntax(_l, tmp, _r) { return '| ' + tmp.children.map(c => c.asStc).join(' ') + ' | '; },
    TemporariesWithInitializerSyntax(_l, tmp, _s, _r) { return '| ' + commaList(tmp) + '; | '; },
    TemporaryWithIdentifierInitializer(nm, _e, exp) { return nm.asStc + ' = ' + exp.asStc; },
    TemporaryWithPatternInitializer(_l, nms, _r, _e, rhs) { return "not implemented"; },
    Temporary(tmp) { return tmp.asStc; },
    NonFinalExpression(e, _, stm) { return e.asStc + '; ' + stm.asStc; },
    FinalExpression(e, _) { return e.asStc; },
    ReturnStatement(_l, e, _r) { return '^ ' + e.asStc; },
    Statements(stm) { return stm.asStc; },
    ExpressionSequence(exp) { return exp.asStc; },
    NonemptyListOf(first, _sep, rest) { return first.asStc + rest.children.map(c => c.asStc); },
    Block(_l, blk, _r) { return blk.asStc; },
    BlockBody(arg, tmp, prm, stm) { return '{ ' + arg.asStc + tmp.asStc + prm.asStc + stm.asStc + ' }'; },
    BlockArguments(arg) { return arg.asStc; },
    BlockArgumentsKeyword(_l, arg, _r) { return 'arg ' + commaList(arg) + '; '; },
    ArgumentName(_c, nm) { return ':' + nm.asStc; },
    BlockArgumentsSyntax(arg, _r) { return arg.children.map(c => c.asStc).join(' ') + ' | '; },
    Primitive(_l, _s, _r) { return this.sourceString; },
    ArrayExpression(_l, array, _r) { return '[' + commaList(array) + ']'; },
    AssociationExpression(lhs, _arrow, rhs) { return lhs.asStc + ': ' + rhs.asStc; },
    DictionaryExpression(_l, dict, _r) { return '(' + commaList(dict) + ')'; },
	AtSyntax(col, _l, key, _r) { return desugar ? `${col.asStc}.at(${key.asStc})` : `${col.asStc}[${key.asStc}]`; },
	PutSyntax(col, _l, key, _r, _e, val) { return desugar ? `${col.asStc}.put(${key.asStc}, ${val.asStc})` : `${col.asStc}[${key.asStc}] = ${val.asStc}`; },
	AtQuotedSyntax(col, _c, key) { return desugar ? `${col.asStc}.at('${key.asStc}')` : `${col.asStc}:${key.asStc}`; },
	PutQuotedSyntax(col, _c, key, _e, val) { return desugar ? `${col.asStc}.put('${key.asStc}', ${val.asStc})` : `${col.asStc}:${key.asStc} = ${val.asStc}`; },
    ParameterList(_l, sq, _r) { return '(' + commaList(sq) + ')'; },
    ImplicitMessage(rcv, arg) { return rcv.asStc + arg.asStc; },
    ImplicitMessageWithTrailingClosures(rcv, arg, tc) { return rcv.asStc + arg.asStc + tc.children.map(c => c.asStc).join(' '); },
    ClassDefinition(clsNm, _l, tmp, mthNm, mthBlk, _r) { return makeClassDefinition(clsNm.asStc, tmp.asStc, mthNm.children.map(c => c.asStc), mthBlk.children.map(c => c.asStc)); },
    ClassExtension(_e, clsNm, _l, mthNm, mthBlk, _r) { return makeClassExtension(clsNm.asStc, mthNm.children.map(c => c.asStc), mthBlk.children.map(c => c.asStc)); },
	methodName(name) { return name.asStc },
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

function commaList(node) {
    return node.asIteration().children.map(e => e.asStc).join(', ');
}

function makeBinaryExpression(left, ops, rights) {
	while (ops.length > 0) {
		const op = ops.shift();
		const right = rights.shift();
		left = `${left} ${op} ${right}`;
	}
	return left;
}

function makeDotExpression(rcv, nms, args) {
	while (nms.length > 0) {
		const nm = nms.shift();
		const arg = args.shift();
		rcv = `${rcv}.${nm}${arg}`;
	}
	return rcv;
}

function makeMethodList(mthNms, mthBlks) {
	let mth = '';
	while (mthNms.length > 0) {
		const nm = mthNms.shift();
		const blk = mthBlks.shift();
		mth += ` ${nm} ${blk}`;
	}
	return mth;
}

function makeClassDefinition(clsNm, tmp, mthNms, mthBlks) {
	return `${clsNm} { ${tmp}${makeMethodList(mthNms, mthBlks)} }`;
}

function makeClassExtension(clsNm, mthNms, mthBlks) {
	return `+ ${clsNm} { ${makeMethodList(mthNms, mthBlks)} }`;
}
