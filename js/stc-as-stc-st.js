// obsolete

import { stc } from './stc-common.js'
import { } from './stc-grammar-st.js'

function stc_group_bin(lhs, seq) {
    if(seq.length == 1) {
        return lhs + seq[0].asStc;
    } else {
        return stc_group_bin('(' + lhs + seq[0].asStc + ')', seq.slice(1))
    }
}

function stc_comma_list(node) {
    return stc.as_iter_map(node, e => e.asStc).join(', ');
}

stc.semanticsSt.addAttribute('asStc', {
    InitializerDefinition(tmp, stm) { return tmp.asStc + stm.asStc; },
    Expression(e) { return e.asStc; },
    ParenthesisedExpression(_l, e, _r) { return '(' + e.asStc + ')'; },
    Assignment(lhs, _, rhs) { return lhs.asStc + ' = ' + rhs.asStc; },
    BinaryMessage(sel, arg) { return ' ' + sel.asStc + ' ' + arg.asStc; },
    BinaryArgument(prm, dotIter) { return prm.asStc + dotIter.children.map(c => c.asStc); },
    DotMessage(_, sel, arg) { return '.' + sel.asStc + arg.asStc; },
    MessageParameters(_l, sq, _r) { return '(' + sq.asIteration().asStc + ')'; },
    BasicExpression(e) { return e.asStc; },
    BasicDotExpression(prm, dotIter, binIter) { return prm.asStc + dotIter.asStc + binIter.asStc; },
    BasicBinaryExpression(prm, binIter) { return stc_group_bin(prm.asStc, binIter.children); },
    Temporaries(_l, tmp, _r) { return 'var ' + stc_comma_list(tmp) + '; '; },
    TemporaryWithInitializer(nm, _, e) { return nm.asStc + ' = ' + e.asStc; },
    Temporary(tmp) { return tmp.asStc; },
    NonFinalExpression(e, _, stm) { return e.asStc + '; ' + stm.asStc; },
    FinalExpression(e, _) { return e.asStc; },
    ReturnStatement(_l, e, _r) { return '^ ' + e.asStc; },
    Statements(stm) { return stm.asStc; },
    Block(_l, blk, _r) { return blk.asStc; },
    BlockBody(arg, tmp, stm) { return '{ ' + arg.asStc + tmp.asStc + stm.asStc + ' }'; },
    BlockArguments(_l, arg, _r) { return 'arg ' + stc_comma_list(arg) + '; '; },
    ArrayExpression(_l, array, _r) { return '[' + stc_comma_list(array) + ']'; },
    ImplicitMessage(rcv, _l, arg, _r) { return rcv.asStc + '(' + stc_comma_list(arg) + ')'; },
    Primary(prm) { return prm.asStc; },
    literal(lit) { return lit.asStc; },
    stringLiteral(_l, _s, _r) { return this.sourceString; },
    symbolLiteral(_l, _s, _r) { return this.sourceString; },
    primitive(_l, _s, _r) { return this.sourceString; },
    numberLiteral(n) { return n.asStc; },
    integerLiteral(s,i) { return s.sourceString + i.sourceString; },
    floatLiteral(s,i,_,f) { return s.sourceString + i.sourceString + '.' + f.sourceString; },
    identifier(_l, _r) { return this.sourceString; },
    reservedIdentifier(id) { return id.sourceString; },
    _iter(...children) { return children.map(c => c.asStc).join(''); },
    _terminal() { return this.sourceString; }
});
