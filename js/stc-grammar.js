import ohm from 'https://unpkg.com/ohm-js@16/dist/ohm.esm.js';
import { extras } from 'https://unpkg.com/ohm-js@16/dist/ohm.esm.js';

export const stcGrammar = ohm.grammar(String.raw`
Stc {

    TopLevel = LibraryExpression+ | Program
    LibraryExpression = ClassExpression | TraitExpression
    ClassExpression = ClassExtension | ClassDefinition
    ClassExtension = "+" identifier "{" (methodName Block)* "}"
    ClassDefinition = identifier TraitList? "{" Temporaries? (methodName Block)* "}"
    TraitList = ":" "[" NonemptyListOf<identifier, ","> "]"
    TraitExpression = TraitExtension | TraitDefinition
    TraitExtension = "+" "@" identifier "{" (methodName Block)* "}"
    TraitDefinition = "@" identifier "{" (methodName Block)* "}"
    Program = Temporaries? ExpressionSequence
    Temporaries = TemporariesKeyword+ | TemporariesWithInitializerSyntax | TemporariesSyntax
    TemporariesKeyword = "var" NonemptyListOf<Temporary, ","> ";"
    Temporary = TemporaryWithInitializer | identifier
    TemporaryWithInitializer = TemporaryWithIdentifierInitializer | TemporaryWithDictionaryInitializer | TemporaryWithArrayInitializer
    TemporaryWithIdentifierInitializer = identifier "=" Expression
    TemporaryWithDictionaryInitializer = "("  NonemptyListOf<identifier, ","> ")" "=" identifier
    TemporaryWithArrayInitializer = "["  NonemptyListOf<identifier, ","> "]" "=" identifier
    TemporariesWithInitializerSyntax = "|" NonemptyListOf<TemporaryWithInitializer, ","> ";" "|"
    TemporariesSyntax = "|" identifier+ "|"
    ExpressionSequence = ListOf<Expression, ";">
    Expression = Assignment | BinaryExpression | Primary
    Assignment = identifier ":=" Expression
    BinaryExpression = Expression (binaryOperator Primary)+

    Primary
      = PutSyntax
      | PutQuotedSyntax
      | AtSyntax
      | AtQuotedSyntax
      | DotExpressionWithTrailingClosures
      | DotExpression
      | Block
      | ApplyWithTrailingClosures
      | Apply
      | reservedIdentifier
      | identifier
      | literal
      | ParenthesisedExpression
      | DictionaryExpression
      | ArrayExpression
      | ArrayRangeSyntax
      | IntervalSyntax

    PutSyntax = Primary "[" Expression "]" ":=" Expression
    PutQuotedSyntax = Primary ":" identifier ":=" Expression
    AtSyntax = Primary "[" Expression "]"
    AtQuotedSyntax = Primary ":" identifier

    DotExpressionWithTrailingClosures = Primary "." identifier NonEmptyParameterList? Block+
    NonEmptyParameterList =  "(" NonemptyListOf<Expression, ","> ")"
    DotExpression = Primary ("." identifier ~"{" NonEmptyParameterList?)+

    Block = "{" BlockBody "}"
    BlockBody = BlockArguments? Temporaries? Primitive? Statements?
    BlockArguments = BlockArgumentsSyntax | BlockArgumentsKeyword
    BlockArgumentsSyntax = ArgumentName+ "|"
    ArgumentName = ":" identifier
    BlockArgumentsKeyword = "arg" NonemptyListOf<identifier, ","> ";"
    Primitive = "<primitive:" primitiveCharacter* ">"
    Statements = NonFinalExpression | FinalExpression | ReturnStatement
    NonFinalExpression = Expression ";" Statements
    FinalExpression = Expression ";"?
    ReturnStatement = "^" Expression ";"?

    ApplyWithTrailingClosures = identifier NonEmptyParameterList? Block+
    Apply = identifier ParameterList
    ParameterList =  "(" ListOf<Expression, ","> ")"
    ParenthesisedExpression = "(" Expression ")"
    DictionaryExpression = "(" ListOf<AssociationExpression, ","> ")"
    AssociationExpression = identifier ":" Expression
    ArrayExpression = "[" ListOf<Expression, ","> "]"
    ArrayRangeSyntax = "[" Expression ".." Expression "]"
    IntervalSyntax = "(" Expression ".." Expression ")"

    methodName = identifier | binaryOperator
    identifier = letter letterOrDigitOrUnderscore*
    letterOrDigitOrUnderscore = letter | digit | "_"
    reservedIdentifier = "nil" | "true" | "false"
    binaryOperator = binaryChar+
    binaryChar = "!" | "%" | "&" | "*" | "+" | "/" | "<" | "=" | ">" | "?" | "@" | "~" | "|" | "-"

    literal = numberLiteral | stringLiteral | symbolLiteral
    numberLiteral = floatLiteral | integerLiteral
    floatLiteral = "-"? digit+ "." digit+
    integerLiteral = "-"? digit+
    stringLiteral = doubleQuoteChar stringCharacter* doubleQuoteChar
    doubleQuoteChar = "\""
    stringCharacter = ~(doubleQuoteChar | lineTerminator) sourceCharacter
    lineTerminator = "\n" | "\r"
    sourceCharacter = any
    symbolLiteral = singleQuoteChar symbolCharacter* singleQuoteChar
    singleQuoteChar = "\'"
    symbolCharacter = ~(singleQuoteChar | lineTerminator) sourceCharacter

    primitiveCharacter = ~(">" | lineTerminator) sourceCharacter

    comment = multiLineComment | singleLineComment
    multiLineComment = "/*" (~"*/" sourceCharacter)* "*/"
    singleLineComment = "//" (~lineTerminator sourceCharacter)*
    space += comment

}
`);

export const stcSemantics = stcGrammar.createSemantics();

export function stcParse(str) {
	return stcSemantics(stcGrammar.match(str));
}

export function stcParseToAst(str) {
	return extras.toAST(stcGrammar.match(str));
}

export function stcTemporariesKeywordNames(str) {
	return stcParseToAst(str)[0][0];
}

export function stcTemporariesSyntaxNames(str) {
	return stcParseToAst(str)[0];
}

export function stcBlockArity(str) {
	const arg = stcParseToAst(str)[1][0][0];
	return arg === null ? 0 : arg.length;
}

/*
import * as stc from './stc-grammar.js'
stc.temporariesKeywordNames('var i, j;') //= ['i', 'j']
stc.temporariesSyntaxNames('| i j |') //= ['i', 'j']
stc.blockArity('{ arg i, j; i + 1 * j }') === 2
*/
