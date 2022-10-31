import ohm from 'https://unpkg.com/ohm-js@16/dist/ohm.esm.js';
import { extras } from 'https://unpkg.com/ohm-js@16/dist/ohm.esm.js';

import { stc } from './stc-common.js';

stc.grammar = ohm.grammar(String.raw`
Stc {

    TopLevel
      = ClassExpression+
      | InitializerDefinition

    InitializerDefinition
      = Temporaries* ExpressionSequence

    ExpressionSequence
      = ListOf<Expression, ";">

    ParameterList
      =  "(" NonemptyListOf<Expression, ","> ")"

    Temporaries
      = "var" NonemptyListOf<Temporary, ","> ";"

    TemporaryWithInitializer
      = identifier "=" Expression

    Temporary
      = TemporaryWithInitializer
      | identifier

    NonFinalExpression
      = Expression ";" Statements

    FinalExpression
      = Expression ";"?

    ReturnStatement
      = "^" Expression ";"?

    Statements
      = NonFinalExpression
      | FinalExpression
      | ReturnStatement

    Block
      = "{" BlockBody "}"

    BlockBody
      = BlockArguments? Temporaries* Primitive? Statements?

    BlockArguments
      = "arg" NonemptyListOf<identifier, ","> ";"

    Primitive
      = "<primitive: " primitiveCharacter* ">"

    ArrayExpression
      = "[" ListOf<Expression, ","> "]"

    ImplicitMessage
      = identifier "(" ListOf<Expression, ","> ")"

    ParenthesisedExpression
      = "(" Expression ")"

    Assignment
      = identifier "=" Expression

    Primary
      = DotExpression
      | Block
      | ImplicitMessage
      | reservedIdentifier
      | identifier
      | literal
      | ParenthesisedExpression
      | ArrayExpression

    BinaryExpression
      = Expression (binaryOperator Primary)+

    DotExpression
      = Primary ("." identifier ParameterList?)+

    Expression
      = Assignment
      | BinaryExpression
      | Primary

    ClassExpression
      = ClassExtension
      | ClassDefinition

    ClassDefinition
      = identifier "{" Temporaries? (methodName Block)* "}"

    ClassExtension
      = "+" identifier "{" (methodName Block)* "}"

    methodName
      = identifier
      | binaryOperator

    literal
      = numberLiteral
      | stringLiteral
      | symbolLiteral

    binaryChar
      = "!" | "%" | "&" | "*" | "+" | "/" | "<" | "=" | ">" | "?" | "@" | "~" | "|" | "-"

    binaryOperator
      = binaryChar+

    numberLiteral
      = floatLiteral
      | integerLiteral

    integerLiteral
      = "-"? digit+

    floatLiteral
      = "-"? digit+ "." digit+

    letterOrDigitOrUnderscore
      = letter
      | digit
      | "_"

    identifier
      = letter letterOrDigitOrUnderscore*

    reservedIdentifier
      = "nil" | "true" | "false" | "this"

    doubleQuoteChar
      = "\""

    stringLiteral
      = doubleQuoteChar stringCharacter* doubleQuoteChar

    stringCharacter
      = ~(doubleQuoteChar | lineTerminator) sourceCharacter

    singleQuoteChar
      = "\'"

    symbolLiteral
       = singleQuoteChar symbolCharacter* singleQuoteChar

    symbolCharacter
      = ~(singleQuoteChar | lineTerminator) sourceCharacter

    primitiveCharacter
      = ~(">" | lineTerminator) sourceCharacter

    sourceCharacter
      = any

    lineTerminator
      = "\n"
      | "\r"
}
`);

stc.semantics = stc.grammar.createSemantics();

stc.match = function(str) { return stc.grammar.match(str); }
stc.parse = function(str) { return stc.semantics(stc.grammar.match(str)); }
stc.parseAst = function(str) { return extras.toAST(stc.grammar.match(str)); }
stc.temporariesNames = function(str) { return extras.toAST(stc.grammar.match(str))[0][0].map(item => `'${item}'`) };

stc.blockArity = function(str) {
	const arg = extras.toAST(stc.grammar.match(str))[1][0][0];
	return arg === null ? 0 : arg.length;
};


// stc.temporariesNames('var i, j;')
// stc.blockArity('{ arg i, j; i + 1 * j }')
