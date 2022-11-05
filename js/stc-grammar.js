import ohm from 'https://unpkg.com/ohm-js@16/dist/ohm.esm.js';
import { extras } from 'https://unpkg.com/ohm-js@16/dist/ohm.esm.js';

export const grammar = ohm.grammar(String.raw`
Stc {

    TopLevel
      = ClassExpression+
      | InitializerDefinition

    InitializerDefinition
      = Temporaries* ExpressionSequence

    ExpressionSequence
      = ListOf<Expression, ";">

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

    ParameterList
      =  "(" ListOf<Expression, ","> ")"

    ImplicitMessage
      = identifier ParameterList

    ImplicitMessageWithTrailingClosures
      = identifier NonEmptyParameterList? Block+

    ParenthesisedExpression
      = "(" Expression ")"

    Assignment
      = identifier "=" Expression

    Primary
      = DotExpression
      | Block
      | ImplicitMessageWithTrailingClosures
      | ImplicitMessage
      | reservedIdentifier
      | identifier
      | literal
      | ParenthesisedExpression
      | ArrayExpression

    BinaryExpression
      = Expression (binaryOperator Primary)+

    NonEmptyParameterList
      =  "(" NonemptyListOf<Expression, ","> ")"

    DotExpression
      = Primary ("." identifier NonEmptyParameterList?)+

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

export const semantics = grammar.createSemantics();

export function parse(str) {
	return semantics(grammar.match(str));
}

export function parseAst(str) {
	return extras.toAST(grammar.match(str));
}

export function temporariesNames(str) {
	return extras.toAST(grammar.match(str))[0][0];
}

export function blockArity(str) {
	const arg = extras.toAST(grammar.match(str))[1][0][0];
	return arg === null ? 0 : arg.length;
}

/*
import * as stc from './stc-grammar.js'
stc.temporariesNames('var i, j;') //= ['i', 'j']
stc.blockArity('{ arg i, j; i + 1 * j }') === 2
*/
