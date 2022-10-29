import ohm from 'https://unpkg.com/ohm-js@16/dist/ohm.esm.js';

import { stc } from './stc-common.js';

stc.grammar = ohm.grammar(String.raw`
Stc {

    InitializerDefinition
      = Temporaries* Statements?

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

    ArrayExpression
      = "[" ListOf<Expression, ","> "]"

    ImplicitMessage
      = identifier "(" ListOf<Expression, ","> ")"

    ParenthesisedExpression
      = "(" Expression ")"

    Assignment
      = identifier "=" Expression

    Primary
      = literal
      | Block
      | ImplicitMessage
      | identifier
      | reservedIdentifier
      | ParenthesisedExpression
      | ArrayExpression
      | Assignment

    BinaryExpression
      = Expression (binaryOperator Expression)+

    DotExpression
      = Expression ("." identifier ParameterList?)+

    Expression
      = BinaryExpression
      | DotExpression
      | Primary

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

    letterOrDigit
      = letter
      | digit

    identifier
      = letter letterOrDigit*

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

    Primitive
      = "<primitive: " primitiveCharacter* ">"

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

stc.parse = function(str) {
	return stc.semantics(stc.grammar.match(str));
}
