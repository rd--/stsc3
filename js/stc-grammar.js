import ohm from 'https://unpkg.com/ohm-js@16/dist/ohm.esm.js';
import { extras } from 'https://unpkg.com/ohm-js@16/dist/ohm.esm.js';

import { stc } from './stc-common.js';

stc.grammar = ohm.grammar(String.raw`
Stc {

    TopLevel
      = ClassDefinition
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
      = literal
      | Block
      | ImplicitMessage
      | DotExpression
      | identifier
      | reservedIdentifier
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

    ClassDefinition
      = identifier "{" Temporaries (identifier Block)* "}"

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
stc.temporariesNames = function(m) { return extras.toAST(m)[0][0] };
stc.blockArgumentNames = function(m) { return extras.toAST(m)[1][0][0] };

// stc.temporariesNames(stc.match('var i, j;'))
// stc.blockArgumentNames(stc.match('{ arg i, j; i + 1 * j }'))
