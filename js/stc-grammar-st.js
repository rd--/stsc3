import ohm from 'https://unpkg.com/ohm-js@16/dist/ohm.esm.js';

import { stc } from './stc-common.js';

stc.grammarSt = ohm.grammar(String.raw`
Stc {

    InitializerDefinition (an initializer definition)
      = Temporaries* Statements?

    Expression (an expression)
      = Assignment
      | BasicExpression

    ParenthesisedExpression (a parenthesised expression)
      = "(" Expression ")"

    Assignment (an assignment)
      = identifier "=" Expression

    BinaryMessage (a binary message)
      = binarySelector BinaryArgument

    BinaryArgument (a binary argument)
      = Primary DotMessage*

    DotMessage (a dot message)
      = "." identifier MessageParameters?

    MessageParameters (message parameters)
      =  "(" NonemptyListOf<BasicExpression, ","> ")"

    BasicExpression (a basic expression)
      = BasicDotExpression
      | BasicBinaryExpression
      | Primary

    BasicDotExpression (a basic dot expression)
      = Primary DotMessage+ BinaryMessage*

    BasicBinaryExpression (a basic binary expression)
      = Primary BinaryMessage+

    Temporaries (temporaries)
      = "var" NonemptyListOf<Temporary, ","> ";"

    TemporaryWithInitializer (a temporary with an initializer)
      = identifier "=" BasicExpression

    Temporary (a temporary)
      = TemporaryWithInitializer
      | identifier

    NonFinalExpression (an expression followed by statements)
      = Expression ";" Statements

    FinalExpression (a last expression)
      = Expression ";"?

    ReturnStatement (a return statement)
      = "^" Expression ";"?

    Statements (statements)
      = NonFinalExpression
      | FinalExpression
      | ReturnStatement

    Block (a block)
      = "{" BlockBody "}"

    BlockBody (a block body)
      = BlockArguments? Temporaries* Statements?

    BlockArguments (block arguments)
      = "arg" NonemptyListOf<identifier, ","> ";"

    ArrayExpression (an array expresssion)
      = "[" ListOf<Expression, ","> "]"

    ImplicitMessage (an implicit message)
      = identifier "(" ListOf<Expression, ","> ")"

    Primary (a primary)
      = ImplicitMessage
      | ArrayExpression
      | ParenthesisedExpression
      | Block
      | reservedIdentifier
      | identifier
      | literal
      | primitive

    literal (a literal)
      = numberLiteral
      | stringLiteral
      | symbolLiteral

    binaryChar
      = "!" | "%" | "&" | "*" | "+" | "/" | "<" | "=" | ">" | "?" | "@" | "~" | "|" | "-"

    binarySelector (a binary selector)
      = binaryChar+

    numberLiteral (a number)
      = floatLiteral
      | integerLiteral

    integerLiteral (an integer)
      = "-"? digit+

    floatLiteral (a float)
      = "-"? digit+ "." digit+

    letterOrDigit
      = letter
      | digit

    identifier (an identifier)
      = letter letterOrDigit*

    reservedIdentifier (a reserved identifier)
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

    primitive
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

stc.semanticsSt = stc.grammarSt.createSemantics();

stc.parseSt = function(str) {
	return stc.semanticsSt(stc.grammarSt.match(str));
}
