import ohm from 'https://unpkg.com/ohm-js@16/dist/ohm.esm.js';
import { extras } from 'https://unpkg.com/ohm-js@16/dist/ohm.esm.js';

export const stcGrammar = ohm.grammar(String.raw`
Stc {

    TopLevel
      = ClassExpression+
      | InitializerDefinition

    InitializerDefinition
      = Temporaries? ExpressionSequence

    ExpressionSequence
      = ListOf<Expression, ";">

    Temporaries
      = TemporariesKeyword+
      | TemporariesWithInitializerSyntax
      | TemporariesSyntax

    TemporariesKeyword
      = "var" NonemptyListOf<Temporary, ","> ";"

    TemporariesSyntax
      = "|" identifier+ "|"

    TemporariesWithInitializerSyntax
      = "|" NonemptyListOf<TemporaryWithInitializer, ","> ";" "|"

    TemporaryWithInitializer
      = TemporaryWithIdentifierInitializer
      | TemporaryWithPatternInitializer

    TemporaryWithIdentifierInitializer
      = identifier "=" Expression

    TemporaryWithPatternInitializer
      = "("  NonemptyListOf<identifier, ","> ")" "=" identifier

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

    Routine
      = "{!" RoutineBody "}"

    RoutineBody
      = BlockArguments? Temporaries? Primitive? Statements?

    Block
      = "{" BlockBody "}"

    BlockBody
      = BlockArguments? Temporaries? Primitive? Statements?

    BlockArguments
      = BlockArgumentsSyntax
      | BlockArgumentsKeyword

    ArgumentName
      = ":" identifier

    BlockArgumentsSyntax
      = ArgumentName+ "|"

    BlockArgumentsKeyword
      = "arg" NonemptyListOf<identifier, ","> ";"

    Primitive
      = "<primitive:" primitiveCharacter* ">"

    ArrayExpression
      = "[" ListOf<Expression, ","> "]"

    ArrayRangeSyntax
      = "[" Expression ".." Expression "]"

    AssociationExpression
      = identifier ":" Expression

    DictionaryExpression
      = "(" ListOf<AssociationExpression, ","> ")"

    AtSyntax
      = Primary "[" Expression "]"

    PutSyntax
      = Primary "[" Expression "]" ":=" Expression

    AtQuotedSyntax
      = Primary ":" identifier

    PutQuotedSyntax
      = Primary ":" identifier ":=" Expression

    ParameterList
      =  "(" ListOf<Expression, ","> ")"

    ImplicitMessage
      = identifier ParameterList

    ImplicitMessageWithTrailingClosures
      = identifier NonEmptyParameterList? Block+

    ParenthesisedExpression
      = "(" Expression ")"

    Assignment
      = identifier ":=" Expression

    Primary
      = PutSyntax
      | PutQuotedSyntax
      | AtSyntax
      | AtQuotedSyntax
      | DotExpressionWithTrailingClosures
      | DotExpression
      | Routine
      | Block
      | ImplicitMessageWithTrailingClosures
      | ImplicitMessage
      | reservedIdentifier
      | identifier
      | literal
      | ParenthesisedExpression
      | DictionaryExpression
      | ArrayExpression
      | ArrayRangeSyntax

    BinaryExpression
      = Expression (binaryOperator Primary)+

    NonEmptyParameterList
      =  "(" NonemptyListOf<Expression, ","> ")"

    DotExpression
      = Primary ("." identifier ~"{" NonEmptyParameterList?)+

    DotExpressionWithTrailingClosures
      = Primary "." identifier NonEmptyParameterList? Block+

    Expression
      = Assignment
      | BinaryExpression
      | Primary

    ClassExpression
      = ClassExtension
      | ClassDefinition
      | TraitExtension
      | TraitDefinition

    TraitList
      = ":" "[" NonemptyListOf<identifier, ","> "]"

    TraitDefinition
      = "@" identifier "{" (methodName Block)* "}"

    TraitExtension
      = "+" "@" identifier "{" (methodName Block)* "}"

    ClassDefinition
      = identifier TraitList? "{" Temporaries? (methodName Block)* "}"

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

    comment
      = multiLineComment
      | singleLineComment

    multiLineComment
       = "/*" (~"*/" sourceCharacter)* "*/"

    singleLineComment
      = "//" (~lineTerminator sourceCharacter)*

    space
      += comment

    sourceCharacter
      = any

    lineTerminator
      = "\n"
      | "\r"
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
