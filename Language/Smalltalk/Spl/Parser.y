{
module Language.Smalltalk.Spl.Parser where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.Stc.Ast {- stsc3 -}
import           Language.Smalltalk.Spl.Ast {- stsc3 -}
import           Language.Smalltalk.Spl.Lexer {- stsc3 -}
import           Language.Smalltalk.Spl.Operator {- stsc3 -}
import           Language.Smalltalk.Spl.Token {- stsc3 -}
}

%name parseInitializerDefinition initializer_definition
%tokentype { Token }
%error { parseError }

%token
      '(' { LeftParen }
      ')' { RightParen }
      ',' { Comma }
      '.' { Dot }
      '..' { DotDot }
      ':' { Colon }
      '::' { ColonColon }
      ':=' { ColonEquals }
      ';' { SemiColon }
      '=' { Equals }
      '[' { LeftBracket }
      ']' { RightBracket }
      '{' { LeftBrace }
      '|' { VerticalBar }
      '}' { RightBrace }

      false { FalseIdentifier }
      let { Let }
      nil { NilIdentifier }
      true { TrueIdentifier }

      arity_qualified_identifier { ArityQualifiedIdentifier $$ }
      binary_selector { BinarySelector $$ }
      double_quoted_string { DoubleQuotedString $$ }
      float { Float $$ }
      identifier { Identifier $$ }
      integer { Integer $$ }
      single_quoted_string { SingleQuotedString $$ }

%%

initializer_definition :: { StcInitializerDefinition }
    : maybe_temporaries_seq maybe_statements { StcInitializerDefinition Nothing $1 $2 }

binary_operator :: { (String, Maybe String) }
    : extended_binary_selector { ($1, Nothing) }
    | extended_binary_selector '.' identifier { ($1, Just $3) }
    | '=' { ("=", Nothing) }

extended_binary_selector :: { String }
    : binary_selector { $1 }
    | '|' { "|" }

expression :: { StcExpression }
    : identifier ':=' expression { StcExprAssignment $1 $3 }
    | syntax_at_put { StcExprBasic $1 }
    | basic_expression { StcExprBasic $1 }

syntax_at_put :: { StcBasicExpression }
    : primary '[' basic_expression ']' ':=' basic_expression { StcBasicExpression $1 (Just (stcConstructDotMessage "at:put" [$3, $6])) }

syntax_quoted_at :: { StcBasicExpression }
    : primary '::' identifier { stcConstructDotMessageSend $1 "at" [stcLiteralToBasicExpression (St.StringLiteral ("'" ++ $3 ++ "'"))] }

basic_expression :: { StcBasicExpression }
    : primary maybe_messages { StcBasicExpression $1 $2 }
    | dictionary_expression maybe_messages { StcBasicExpression $1 $2 }

dictionary_expression :: { StcPrimary }
    : '(' dictionary_item_seq ')' { StcPrimaryDictionaryExpression $2 }

dictionary_item_seq :: { [(St.Identifier, StcBasicExpression)] }
    : { [] }
    | non_empty_dictionary_item_seq { $1 }

non_empty_dictionary_item_seq :: { [(St.Identifier, StcBasicExpression)] }
    : dictionary_item { [$1] }
    | dictionary_item ',' dictionary_item_seq { $1 : $3 }

dictionary_item :: { (St.Identifier, StcBasicExpression) }
    : identifier ':' basic_expression { ($1, $3) }

maybe_messages :: { Maybe StcMessages }
    : { Nothing }
    | messages { Just $1 }

messages :: { StcMessages }
    : dot_message_seq maybe_binary_message_seq { StcMessagesDot $1 $2 }
    | binary_message_seq { StcMessagesBinary $1 }

maybe_dot_message_seq :: { Maybe [StcDotMessage] }
    : { Nothing }
    | dot_message_seq { Just $1 }

dot_message_seq :: { [StcDotMessage] }
    : dot_message dot_message_seq { $1 : $2 }
    | dot_message { [$1] }

dot_message :: { StcDotMessage }
    : '.' identifier { StcDotMessage $2 []}
    | '.' extended_binary_selector { StcDotMessage (resolveMethodName $2) []}
    | '.' identifier blockexpression_seq { StcDotMessage $2 $3}
    | '.' identifier message_param { StcDotMessage $2 $3}
    | '.' extended_binary_selector message_param { StcDotMessage (resolveMethodName $2) $3 }
    | '.' identifier message_param blockexpression_seq { StcDotMessage $2 ($3 ++ $4) }
    | syntax_at { $1 }

blockexpression :: { StcBasicExpression }
    : '{' block_body '}' { StcBasicExpression (StcPrimaryBlock $2) Nothing }

blockexpression_seq :: { [StcBasicExpression] }
    : blockexpression { [$1] }
    | blockexpression blockexpression_seq { $1 : $2 }

syntax_at :: { StcDotMessage }
    : '[' basic_expression ']' { StcDotMessage "at" [$2] }

message_param :: { [StcBasicExpression] }
    : '(' basic_expression_seq ')' { $2 }

basic_expression_seq :: { [StcBasicExpression] }
    : basic_expression { [$1] }
    | basic_expression ',' basic_expression_seq { $1 : $3 }

maybe_binary_message_seq :: { Maybe [StcBinaryMessage] }
    : { Nothing }
    | binary_message_seq { Just $1 }

binary_message_seq :: { [StcBinaryMessage] }
    : binary_message binary_message_seq { $1 : $2 }
    | binary_message { [$1] }

binary_message :: { StcBinaryMessage }
    : binary_operator binary_argument { StcBinaryMessage $1 $2 }

binary_argument :: { StcBinaryArgument }
    : primary maybe_dot_message_seq { StcBinaryArgument $1 $2 }

primary :: { StcPrimary }
    : syntax_quoted_at { StcPrimaryExpression (StcExprBasic $1) }
    | identifier { StcPrimaryIdentifier $1 }
    | arity_qualified_identifier { StcPrimaryIdentifier $1 }
    | binary_selector { StcPrimaryIdentifier $1 }
    | reserved_identifier { StcPrimaryIdentifier $1 }
    | literal { StcPrimaryLiteral $1 }
    | '{' block_body '}' { StcPrimaryBlock $2 }
    | '(' expression ')' { StcPrimaryExpression $2 }
    | '[' expression_seq ']' { StcPrimaryArrayExpression $2 }
    | '(' tuple_expression_seq ')' { stcBasicExpressionToPrimary (stcConstructDotMessageSend (StcPrimaryArrayExpression $2) "asTuple" []) }
    | '[' vector_expression ']' { StcPrimaryArrayExpression $2 }
    | '[' matrix_expression ']' { splMatrixExpression $2 }
    | '(' expression '..' expression ')' { stcIntervalRange $2 $4 }
    | '[' expression '..' expression ']' { stcArrayRange $2 $4 }
    | integer ':' integer { stcIntervalRange (intExpr $1) (intExpr $3) }
    | integer ':' integer ':' integer { stcFromToBy (intExpr $1) (intExpr $5) (intExpr $3) }
    | identifier '(' expression_seq ')' { StcPrimaryImplicitMessageSend $1 $3 }
    | identifier '(' expression_seq ')' blockexpression_seq { StcPrimaryImplicitMessageSend $1 ($3 ++ $5) }

reserved_identifier :: { St.Identifier }
    : nil { "nil" }
    | true { "true" }
    | false { "false" }

vector_expression :: { [StcBasicExpression] }
    : vector_item vector_item { [$1, $2] }
    | vector_item vector_expression { $1 : $2 }

vector_item :: { StcBasicExpression }
    : identifier { stcIdentifierToBasicExpression $1 }
    | literal { stcLiteralToBasicExpression $1 }

matrix_expression :: { [[StcBasicExpression]] }
    : vector_expression ';' vector_expression { [$1, $3] }
    | vector_expression ';' matrix_expression { $1 : $3 }

expression_seq :: { [StcBasicExpression] }
    : { [] }
    | non_empty_expression_seq { $1 }

non_empty_expression_seq :: { [StcBasicExpression] }
    : basic_expression { [$1] }
    | basic_expression ',' non_empty_expression_seq { $1 : $3 }

tuple_expression_seq :: { [StcBasicExpression] }
    : basic_expression ',' basic_expression { [$1, $3] }
    | basic_expression ',' tuple_expression_seq { $1 : $3 }

block_body :: { StcBlockBody }
    : maybe_arguments maybe_temporaries_seq maybe_statements { StcBlockBody $1 $2 $3 }

maybe_arguments :: { Maybe [StcBlockArgument] }
    : { Nothing }
    | arguments { Just $1 }

arguments :: { [StcBlockArgument] }
    : arg_name_seq '|' { $1 }

{-
default_var_seq :: { [StcBlockArgument] }
    : default_var { [$1] }
    | default_var ',' default_var_seq { $1 : $3 }

default_var :: { StcBlockArgument }
    : identifier { ($1,Nothing) }
    | identifier '=' literal { ($1,Just $3) }
-}

arg_name_seq :: { [StcBlockArgument] }
    : arg_name { [$1] }
    | arg_name arg_name_seq { $1 : $2 }

arg_name :: { StcBlockArgument }
    : ':' identifier { ($2,Nothing) }

maybe_temporaries_seq :: { Maybe [StcTemporaries] }
    : { Nothing }
    | temporaries_seq { Just $1 }

temporaries_seq :: { [StcTemporaries] }
    : temporaries { [$1] }
    | temporaries temporaries_seq { $1 : $2 }

temporaries :: { StcTemporaries }
    : let temporary_seq ';' { $2 }
    | '|' temporary_name_seq '|'{ $2 }

temporary_name_seq :: { StcTemporaries }
    : identifier { [($1, Nothing)] }
    | identifier temporary_name_seq { ($1, Nothing) : $2 }

temporary_seq :: { StcTemporaries }
    : temporary { [$1] }
    | temporary ',' temporary_seq { $1 : $3 }

temporary :: { StcTemporary }
    : identifier_or_arity_qualified_identifier { ($1,Nothing) }
    | identifier_or_arity_qualified_identifier '=' basic_expression { ($1,Just $3) }

identifier_or_arity_qualified_identifier :: { St.Identifier }
    : identifier { $1 }
    | arity_qualified_identifier { $1 }

maybe_statements :: { Maybe StcStatements }
    : { Nothing }
    | statements { Just $1 }

statements :: { StcStatements }
    : expression { StcStatementsExpression $1 Nothing }
    | expression ';' statements { StcStatementsExpression $1 (Just $3) }

literal :: { St.Literal }
    : integer { St.NumberLiteral (St.Int $1) }
    | float { St.NumberLiteral (St.Float $1) }
    | double_quoted_string { St.StringLiteral $1 }
    | single_quoted_string { St.StringLiteral $1 }

{
parseError :: [Token] -> a
parseError t = error ("Parse error: " ++ show t)

intExpr :: Integer -> StcExpression
intExpr = StcExprBasic . stcLiteralToBasicExpression . St.NumberLiteral . St.Int
}
