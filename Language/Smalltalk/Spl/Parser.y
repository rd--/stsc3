{
module Language.Smalltalk.Spl.Parser where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.Spl.Ast {- stsc3 -}
import           Language.Smalltalk.Spl.Ast {- stsc3 -}
import           Language.Smalltalk.Spl.Lexer {- stsc3 -}
import           Language.Smalltalk.Spl.Operator {- stsc3 -}
import           Language.Smalltalk.Spl.Token {- stsc3 -}
}

%name parseInitializerDefinition initializer_definition
%tokentype { Token }
%monad { Alex } { >>= } { return }
%lexer { lexer } { EOF }
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
      NaN { NaNIdentifier }
      Infinity { InfinityIdentifier }

      arity_qualified_lowercaseIdentifier { ArityQualifiedLowercaseIdentifier $$ }
      arity_qualified_uppercaseIdentifier { ArityQualifiedUppercaseIdentifier $$ }
      binary_selector { BinarySelector $$ }
      double_quoted_string { DoubleQuotedString $$ }
      float { Float $$ }
      lowercaseIdentifier { LowercaseIdentifier $$ }
      uppercaseIdentifier { UppercaseIdentifier $$ }
      negatedIdentifier { NegatedIdentifier $$ }
      integer { Integer $$ }
      single_quoted_string { SingleQuotedString $$ }

%%

initializer_definition :: { SplInitializerDefinition }
    : maybe_temporaries_seq maybe_statements { SplInitializerDefinition Nothing $1 $2 }

identifier :: { String }
    : lowercaseIdentifier { $1 }
    | uppercaseIdentifier { $1 }

arity_qualified_identifier :: { String }
    : arity_qualified_lowercaseIdentifier { $1 }
    | arity_qualified_uppercaseIdentifier { $1 }

binary_operator :: { (String, Maybe String) }
    : extended_binary_selector { ($1, Nothing) }
    | extended_binary_selector '.' lowercaseIdentifier { ($1, Just $3) }
    | '=' { ("=", Nothing) }

extended_binary_selector :: { String }
    : binary_selector { $1 }
    | '|' { "|" }

expression :: { SplExpression }
    : lowercaseIdentifier ':=' expression { SplExprAssignment $1 $3 }
    | syntax_put { SplExprBasic $1 }
    | basic_expression { SplExprBasic $1 }

syntax_put :: { SplBasicExpression }
    : primary '[' basic_expression ']' ':=' basic_expression { SplBasicExpression $1 (Just (splConstructDotMessage "at:put" [$3, $6])) }

syntax_quoted_at :: { SplBasicExpression }
    : primary '::' identifier { splConstructDotMessageSend $1 "at" [splLiteralToBasicExpression (St.StringLiteral ("'" ++ $3 ++ "'"))] }

basic_expression :: { SplBasicExpression }
    : primary maybe_messages { SplBasicExpression $1 $2 }
    | dictionary_expression maybe_messages { SplBasicExpression $1 $2 }

dictionary_expression :: { SplPrimary }
    : '(' ':' ')' { SplPrimaryDictionaryExpression [] }
    | '(' non_empty_dictionary_item_seq ')' { SplPrimaryDictionaryExpression $2 }

non_empty_dictionary_item_seq :: { [(St.Identifier, SplBasicExpression)] }
    : dictionary_item { [$1] }
    | dictionary_item ',' non_empty_dictionary_item_seq { $1 : $3 }

dictionary_item :: { (St.Identifier, SplBasicExpression) }
    : identifier ':' basic_expression { ($1, $3) }

maybe_messages :: { Maybe SplMessages }
    : { Nothing }
    | messages { Just $1 }

messages :: { SplMessages }
    : dot_message_seq maybe_binary_message_seq { SplMessagesDot $1 $2 }
    | binary_message_seq { SplMessagesBinary $1 }

maybe_dot_message_seq :: { Maybe [SplDotMessage] }
    : { Nothing }
    | dot_message_seq { Just $1 }

dot_message_seq :: { [SplDotMessage] }
    : dot_message dot_message_seq { $1 : $2 }
    | dot_message { [$1] }

dot_message :: { SplDotMessage }
    : '.' identifier { SplDotMessage $2 []}
    | '.' extended_binary_selector { SplDotMessage (resolveMethodName $2) []}
    | '.' identifier blockexpression_seq { SplDotMessage $2 $3}
    | '.' identifier message_param { SplDotMessage $2 $3}
    | '.' extended_binary_selector message_param { SplDotMessage (resolveMethodName $2) $3 }
    | '.' identifier message_param blockexpression_seq { SplDotMessage $2 ($3 ++ $4) }
    | syntax_at { $1 }

blockexpression :: { SplBasicExpression }
    : '{' block_body '}' { SplBasicExpression (SplPrimaryBlock $2) Nothing }

blockexpression_seq :: { [SplBasicExpression] }
    : blockexpression { [$1] }
    | blockexpression blockexpression_seq { $1 : $2 }

syntax_at :: { SplDotMessage }
    : '[' basic_expression ']' { SplDotMessage "at" [$2] }

message_param :: { [SplBasicExpression] }
    : '(' basic_expression_seq ')' { $2 }

basic_expression_seq :: { [SplBasicExpression] }
    : basic_expression { [$1] }
    | basic_expression ',' basic_expression_seq { $1 : $3 }

maybe_binary_message_seq :: { Maybe [SplBinaryMessage] }
    : { Nothing }
    | binary_message_seq { Just $1 }

binary_message_seq :: { [SplBinaryMessage] }
    : binary_message binary_message_seq { $1 : $2 }
    | binary_message { [$1] }

binary_message :: { SplBinaryMessage }
    : binary_operator binary_argument { SplBinaryMessage $1 $2 }

binary_argument :: { SplBinaryArgument }
    : primary maybe_dot_message_seq { SplBinaryArgument $1 $2 }

primary :: { SplPrimary }
    : syntax_quoted_at { SplPrimaryExpression (SplExprBasic $1) }
    | lowercaseIdentifier { SplPrimaryIdentifier $1 }
    | negatedIdentifier { splNegatedIdentifier $1 }
    | arity_qualified_identifier { SplPrimaryIdentifier $1 }
    | binary_selector { SplPrimaryIdentifier $1 }
    | reserved_identifier { SplPrimaryIdentifier $1 }
    | literal { SplPrimaryLiteral $1 }
    | '{' block_body '}' { SplPrimaryBlock $2 }
    | '(' expression ')' { SplPrimaryExpression $2 }
    | '[' expression_seq ']' { SplPrimaryArrayExpression $2 }
    | '(' tuple_expression_seq ')' { splBasicExpressionToPrimary (splConstructDotMessageSend (SplPrimaryArrayExpression $2) "asTuple" []) }
    | '[' vector_expression ']' { SplPrimaryArrayExpression $2 }
    | '[' matrix_expression ']' { splMatrixExpression $2 }
    | '(' expression '..' expression ')' { splIntervalRange $2 $4 }
    | '[' expression '..' expression ']' { splArrayRange $2 $4 }
    | '(' expression '..' expression ';' expression ')' { splFromToBy $2 $4 $6 }
    | '[' expression '..' expression ';' expression ']' { splArrayFromToBy $2 $4 $6 }
    | uppercaseIdentifier '[' expression_seq ']' { splListConstructorSyntax $1 (SplPrimaryArrayExpression $3) }
    | uppercaseIdentifier '[' vector_expression ']' { splListConstructorSyntax $1 (SplPrimaryArrayExpression $3) }
    | uppercaseIdentifier '[' matrix_expression ']' { splListConstructorSyntax $1 (splMatrixExpression $3) }
    | integer ':' integer { splIntervalRange (intExpr $1) (intExpr $3) }
    | integer ':' integer ':' integer { splFromToBy (intExpr $1) (intExpr $5) (intExpr $3) }
    | identifier '(' expression_seq ')' { SplPrimaryImplicitMessageSend $1 $3 }
    | identifier '(' expression_seq ')' blockexpression_seq { SplPrimaryImplicitMessageSend $1 ($3 ++ $5) }

reserved_identifier :: { St.Identifier }
    : nil { "nil" }
    | true { "true" }
    | false { "false" }
    | NaN { "NaN" }
    | Infinity { "Infinity" }

vector_expression :: { [SplBasicExpression] }
    : vector_item vector_item { [$1, $2] }
    | vector_item vector_expression { $1 : $2 }

vector_item :: { SplBasicExpression }
    : lowercaseIdentifier { splIdentifierToBasicExpression $1 }
    | literal { splLiteralToBasicExpression $1 }

matrix_expression :: { [[SplBasicExpression]] }
    : vector_expression ';' vector_expression { [$1, $3] }
    | vector_expression ';' matrix_expression { $1 : $3 }

expression_seq :: { [SplBasicExpression] }
    : { [] }
    | non_empty_expression_seq { $1 }

non_empty_expression_seq :: { [SplBasicExpression] }
    : basic_expression { [$1] }
    | basic_expression ',' non_empty_expression_seq { $1 : $3 }

tuple_expression_seq :: { [SplBasicExpression] }
    : basic_expression ',' basic_expression { [$1, $3] }
    | basic_expression ',' tuple_expression_seq { $1 : $3 }

block_body :: { SplBlockBody }
    : maybe_arguments maybe_temporaries_seq maybe_statements { SplBlockBody $1 $2 $3 }

maybe_arguments :: { Maybe [SplBlockArgument] }
    : { Nothing }
    | arguments { Just $1 }

arguments :: { [SplBlockArgument] }
    : arg_name_seq '|' { $1 }

{-
default_var_seq :: { [SplBlockArgument] }
    : default_var { [$1] }
    | default_var ',' default_var_seq { $1 : $3 }

default_var :: { SplBlockArgument }
    : identifier { ($1,Nothing) }
    | identifier '=' literal { ($1,Just $3) }
-}

arg_name_seq :: { [SplBlockArgument] }
    : arg_name { [$1] }
    | arg_name arg_name_seq { $1 : $2 }

arg_name :: { SplBlockArgument }
    : ':' arity_qualified_lowercaseIdentifier { ($2,Nothing) }
    | ':' lowercaseIdentifier { ($2,Nothing) }

maybe_temporaries_seq :: { Maybe [SplTemporaries] }
    : { Nothing }
    | temporaries_seq { Just $1 }

temporaries_seq :: { [SplTemporaries] }
    : temporaries { [$1] }
    | temporaries temporaries_seq { $1 : $2 }

temporaries :: { SplTemporaries }
    : let temporary_seq ';' { $2 }
    | '|' temporary_name_seq '|'{ $2 }

temporary_name_seq :: { SplTemporaries }
    : lowercaseIdentifier { [($1, Nothing)] }
    | lowercaseIdentifier temporary_name_seq { ($1, Nothing) : $2 }

temporary_seq :: { SplTemporaries }
    : temporary { [$1] }
    | temporary ',' temporary_seq { $1 : $3 }

temporary :: { SplTemporary }
    : lowercaseIdentifier_maybe_arity_qualified { ($1,Nothing) }
    | lowercaseIdentifier_maybe_arity_qualified '=' basic_expression { ($1,Just $3) }

lowercaseIdentifier_maybe_arity_qualified :: { St.Identifier }
    : lowercaseIdentifier { $1 }
    | arity_qualified_lowercaseIdentifier { $1 }

maybe_statements :: { Maybe SplStatements }
    : { Nothing }
    | statements { Just $1 }

statements :: { SplStatements }
    : expression { SplStatementsExpression $1 Nothing }
    | expression ';' statements { SplStatementsExpression $1 (Just $3) }

literal :: { St.Literal }
    : integer { St.NumberLiteral (St.Int $1) }
    | float { St.NumberLiteral (St.Float $1) }
    | double_quoted_string { St.StringLiteral $1 }
    | single_quoted_string { St.StringLiteral $1 }

{
parseError :: Token -> Alex a
parseError t = do
  ((AlexPn _ line column), _, _, _) <- alexGetInput
  alexError ("Parse error at line " ++ (show line) ++ ", column " ++ (show column) ++ ", token" ++ (show t))

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

intExpr :: Integer -> SplExpression
intExpr = SplExprBasic . splLiteralToBasicExpression . St.NumberLiteral . St.Int
}
