{
module Language.Smalltalk.SuperCollider.Parser where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.Spl.Ast {- stsc3 -}
import           Language.Smalltalk.Spl.Lexer {- stsc3 -}
import           Language.Smalltalk.Spl.Token {- stsc3 -}
}

%name parseInitializerDefinition initializer_definition
%name parseClassDefinitionSeq class_definition_seq
%name parseClassExtensionSeq class_extension_seq
%tokentype { Token }
%error { parseError }

%token
      '#[' { HashLeftBracket }
      '(' { LeftParen }
      ')' { RightParen }
      '*' { Asterisk }
      '+' { Plus }
      ',' { Comma }
      '.' { Dot }
      '..' { DotDot }
      ':' { Colon }
      ';' { SemiColon }
      '=' { Equals }
      '[' { LeftBracket }
      ']' { RightBracket }
      '{' { LeftBrace }
      '|' { VerticalBar }
      '}' { RightBrace }

      arg { Arg }
      class_var { ClassVar }
      false { FalseIdentifier }
      nil { NilIdentifier }
      self { SelfIdentifier }
      true { TrueIdentifier }
      var { Var }

      '^' { ReturnOperator }

      binary_selector { BinarySelector $$ }
      double_quoted_string { DoubleQuotedString $$ }
      float { Float $$ }
      identifier { Identifier $$ }
      integer { Integer $$ }
      keyword { Keyword $$ }
      keyword_selector { KeywordSelector $$ }
      quoted_char { QuotedCharacter $$ }
      single_quoted_string { SingleQuotedString $$ }

%%

initializer_definition :: { SplInitializerDefinition }
    : maybe_temporaries_seq maybe_statements { SplInitializerDefinition Nothing $1 $2 }

class_extension_seq :: { [SplClassExtension] }
    : { [] }
    | class_extension class_extension_seq { $1 : $2 }

class_extension :: { SplClassExtension }
    : '+' identifier '{' method_definition_seq '}' { SplClassExtension $2 $4 }

class_definition_seq :: { [SplClassDefinition] }
    : { [] }
    | class_definition class_definition_seq { $1 : $2 }

class_definition :: { SplClassDefinition }
    : identifier maybe_super_class '{' maybe_class_variables maybe_variables method_definition_seq '}' { SplClassDefinition $1 $2 $5 $4 $6 Nothing Nothing }

maybe_super_class :: { Maybe St.Identifier }
    : { Just "Object" }
    | ':' identifier { (if $2 == "nil" then Nothing else Just $2) }

method_definition_seq :: { [SplMethodDefinition] }
    : { [] }
    | method_definition method_definition_seq { $1 : $2 }

binary_operator :: { (String, Maybe String) }
    : extended_binary_selector { ($1, Nothing) }
    | extended_binary_selector '.' identifier { ($1, Just $3) }

extended_binary_selector :: { String }
    : binary_selector { $1 }
    | '+' { "+" }
    | '*' { "*" }
    | '|' { "|" }

method_definition :: { SplMethodDefinition }
    : '*' identifier '{' block_body '}' { SplMethodDefinition True $2 $4 Nothing Nothing }
    | identifier_or_binary_selector_or_keyword_selector '{' block_body '}' { SplMethodDefinition False $1 $3 Nothing Nothing }

identifier_or_binary_selector_or_keyword_selector :: { String }
    : identifier { $1 }
    | extended_binary_selector { $1 }
    | keyword_selector { $1 }

maybe_class_variables :: { Maybe [SplVariable] }
    : { Nothing }
    | class_variables { Just $1 }

class_variables :: { [SplVariable] }
    : class_var default_var_seq ';' { $2 }

maybe_variables :: { Maybe [SplVariable] }
    : { Nothing }
    | variables { Just $1 }

variables :: { [SplVariable] }
    : var default_var_seq ';' { $2 }

expression :: { SplExpression }
    : identifier '=' expression { SplExprAssignment $1 $3 }
    | syntax_at_put { SplExprBasic $1 }
    | basic_expression { SplExprBasic $1 }

syntax_at_put :: { SplBasicExpression }
    : primary '[' basic_expression ']' '=' basic_expression { SplBasicExpression $1 (Just (splConstructDotMessage "at:put" [$3, $6])) }

basic_expression :: { SplBasicExpression }
    : primary maybe_messages { SplBasicExpression $1 $2 }
    | dictionary_expression maybe_messages { SplBasicExpression $1 $2 }

dictionary_expression :: { SplPrimary }
    : '(' keyword_expression_seq ')' { SplPrimaryDictionaryExpression $2 }

keyword_expression_seq :: { [(St.Identifier, SplBasicExpression)] }
    : { [] }
    | non_empty_keyword_expression_seq { $1 }

non_empty_keyword_expression_seq :: { [(St.Identifier, SplBasicExpression)] }
    : keyword_expression { [$1] }
    | keyword_expression ',' keyword_expression_seq { $1 : $3 }

keyword_expression :: { (St.Identifier, SplBasicExpression) }
    : keyword basic_expression { ($1, $2) }

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
    | '.' identifier block_expression_seq { SplDotMessage $2 $3}
    | '.' identifier message_param { SplDotMessage $2 $3}
    | '.' identifier message_param block_expression_seq { SplDotMessage $2 ($3 ++ $4) }
    | '.' identifier keyword_message_param { splDotMessageFromKeywordParam $2 $3 }
    | syntax_at { $1 }

block_expression :: { SplBasicExpression }
    : '{' block_body '}' { SplBasicExpression (SplPrimaryBlock $2) Nothing }

block_expression_seq :: { [SplBasicExpression] }
    : block_expression { [$1] }
    | block_expression block_expression_seq { $1 : $2 }

syntax_at :: { SplDotMessage }
    : '[' basic_expression ']' { SplDotMessage "at" [$2] }

message_param :: { [SplBasicExpression] }
    : '(' non_empty_basic_expression_seq ')' { $2 }

keyword_message_param :: { (SplBasicExpression, [(St.Identifier, SplBasicExpression)]) }
    : '(' basic_expression ',' non_empty_keyword_expression_seq ')' { ($2, $4) }

non_empty_basic_expression_seq :: { [SplBasicExpression] }
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
    : identifier { SplPrimaryIdentifier $1 }
    | reserved_identifier { SplPrimaryIdentifier $1 }
    | literal { SplPrimaryLiteral $1 }
    | '{' block_body '}' { SplPrimaryBlock $2 }
    | '(' expression ')' { SplPrimaryExpression $2 }
    | '[' basic_expression_seq ']' { SplPrimaryArrayExpression $2 }
    | '(' expression '..' expression ')' { splIntervalRange $2 $4 }
    | '(' expression ',' expression '..' expression ')' { splFromThenTo $2 $4 $6 }
    | '[' expression '..' expression ']' { splArrayRange $2 $4 }
    | identifier '(' basic_expression_seq ')' { SplPrimaryImplicitMessageSend $1 $3 }
    | identifier '(' basic_expression_seq ')' block_expression_seq { SplPrimaryImplicitMessageSend $1 ($3 ++ $5) }
    | identifier '(' non_empty_keyword_expression_seq ')' { splPrimaryKeywordMessageSend (SplPrimaryIdentifier $1) $3 }

reserved_identifier :: { St.Identifier }
    : nil { "nil" }
    | true { "true" }
    | false { "false" }
    | self { "self" }

basic_expression_seq :: { [SplBasicExpression] }
    : { [] }
    | non_empty_basic_expression_seq { $1 }

block_body :: { SplBlockBody }
    : maybe_arguments maybe_temporaries_seq maybe_statements { SplBlockBody $1 $2 $3 }

maybe_arguments :: { Maybe [SplBlockArgument] }
    : { Nothing }
    | arguments { Just $1 }

arguments :: { [SplBlockArgument] }
    : arg default_var_seq ';' { $2 }
    | '|' default_var_seq '|' { $2 }

default_var_seq :: { [SplBlockArgument] }
    : default_var { [$1] }
    | default_var default_var_seq { $1 : $2 }
    | default_var ',' default_var_seq { $1 : $3 }

default_var :: { SplBlockArgument }
    : identifier { ($1,Nothing) }
    | identifier '=' literal { ($1,Just $3) }

maybe_temporaries_seq :: { Maybe [SplTemporaries] }
    : { Nothing }
    | temporaries_seq { Just $1 }

temporaries_seq :: { [SplTemporaries] }
    : temporaries { [$1] }
    | temporaries temporaries_seq { $1 : $2 }

temporaries :: { SplTemporaries }
    : var temporary_seq ';' { $2 }

temporary_seq :: { SplTemporaries }
    : temporary { [$1] }
    | temporary ',' temporary_seq { $1 : $3 }

temporary :: { SplTemporary }
    : identifier { ($1,Nothing) }
    | identifier '=' basic_expression { ($1,Just $3) }

maybe_statements :: { Maybe SplStatements }
    : { Nothing }
    | statements { Just $1 }

statements :: { SplStatements }
    : return_statement { $1 }
    | expression opt_semicolon { SplStatementsExpression $1 Nothing }
    | expression ';' statements { SplStatementsExpression $1 (Just $3) }

return_statement :: { SplStatements }
    : '^' expression opt_semicolon { SplStatementsReturn (SplReturnStatement $2) }

literal :: { St.Literal }
    : integer { St.NumberLiteral (St.Int $1) }
    | float { St.NumberLiteral (St.Float $1) }
    | double_quoted_string { St.StringLiteral $1 }
    | quoted_char { St.CharacterLiteral $1 }
    | single_quoted_string { St.SymbolLiteral $1 }
    | '#[' array_literal ']' { St.ArrayLiteral $2 }

array_literal :: { [Either St.Literal St.Identifier] }
    : { [] }
    | array_literal_elem { [$1] }
    | array_literal_elem ',' array_literal { $1 : $3 }

array_literal_elem :: { Either St.Literal St.Identifier }
    : literal{ Left $1 }
    | reserved_identifier{ Right $1 }

opt_semicolon :: { () }
    : { () }
    | ';' { () }

{
parseError :: [Token] -> a
parseError t = error ("Parse error: " ++ show t)
}
