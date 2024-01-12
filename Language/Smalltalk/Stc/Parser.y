{
module Language.Smalltalk.Stc.Parser where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.Stc.Ast {- stsc3 -}
import           Language.Smalltalk.Stc.Lexer {- stsc3 -}
import           Language.Smalltalk.Stc.Token {- stsc3 -}
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
      classvar { ClassVar }
      false { FalseIdentifier }
      nil { NilIdentifier }
      self { SelfIdentifier }
      true { TrueIdentifier }
      var { Var }

      ':=' { AssignmentOperator }
      '^' { ReturnOperator }

      binary_selector { BinarySelector $$ }
      double_quoted_string { DoubleQuotedString $$ }
      float { Float $$ }
      identifier { Identifier $$ }
      integer { Integer $$ }
      keyword { Keyword $$ }
      keyword_selector { KeywordSelector $$ }
      quoted_char { QuotedChar $$ }
      single_quoted_string { SingleQuotedString $$ }

%%

initializer_definition :: { StcInitializerDefinition }
    : maybe_temporaries_seq maybe_statements { StcInitializerDefinition Nothing $1 $2 }

class_extension_seq :: { [StcClassExtension] }
    : { [] }
    | class_extension class_extension_seq { $1 : $2 }

class_extension :: { StcClassExtension }
    : '+' identifier '{' method_definition_seq '}' { StcClassExtension $2 $4 }

class_definition_seq :: { [StcClassDefinition] }
    : { [] }
    | class_definition class_definition_seq { $1 : $2 }

class_definition :: { StcClassDefinition }
    : identifier maybe_super_class '{' maybe_class_variables maybe_variables method_definition_seq '}' { StcClassDefinition $1 $2 $5 $4 $6 Nothing Nothing }

maybe_super_class :: { Maybe St.Identifier }
    : { Just "Object" }
    | ':' identifier { (if $2 == "nil" then Nothing else Just $2) }

method_definition_seq :: { [StcMethodDefinition] }
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

method_definition :: { StcMethodDefinition }
    : '*' identifier '{' block_body '}' { StcMethodDefinition True $2 $4 Nothing Nothing }
    | identifier_or_binary_selector_or_keyword_selector '{' block_body '}' { StcMethodDefinition False $1 $3 Nothing Nothing }

identifier_or_binary_selector_or_keyword_selector :: { String }
    : identifier { $1 }
    | extended_binary_selector { $1 }
    | keyword_selector { $1 }

maybe_class_variables :: { Maybe [StcVariable] }
    : { Nothing }
    | class_variables { Just $1 }

class_variables :: { [StcVariable] }
    : classvar default_var_seq ';' { $2 }

maybe_variables :: { Maybe [StcVariable] }
    : { Nothing }
    | variables { Just $1 }

variables :: { [StcVariable] }
    : var default_var_seq ';' { $2 }

expression :: { StcExpression }
    : identifier ':=' expression { StcExprAssignment $1 $3 }
    | syntax_at_put { StcExprBasic $1 }
    | basic_expression { StcExprBasic $1 }

syntax_at_put :: { StcBasicExpression }
    : primary '[' basic_expression ']' ':=' basic_expression { StcBasicExpression $1 (Just (stcConstructDotMessage "at:put" [$3, $6])) }

basic_expression :: { StcBasicExpression }
    : primary maybe_messages { StcBasicExpression $1 $2 }
    | dictionary_expression maybe_messages { StcBasicExpression $1 $2 }

dictionary_expression :: { StcPrimary }
    : '(' keyword_expression_seq ')' { StcPrimaryDictionaryExpression $2 }

keyword_expression_seq :: { [(St.Identifier, StcBasicExpression)] }
    : { [] }
    | non_empty_keyword_expression_seq { $1 }

non_empty_keyword_expression_seq :: { [(St.Identifier, StcBasicExpression)] }
    : keyword_expression { [$1] }
    | keyword_expression ',' keyword_expression_seq { $1 : $3 }

keyword_expression :: { (St.Identifier, StcBasicExpression) }
    : keyword basic_expression { ($1, $2) }

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
    | '.' identifier block_expression_seq { StcDotMessage $2 $3}
    | '.' identifier message_param { StcDotMessage $2 $3}
    | '.' identifier message_param block_expression_seq { StcDotMessage $2 ($3 ++ $4) }
    | '.' identifier keyword_message_param { stcDotMessageFromKeywordParam $2 $3 }
    | syntax_at { $1 }

block_expression :: { StcBasicExpression }
    : '{' block_body '}' { StcBasicExpression (StcPrimaryBlock $2) Nothing }

block_expression_seq :: { [StcBasicExpression] }
    : block_expression { [$1] }
    | block_expression block_expression_seq { $1 : $2 }

syntax_at :: { StcDotMessage }
    : '[' basic_expression ']' { StcDotMessage "at" [$2] }

message_param :: { [StcBasicExpression] }
    : '(' non_empty_basic_expression_seq ')' { $2 }

keyword_message_param :: { (StcBasicExpression, [(St.Identifier, StcBasicExpression)]) }
    : '(' basic_expression ',' non_empty_keyword_expression_seq ')' { ($2, $4) }

non_empty_basic_expression_seq :: { [StcBasicExpression] }
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
    : identifier { StcPrimaryIdentifier $1 }
    | reserved_identifier { StcPrimaryIdentifier $1 }
    | literal { StcPrimaryLiteral $1 }
    | '{' block_body '}' { StcPrimaryBlock $2 }
    | '(' expression ')' { StcPrimaryExpression $2 }
    | '[' basic_expression_seq ']' { StcPrimaryArrayExpression $2 }
    | '(' expression '..' expression ')' { stcIntervalRange $2 $4 }
    | '(' expression ',' expression '..' expression ')' { stcFromThenTo $2 $4 $6 }
    | '[' expression '..' expression ']' { stcArrayRange $2 $4 }
    | identifier '(' basic_expression_seq ')' { StcPrimaryImplicitMessageSend $1 $3 }
    | identifier '(' basic_expression_seq ')' block_expression_seq { StcPrimaryImplicitMessageSend $1 ($3 ++ $5) }
    | identifier '(' non_empty_keyword_expression_seq ')' { stcPrimaryKeywordMessageSend (StcPrimaryIdentifier $1) $3 }

reserved_identifier :: { St.Identifier }
    : nil { "nil" }
    | true { "true" }
    | false { "false" }
    | self { "self" }

basic_expression_seq :: { [StcBasicExpression] }
    : { [] }
    | non_empty_basic_expression_seq { $1 }

block_body :: { StcBlockBody }
    : maybe_arguments maybe_temporaries_seq maybe_statements { StcBlockBody $1 $2 $3 }

maybe_arguments :: { Maybe [StcBlockArgument] }
    : { Nothing }
    | arguments { Just $1 }

arguments :: { [StcBlockArgument] }
    : arg default_var_seq ';' { $2 }
    | '|' default_var_seq '|' { $2 }

default_var_seq :: { [StcBlockArgument] }
    : default_var { [$1] }
    | default_var default_var_seq { $1 : $2 }
    | default_var ',' default_var_seq { $1 : $3 }

default_var :: { StcBlockArgument }
    : identifier { ($1,Nothing) }
    | identifier '=' literal { ($1,Just $3) }

maybe_temporaries_seq :: { Maybe [StcTemporaries] }
    : { Nothing }
    | temporaries_seq { Just $1 }

temporaries_seq :: { [StcTemporaries] }
    : temporaries { [$1] }
    | temporaries temporaries_seq { $1 : $2 }

temporaries :: { StcTemporaries }
    : var temporary_seq ';' { $2 }

temporary_seq :: { StcTemporaries }
    : temporary { [$1] }
    | temporary ',' temporary_seq { $1 : $3 }

temporary :: { StcTemporary }
    : identifier { ($1,Nothing) }
    | identifier '=' basic_expression { ($1,Just $3) }

maybe_statements :: { Maybe StcStatements }
    : { Nothing }
    | statements { Just $1 }

statements :: { StcStatements }
    : return_statement { $1 }
    | expression opt_semicolon { StcStatementsExpression $1 Nothing }
    | expression ';' statements { StcStatementsExpression $1 (Just $3) }

return_statement :: { StcStatements }
    : '^' expression opt_semicolon { StcStatementsReturn (StcReturnStatement $2) }

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
