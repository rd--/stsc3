{
module Language.Smalltalk.Ansi.Parser.Happy.Parser where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

import Language.Smalltalk.Ansi.Parser.Happy.Lexer {- stsc3 -}
import Language.Smalltalk.Ansi.Parser.Happy.Token {- stsc3 -}
}

%name smalltalkParser
%tokentype { Token }
%error { parseError }

%token
      '|'             { VerticalBar }
      '['             { LeftBracket }
      ']'             { RightBracket }
      '.'             { Dot }
      ':'             { Colon }
      ';'             { SemiColon }
      '{'             { LeftBrace }
      '}'             { RightBrace }
      '#'             { Hash }
      '#('            { HashLeftParen }
      '('             { LeftParen }
      ')'             { RightParen }
      '<'             { LeftAngleBracket }
      '>'             { RightAngleBracket }

      nil             { NilIdentifier }
      true            { TrueIdentifier }
      false           { FalseIdentifier }
      self            { SelfIdentifier }
      super           { SuperIdentifier }

      identifier      { Identifier $$ }
      keyword         { Keyword $$ }
      binaryselector  { BinarySelector $$ }
      '^'             { ReturnOperator }
      ':='            { AssignmentOperator }
      float           { Float $$ }
      integer         { Integer $$ }
      quotedchar      { QuotedChar $$ }
      quotedstring    { QuotedString $$ }
      hashedstring    { HashedString $$ }


%%

{-
smalltalkprogram :: { St.SmalltalkProgram }
        : programelements                      { St.SmalltalkProgram $1 }

programelements :: { [St.ProgramElement] }
        : programelement                       { [$1] }
        | programelement programelements       { $1 : $2 }

programelement :: { St.ProgramElement }
        : globaldefinition                     { St.ProgramGlobal $1 }
        | programinitializerdefinition         { St.ProgramInitializer $1 }

programinitializerdefinition :: { St.ProgramInitializerDefinition }
        : initializerdefinition                { $1 }

globaldefinition :: { St.GlobalDefinition }
        : identifier
          maybe_variableinitializer            { St.GlobalDefinition $1 $2 }

maybe_variableinitializer :: { Maybe St.VariableInitializer }
        : {- empty -}                          { Nothing }
        | initializerdefinition                { Just $1 }

maybe_initializerdefinition :: { Maybe St.InitializerDefinition }
        : {- empty -}                          { Nothing }
        | initializerdefinition                { Just $1 }
-}

smalltalkprogram :: { St.SmalltalkProgram }
        : initializerdefinition                { St.SmalltalkProgram [St.ProgramInitializer $1] }

initializerdefinition :: { St.InitializerDefinition }
        : maybe_temporaries maybe_statements   { St.InitializerDefinition Nothing $1 $2 }

expression :: { St.Expression }
        : assignment                           { St.ExprAssignment $1 }
        | basicexpression                      { St.ExprBasic $1 }

assignment :: { St.Assignment }
        : identifier ':=' expression           { St.Assignment $1 ($3) }

basicexpression :: { St.BasicExpression }
        : primary maybe_messages
                  maybe_cascadedmessages       { St.BasicExpression $1 $2 $3 }

maybe_messages :: { Maybe St.Messages }
        : {- empty -}                          { Nothing }
        | messages                             { Just $1 }

messages :: { St.Messages }
        : unarymessage_seq
          maybe_binarymessage_seq
          maybe_keywordmessage                 { St.MessagesUnary $1 $2 $3 }
        | binarymessage_seq
          maybe_keywordmessage                 { St.MessagesBinary $1 $2 }
        | keywordmessage                       { St.MessagesKeyword $1 }

maybe_cascadedmessages :: { Maybe St.CascadedMessages }
        : {- empty -}                          { Nothing }
        | cascadedmessages                     { Just $1 }

cascadedmessages :: { St.CascadedMessages }
        : ';' messages                         { [$2] }
        | ';' messages cascadedmessages        { $2 : $3 }


maybe_unarymessage_seq :: { Maybe [St.UnaryMessage] }
        : {- empty -}                          { Nothing }
        | unarymessage_seq                     { Just $1 }

unarymessage_seq :: { [St.UnaryMessage] }
        : unarymessage unarymessage_seq        { $1 : $2 }
        | unarymessage                         { [$1] }

unarymessage :: { St.UnaryMessage }
        : identifier                           { St.UnaryMessage $1 }

maybe_binarymessage_seq :: { Maybe [St.BinaryMessage] }
        : {- empty -}                          { Nothing }
        | binarymessage_seq                    { Just $1 }

binarymessage_seq :: { [St.BinaryMessage] }
        : binarymessage binarymessage_seq      { $1 : $2 }
        | binarymessage                        { [$1] }

binaryoperator :: { St.Identifier }
        : binaryselector                       { $1 }
        | '|'                                  { "|" }
        | '<'                                  { "<" }
        | '>'                                  { ">" }

binarymessage :: { St.BinaryMessage }
        : binaryoperator binaryargument        { St.BinaryMessage $1 $2 }

binaryargument :: { St.BinaryArgument }
        : primary maybe_unarymessage_seq       { St.BinaryArgument $1 $2 }

maybe_keywordmessage :: { Maybe St.KeywordMessage }
        : {- empty -}                          { Nothing }
        | keywordmessage                       { Just $1 }

keywordmessage :: { St.KeywordMessage }
        : keywordparam_seq                     { St.KeywordMessage $1 }

keywordparam_seq :: { [(St.Keyword,St.KeywordArgument)] }
        : keywordparam                         { [$1] }
        | keywordparam keywordparam_seq        { $1 : $2 }

keywordparam :: { (St.Keyword,St.KeywordArgument) }
        : keyword keywordargument              { ($1,$2) }

keywordargument :: { St.KeywordArgument }
        : primary
          maybe_unarymessage_seq
          maybe_binarymessage_seq              { St.KeywordArgument $1 $2 $3 }

primary :: { St.Primary }
        : identifier                           { St.PrimaryIdentifier $1 }
        | reserveridentifier                   { St.PrimaryIdentifier $1 }
        | literal                              { St.PrimaryLiteral $1 }
        | '[' blockbody ']'                    { St.PrimaryBlock $2 }
        | '(' expression ')'                   { St.PrimaryExpression $2 }
        | '{' arrayexpression '}'              { St.PrimaryArrayExpression $2 }

reserveridentifier :: { St.Identifier }
        : nil                                  { "nil" }
        | true                                 { "true" }
        | false                                { "false" }
        | self                                 { "self" }
        | super                                { "super" }

arrayexpression :: { [St.BasicExpression] }
        : basicexpression '.' arrayexpression  { $1 : $3 }
        | basicexpression                      { [$1] }

blockbody :: { St.BlockBody }
        : maybe_blockarguments
          maybe_temporaries
          maybe_statements                     { St.BlockBody Nothing $1 $2 $3 }

maybe_blockarguments :: { Maybe [St.BlockArgument] }
        : {- empty -}                          { Nothing }
        | blockargument_seq '|'                { Just $1 }

blockargument_seq :: { [St.BlockArgument] }
        : blockargument                        { [$1] }
        | blockargument blockargument_seq      { $1 : $2 }

blockargument :: { St.BlockArgument }
        : ':' identifier                       { $2 }

maybe_temporaries :: { Maybe St.Temporaries }
        : {- empty -}                          { Nothing }
        | '|' identifier_seq '|'               { Just (St.Temporaries $2) }

identifier_seq :: { [St.Identifier] }
        : identifier                           { [$1] }
        | identifier identifier_seq            { $1 : $2 }

maybe_statements :: { Maybe St.Statements }
        : {- empty -}                           { Nothing }
        | statements                            { Just $1 }

statements :: { St.Statements }
        : returnstatement                       { $1 }
        | expression optdot                     { St.StatementsExpression $1 Nothing }
        | expression '.' statements             { St.StatementsExpression $1 (Just $3) }

returnstatement :: { St.Statements }
        : '^' expression optdot                 { St.StatementsReturn (St.ReturnStatement $2) }

literal :: { St.Literal }
        : integer                               { St.NumberLiteral (St.Int $1) }
        | float                                 { St.NumberLiteral (St.Float $1) }
        | quotedstring                          { St.StringLiteral $1 }
        | quotedchar                            { St.CharacterLiteral $1 }
        | '#(' maybe_arrayliteral ')'           { St.ArrayLiteral $2 }
        | hashedstring                          { St.SymbolLiteral $1 }
        | selectorliteral                       { St.SelectorLiteral $1 }

selectorliteral :: { St.Selector }
        : '#' identifier                        { St.UnarySelector $2 }
        | '#' binaryoperator                    { St.BinarySelector $2 }
        | '#' keyword                           { St.asKeywordSelector $2 }

maybe_arrayliteral :: { [Either St.Literal St.Identifier] }
        : {- empty -}                           { [] }
        | arrayliteral                          { $1 }

arrayliteral :: { [Either St.Literal St.Identifier] }
        : arrayliteral_elem                     { [$1] }
        | arrayliteral_elem arrayliteral        { $1 : $2 }

arrayliteral_elem :: { Either St.Literal St.Identifier }
        : literal                               { Left $1 }
        | '(' maybe_arrayliteral ')'            { Left (St.ArrayLiteral $2) }
        | reserveridentifier                    { Right $1 }

optdot :: { () }
        : {- empty -}                           { () }
        | '.'                                   { () }

{
parseError :: [Token] -> a
parseError t = error ("Parse error: " ++ show t)
}
