{
module Language.Smalltalk.SuperCollider.Parser where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.SuperCollider.Ast {- stsc3 -}
import           Language.Smalltalk.SuperCollider.Lexer {- stsc3 -}
import           Language.Smalltalk.SuperCollider.Token {- stsc3 -}
}

%name superColliderParser
%tokentype { Token }
%error { parseError }

%token
      '['             { LeftBracket }
      ']'             { RightBracket }
      '.'             { Dot }
      ','             { Comma }
      ';'             { SemiColon }
      '{'             { LeftBrace }
      '}'             { RightBrace }
      '('             { LeftParen }
      ')'             { RightParen }
      '#['            { HashLeftBracket }

      nil             { NilIdentifier }
      true            { TrueIdentifier }
      false           { FalseIdentifier }
      this            { ThisIdentifier }
      arg             { Arg }
      var             { Var }

      '='             { AssignmentOperator }
      '^'             { ReturnOperator }

      identifier      { Identifier $$ }
      narymessagename { NaryMessageName $$ }
      keyword         { Keyword $$ }
      binaryselector  { BinarySelector $$ }
      float           { Float $$ }
      integer         { Integer $$ }
      quotedchar      { QuotedChar $$ }
      quotedstring    { QuotedString $$ }
      hashedstring    { HashedString $$ }

%%

initializerdefinition :: { ScInitializerDefinition }
        : maybe_temporaries_seq
          maybe_statements                     { ScInitializerDefinition Nothing $1 $2 }

expression :: { ScExpression }
        : identifier '=' expression            { ScExprAssignment $1 $3 }
        | basicexpression                      { ScExprBasic $1 }

basicexpression :: { ScBasicExpression }
        : primary maybe_messages               { ScBasicExpression $1 $2 }

maybe_messages :: { Maybe ScMessages }
        : {- empty -}                          { Nothing }
        | messages                             { Just $1 }

messages :: { ScMessages }
        : dotmessage_seq
          maybe_binarymessage_seq              { ScMessagesDot $1 $2 }
        | binarymessage_seq                    { ScMessagesBinary $1 }

maybe_dotmessage_seq :: { Maybe [ScDotMessage] }
        : {- empty -}                          { Nothing }
        | dotmessage_seq                       { Just $1 }

dotmessage_seq :: { [ScDotMessage] }
        : dotmessage dotmessage_seq            { $1 : $2 }
        | dotmessage                           { [$1] }

dotmessage :: { ScDotMessage }
        : '.' identifier                       { ScDotMessage $2 []}
        | '.' identifier message_param         { ScDotMessage $2 $3}
        | '.' narymessagename message_param    { ScDotMessage $2 $3}

message_param :: { [ScKeywordArgument] }
        : '(' keywordargument_seq ')'          { $2 }

keywordargument_seq :: { [ScKeywordArgument] }
        : keywordargument                      { [$1] }
        | keywordargument ','
          keywordargument_seq                  { $1 : $3 }

keywordargument :: { ScKeywordArgument }
        : maybe_keyword basicexpression        { ScKeywordArgument $1 $2 }

maybe_keyword :: { Maybe St.Keyword }
        : {- empty -}                          { Nothing }
        | keyword                              { Just $1 }

maybe_binarymessage_seq :: { Maybe [ScBinaryMessage] }
        : {- empty -}                          { Nothing }
        | binarymessage_seq                    { Just $1 }

binarymessage_seq :: { [ScBinaryMessage] }
        : binarymessage binarymessage_seq      { $1 : $2 }
        | binarymessage                        { [$1] }

binarymessage :: { ScBinaryMessage }
        : binaryselector binaryargument        { ScBinaryMessage $1 $2 }

binaryargument :: { ScBinaryArgument }
        : primary maybe_dotmessage_seq         { ScBinaryArgument $1 $2 }

primary :: { ScPrimary }
        : identifier                           { ScPrimaryIdentifier $1 }
        | reservedidentifier                   { ScPrimaryIdentifier $1 }
        | literal                              { ScPrimaryLiteral $1 }
        | '{' blockbody '}'                    { ScPrimaryBlock $2 }
        | '(' expression ')'                   { ScPrimaryExpression $2 }
        | '[' arrayexpression ']'              { ScPrimaryArrayExpression $2 }
        | identifier '(' arrayexpression ')'   { ScPrimaryImplictMessageSend $1 $3 }

reservedidentifier :: { St.Identifier }
        : nil                                  { "nil" }
        | true                                 { "true" }
        | false                                { "false" }
        | this                                 { "this" }

arrayexpression :: { [ScBasicExpression] }
        : {- empty -}                          { [] }
        | basicexpression                      { [$1] }
        | basicexpression ',' arrayexpression  { $1 : $3 }

blockbody :: { ScBlockBody }
        : maybe_blockarguments
          maybe_temporaries_seq
          maybe_statements                     { ScBlockBody $1 $2 $3 }

maybe_blockarguments :: { Maybe [St.BlockArgument] }
        : {- empty -}                          { Nothing }
        | arg blockargument_seq ';'            { Just $2 }

blockargument_seq :: { [St.BlockArgument] }
        : blockargument                        { [$1] }
        | blockargument ',' blockargument_seq  { $1 : $3 }

blockargument :: { St.BlockArgument }
        : identifier                           { $1 }

maybe_temporaries_seq :: { Maybe [ScTemporaries] }
        : {- empty -}                          { Nothing }
        | temporaries_seq                      { Just $1 }

temporaries_seq :: { [ScTemporaries] }
        : temporaries                          { [$1] }
        | temporaries temporaries_seq          { $1 : $2 }

temporaries :: { ScTemporaries }
        : var temporary_seq ';'                { $2 }

temporary_seq :: { ScTemporaries }
        : temporary                            { [$1] }
        | temporary ',' temporary_seq          { $1 : $3 }

temporary :: { ScTemporary }
        :  identifier                          { ($1,Nothing) }
        |  identifier '=' basicexpression      { ($1,Just $3) }

maybe_statements :: { Maybe ScStatements }
        : {- empty -}                           { Nothing }
        | statements                            { Just $1 }

statements :: { ScStatements }
        : returnstatement                       { $1 }
        | expression optsemicolon               { ScStatementsExpression $1 Nothing }
        | expression ';' statements             { ScStatementsExpression $1 (Just $3) }

returnstatement :: { ScStatements }
        : '^' expression optsemicolon           { ScStatementsReturn (ScReturnStatement $2) }

literal :: { St.Literal }
        : integer                               { St.NumberLiteral (St.Int $1) }
        | float                                 { St.NumberLiteral (St.Float $1) }
        | quotedstring                          { St.StringLiteral $1 }
        | quotedchar                            { St.CharacterLiteral $1 }
        | hashedstring                          { St.SymbolLiteral $1 }
        | '#[' arrayliteral ']'                 { St.ArrayLiteral $2 }

arrayliteral :: { [Either St.Literal St.Identifier] }
        : {- empty -}                           { [] }
        | arrayliteral_elem                     { [$1] }
        | arrayliteral_elem ',' arrayliteral    { $1 : $3 }

arrayliteral_elem :: { Either St.Literal St.Identifier }
        : literal                               { Left $1 }
        | reservedidentifier                    { Right $1 }

optsemicolon :: { () }
        : {- empty -}                           { () }
        | ';'                                   { () }

{
parseError :: [Token] -> a
parseError t = error ("Parse error: " ++ show t)
}
