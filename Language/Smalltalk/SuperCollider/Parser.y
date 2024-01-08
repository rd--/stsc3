{
module Language.Smalltalk.SuperCollider.Parser where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.SuperCollider.Ast {- stsc3 -}
import           Language.Smalltalk.SuperCollider.Lexer {- stsc3 -}
import           Language.Smalltalk.SuperCollider.Token {- stsc3 -}
}

%name superColliderParserInitializerDefinition initializerdefinition
%name superColliderParserClassDefinitionSeq classdefinition_seq
%name superColliderParserClassExtensionSeq classextension_seq
%tokentype { Token }
%error { parseError }

%token
      '['             { LeftBracket }
      ']'             { RightBracket }
      '.'             { Dot }
      '..'            { DotDot }
      ','             { Comma }
      ';'             { SemiColon }
      ':'             { Colon }
      '|'             { VerticalBar }
      '{'             { LeftBrace }
      '}'             { RightBrace }
      '('             { LeftParen }
      ')'             { RightParen }
      '#['            { HashLeftBracket }

      nil             { NilIdentifier }
      true            { TrueIdentifier }
      false           { FalseIdentifier }
      self            { SelfIdentifier }
      arg             { Arg }
      var             { Var }
      classvar        { ClassVar }

      '='             { AssignmentOperator }
      '^'             { ReturnOperator }
      '+'             { ClassExtensionOperator }
      '*'             { ClassMethodOperator }

      identifier      { Identifier $$ }
      keyword         { Keyword $$ }
      binaryselector  { BinarySelector $$ }
      keywordselector { KeywordSelector $$ }
      float           { Float $$ }
      integer         { Integer $$ }
      quotedchar      { QuotedChar $$ }
      quotedstring    { QuotedString $$ }
      hashedstring    { HashedString $$ }
      comment         { Comment $$ }

%%

initializerdefinition :: { ScInitializerDefinition }
        : maybe_temporaries_seq
          maybe_statements                     { ScInitializerDefinition Nothing $1 $2 }

classextension_seq :: { [ScClassExtension] }
        : {- empty -}                          { [] }
        | classextension classextension_seq    { $1 : $2 }

classextension :: { ScClassExtension }
        : '+' identifier '{'
          methoddefinition_seq
          '}'                                  { ScClassExtension $2 $4 }

classdefinition_seq :: { [ScClassDefinition] }
        : {- empty -}                          { [] }
        | classdefinition classdefinition_seq  { $1 : $2 }

maybe_comment :: { Maybe String }
        : {- empty -}                          { Nothing }
        | comment                              { Just $1 }

classdefinition :: { ScClassDefinition }
        : maybe_comment
          identifier maybe_superclass '{'
          maybe_classvariables
          maybe_variables
          methoddefinition_seq
          '}'                                   { ScClassDefinition $2 $3 $6 $5 $7 Nothing $1 }

maybe_superclass :: { Maybe St.Identifier }
        : {- empty -}                           { Just "Object" }
        | ':' identifier                        { (if $2 == "nil" then Nothing else Just $2) }

methoddefinition_seq :: { [ScMethodDefinition] }
        : {- empty -}                           { [] }
        | methoddefinition methoddefinition_seq { $1 : $2 }

binaryoperator :: { String }
        : binaryselector                        { $1 }
        | '+'                                   { "+" }
        | '*'                                   { "*" }
        | '|'                                   { "|" }

methoddefinition :: { ScMethodDefinition }
        : '*' identifier '{'
          maybe_comment
          blockbody '}'                         { ScMethodDefinition True $2 $5 Nothing $4 }
        | identifier_or_binaryoperator_or_keywordselector
          '{' maybe_comment blockbody '}'       { ScMethodDefinition False $1 $4 Nothing $3 }

identifier_or_binaryoperator_or_keywordselector :: { String }
         : identifier                           { $1 }
         | binaryoperator                       { $1 }
         | keywordselector                      { $1 }

maybe_classvariables :: { Maybe [ScVariable] }
        : {- empty -}                          { Nothing }
        | classvariables                       { Just $1 }

classvariables :: { [ScVariable] }
        : classvar defaultvar_seq ';'          { $2 }

maybe_variables :: { Maybe [ScVariable] }
        : {- empty -}                          { Nothing }
        | variables                            { Just $1 }

variables :: { [ScVariable] }
        : var defaultvar_seq ';'               { $2 }

expression :: { ScExpression }
        : identifier '=' expression            { ScExprAssignment $1 $3 }
        | syntax_atput                         { ScExprBasic $1 }
        | basicexpression                      { ScExprBasic $1 }

syntax_atput :: { ScBasicExpression }
        : primary '[' basicexpression ']'
          '=' basicexpression                  { ScBasicExpression $1 (Just (scConstructDotMessage "at:put" [$3, $6])) }

basicexpression :: { ScBasicExpression }
        : primary maybe_messages               { ScBasicExpression $1 $2 }
        | dictionaryexpression maybe_messages  { ScBasicExpression $1 $2 }

dictionaryexpression :: { ScPrimary }
        : '(' keywordexpression_seq ')'       { ScPrimaryDictionaryExpression $2 }

keywordexpression_seq :: { [(St.Identifier, ScBasicExpression)] }
        : {- empty -}                          { [] }
        | nonemptykeywordexpression_seq        { $1 }

nonemptykeywordexpression_seq :: { [(St.Identifier, ScBasicExpression)] }
        : keywordexpression                    { [$1] }
        | keywordexpression
          ',' keywordexpression_seq            { $1 : $3 }

keywordexpression :: { (St.Identifier, ScBasicExpression) }
        : keyword basicexpression              { ($1, $2) }

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
        | '.' identifier blockexpression_seq   { ScDotMessage $2 $3}
        | '.' identifier message_param         { ScDotMessage $2 $3}
        | '.' identifier message_param blockexpression_seq { ScDotMessage $2 ($3 ++ $4) }
        | '.' identifier keywordmessage_param  { scDotMessageFromKeywordParam $2 $3 }
        | syntax_at                            { $1 }

blockexpression :: { ScBasicExpression }
        : '{' blockbody '}'                    { ScBasicExpression (ScPrimaryBlock $2) Nothing }

blockexpression_seq :: { [ScBasicExpression] }
        : blockexpression                         { [$1] }
        | blockexpression blockexpression_seq     { $1 : $2 }

syntax_at :: { ScDotMessage }
        : '[' basicexpression ']'              { ScDotMessage "at" [$2] }

message_param :: { [ScBasicExpression] }
        : '(' basicexpression_seq ')'          { $2 }

keywordmessage_param :: { (ScBasicExpression, [(St.Identifier, ScBasicExpression)]) }
        : '('
          basicexpression
          ','
          nonemptykeywordexpression_seq
          ')'                                  { ($2, $4) }

basicexpression_seq :: { [ScBasicExpression] }
        : basicexpression                         { [$1] }
        | basicexpression ',' basicexpression_seq { $1 : $3 }

maybe_binarymessage_seq :: { Maybe [ScBinaryMessage] }
        : {- empty -}                          { Nothing }
        | binarymessage_seq                    { Just $1 }

binarymessage_seq :: { [ScBinaryMessage] }
        : binarymessage binarymessage_seq      { $1 : $2 }
        | binarymessage                        { [$1] }

binarymessage :: { ScBinaryMessage }
        : binaryoperator binaryargument        { ScBinaryMessage $1 $2 }

binaryargument :: { ScBinaryArgument }
        : primary maybe_dotmessage_seq         { ScBinaryArgument $1 $2 }

primary :: { ScPrimary }
        : identifier                           { ScPrimaryIdentifier $1 }
        | reservedidentifier                   { ScPrimaryIdentifier $1 }
        | literal                              { ScPrimaryLiteral $1 }
        | '{' blockbody '}'                    { ScPrimaryBlock $2 }
        | '(' expression ')'                   { ScPrimaryExpression $2 }
        | '[' arrayexpression ']'              { ScPrimaryArrayExpression $2 }
	| '(' expression '..' expression ')'   { scIntervalRange $2 $4 }
	| '(' expression ',' expression '..' expression ')' { scFromThenTo $2 $4 $6 }
	| '[' expression '..' expression ']'   { scArrayRange $2 $4 }
        | identifier '(' arrayexpression ')'   { ScPrimaryImplicitMessageSend $1 $3 }
        | identifier
          '(' nonemptykeywordexpression_seq ')' { scPrimaryKeywordMessageSend (ScPrimaryIdentifier $1) $3 }

reservedidentifier :: { St.Identifier }
        : nil                                  { "nil" }
        | true                                 { "true" }
        | false                                { "false" }
        | self                                 { "self" }

arrayexpression :: { [ScBasicExpression] }
        : {- empty -}                          { [] }
        | basicexpression                      { [$1] }
        | basicexpression ',' arrayexpression  { $1 : $3 }

blockbody :: { ScBlockBody }
        : maybe_arguments
          maybe_temporaries_seq
          maybe_statements                     { ScBlockBody $1 $2 $3 }

maybe_arguments :: { Maybe [ScBlockArgument] }
        : {- empty -}                          { Nothing }
        | arguments                            { Just $1 }

arguments :: { [ScBlockArgument] }
        : arg defaultvar_seq ';'               { $2 }
        | '|' defaultvar_seq '|'               { $2 }

defaultvar_seq :: { [ScBlockArgument] }
        : defaultvar                           { [$1] }
        | defaultvar defaultvar_seq            { $1 : $2 }
        | defaultvar ',' defaultvar_seq        { $1 : $3 }

defaultvar :: { ScBlockArgument }
        :  identifier                          { ($1,Nothing) }
        |  identifier '=' literal              { ($1,Just $3) }

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
