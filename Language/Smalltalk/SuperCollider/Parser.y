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
      ','             { Comma }
      ';'             { SemiColon }
      ':'             { Colon }
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
      classvar        { ClassVar }

      '='             { AssignmentOperator }
      '^'             { ReturnOperator }
      '+'             { ClassExtensionOperator }
      '*'             { ClassMethodOperator }

      identifier      { Identifier $$ }
      keyword         { Keyword $$ }
      narymessagename { NaryMessageName $$ }
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

classdefinition :: { ScClassDefinition }
        : identifier maybe_superclass '{'
          maybe_classvariables
          maybe_variables
          methoddefinition_seq
          '}'                                   { ScClassDefinition $1 $2 $4 $5 $6 }


maybe_superclass :: { Maybe St.Identifier }
        : {- empty -}                           { Nothing }
        | ':' identifier                        { Just $2 }

methoddefinition_seq :: { [ScMethodDefinition] }
        : {- empty -}                           { [] }
        | methoddefinition methoddefinition_seq { $1 : $2 }

binaryoperator :: { String }
        : binaryselector                        { $1 }
        | '+'                                   { "+" }
        | '*'                                   { "*" }

methoddefinition :: { ScMethodDefinition }
        : '*' identifier '{' blockbody '}'     { ScMethodDefinition True $2 $4 }
        | identifier_or_binaryoperator_or_narymessagename
          '{' blockbody '}'                    { ScMethodDefinition False $1 $3 }


identifier_or_binaryoperator_or_narymessagename :: { String }
        : identifier                           { $1 }
        | binaryoperator                       { $1 }
        | narymessagename                      { $1 }

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
          '=' basicexpression                  { ScBasicExpression $1 (Just (ScMessagesDot [ScDotMessage "at:put" [$3, $6]] Nothing)) }

basicexpression :: { ScBasicExpression }
        : primary maybe_messages               { ScBasicExpression $1 $2 }
        | syntax_dictionary maybe_messages     { ScBasicExpression (scBasicExpressionToPrimary $1) $2 }

syntax_dictionary :: { ScBasicExpression }
        : '(' syntax_association_seq ')'       { ScBasicExpression
                                                  (ScPrimaryIdentifier "Dictionary")
                                                  (Just (ScMessagesDot [ScDotMessage "newFromPairs"
                                                    [ScBasicExpression (ScPrimaryArrayExpression $2) Nothing]] Nothing)) }

syntax_association_seq :: { [ScBasicExpression] }
        : {- empty -}                          { [] }
        | syntax_association                   { $1 }
        | syntax_association ',' syntax_association_seq { $1 ++ $3 }

syntax_association :: { [ScBasicExpression] }
        : keyword basicexpression              { [ScBasicExpression (ScPrimaryLiteral (St.SymbolLiteral $1)) Nothing, $2] }

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
        | syntax_at                            { $1 }

syntax_at :: { ScDotMessage }
        : '[' basicexpression ']'              { ScDotMessage "at" [$2] }

message_param :: { [ScBasicExpression] }
        : '(' basicexpression_seq ')'          { $2 }

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
        | identifier '(' arrayexpression ')'   { ScPrimaryImplicitMessageSend $1 $3 }

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
        : maybe_arguments
          maybe_temporaries_seq
          maybe_statements                     { ScBlockBody $1 $2 $3 }


maybe_arguments :: { Maybe [ScBlockArgument] }
        : {- empty -}                          { Nothing }
        | arguments                            { Just $1 }

arguments :: { [ScBlockArgument] }
        : arg defaultvar_seq ';'               { $2 }

defaultvar_seq :: { [ScBlockArgument] }
        : defaultvar                           { [$1] }
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
