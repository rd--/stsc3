module Language.Smalltalk.Ansi.Parser.Happy (module M, parseSmalltalkProgram) where

import Language.Smalltalk.Ansi.Ast
import Language.Smalltalk.Ansi.Parser.Happy.Lexer as M
import Language.Smalltalk.Ansi.Parser.Happy.Parser as M

-- | Parse and then pretty print Smalltalk program, using Alex/Happy parser.
parseSmalltalkProgram :: String -> SmalltalkProgram
parseSmalltalkProgram = M.smalltalkParser . M.alexScanTokens
