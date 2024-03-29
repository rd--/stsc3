-- | Print Ansi class definition value in Som format.
module Language.Smalltalk.Ansi.Print.Som {- stsc3 -} where

import Data.Maybe {- base -}

import System.FilePath {- system -}

import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.String as String {- hmt-base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}

somVariablesPrint :: [St.Identifier] -> String
somVariablesPrint var = if null var then "" else St.temporaries_pp (St.Temporaries var)

{- | Print class definition in Som format.

If there is a class comment provide it as a comment class method returning a String.
If methods have source code print that rather than pretty-printing the method definition.
-}
classDefinitionPrintSom :: St.ClassDefinition -> String
classDefinitionPrintSom cd =
  let cv = St.classVariableNames cd
      cm = St.classMethods cd
      cmt txt = unlines ["comment = (^'", txt, "')"]
      ln =
        [ unwords [St.className cd, "=", fromMaybe (if St.className cd == "Object" then "nil" else "Object") (St.superclassName cd), "("]
        , somVariablesPrint (St.classInstanceVariableNames cd)
        , unlines (map methodDefinitionPrintSom (St.instanceMethods cd))
        , if null cv && null cm
            then ""
            else
              unlines
                [ "----------------------------"
                , somVariablesPrint cv
                , unlines (map methodDefinitionPrintSom cm)
                , cmt (fromMaybe "" (St.classComment cd))
                ]
        , ")"
        ]
  in unlines (filter (not . null) ln)

{- | Print method definition in Som format.

If methods have source code print that rather than pretty-printing the definition, also for primitives.
-}
methodDefinitionPrintSom :: St.MethodDefinition -> String
methodDefinitionPrintSom md =
  let St.MethodDefinition _ _ pat tmp stm prm _ src = md
      ln =
        [ unwords [St.pattern_pp pat, "=", "("]
        , case src of
            Just txt -> String.unlinesNoTrailingNewline (List.tail_err (lines txt))
            Nothing ->
              String.unlinesNoTrailingNewline
                [ maybe "" St.temporaries_pp tmp
                , maybe "" St.statements_pp stm
                ]
        , ")"
        ]
  in case (prm, src) of
      (Just _, Nothing) -> St.pattern_pp pat ++ " = primitive"
      _ -> unlines (filter (not . null) ln)

writeSomClassDefinition :: FilePath -> St.ClassDefinition -> IO ()
writeSomClassDefinition dir cd = writeFile (dir </> St.className cd <.> "som") (classDefinitionPrintSom cd)
