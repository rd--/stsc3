-- | Print Ansi class definition value in Som format.
module Language.Smalltalk.Ansi.Print.Som where {- stsc3 -}

import Data.Maybe {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}

somVariablesPrint :: [St.Identifier] -> String
somVariablesPrint var = if null var then "" else St.temporaries_pp (St.Temporaries var)

classDefinitionPrintSom :: St.ClassDefinition -> String
classDefinitionPrintSom cd =
  let cv = St.classVariableNames cd
      cm = St.classMethods cd
      ln =
        [unwords [St.className cd, "=", fromMaybe (if St.className cd == "Object" then "nil" else "Object") (St.superclassName cd), "("]
        ,somVariablesPrint (St.classInstanceVariableNames cd)
        ,unlines (map methodDefinitionPrintSom (St.instanceMethods cd))
        ,if null cv && null cm
         then ""
         else unlines ["----------------------------"
                      ,somVariablesPrint cv
                      ,unlines (map methodDefinitionPrintSom cm)]
        ,")"]
  in unlines (filter (not . null) ln)

methodDefinitionPrintSom :: St.MethodDefinition -> String
methodDefinitionPrintSom md =
  let St.MethodDefinition _ _ pat tmp stm prm _ src = md
      ln =
        [unwords [St.pattern_pp pat, "=", "("]
        ,case src of
            Just txt -> unlines (tail (lines txt))
            Nothing -> unlines
              [maybe "" St.temporaries_pp tmp
              ,maybe "" St.statements_pp stm]
        ,")"]
  in case (prm, src) of
       (Just _, Nothing) -> St.pattern_pp pat ++ " = primitive"
       _ -> unlines (filter (not . null) ln)
