module Language.Smalltalk.Ansi.Print.Som where {- stsc3 -}

import Data.Maybe {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}

somVariablesPrint :: [St.Identifier] -> String
somVariablesPrint var = if null var then "" else St.temporaries_pp (St.Temporaries var)

classDefinitionPrintSom :: St.ClassDefinition -> String
classDefinitionPrintSom cd =
  let ln =
        [unwords [St.className cd, "=", fromMaybe "Object" (St.superclassName cd), "("]
        ,somVariablesPrint (St.classInstanceVariableNames cd)
        ,unlines (map methodDefinitionPrintSom (St.instanceMethods cd))
        ,"----------------------------"
        ,somVariablesPrint (St.classVariableNames cd)
        ,unlines (map methodDefinitionPrintSom (St.classMethods cd))
        ,")"]
  in unlines (filter (not . null) ln)

methodDefinitionPrintSom :: St.MethodDefinition -> String
methodDefinitionPrintSom (St.MethodDefinition _ _ pat tmp stm _ _) =
  let ln =
        [unwords [St.pattern_pp pat, "=", "("]
        ,maybe "" St.temporaries_pp tmp
        ,maybe "" St.statements_pp stm
        ,")"]
  in unlines (filter (not . null) ln)

