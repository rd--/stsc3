module Language.Smalltalk.Organisation where

import Data.Function {- base -}
import Data.List {- base -}

import Language.Smalltalk.Ansi {- stsc3 -}

classLibraryAllCategories :: ClassLibrary -> [String]
classLibraryAllCategories = sort . nub . map classCategoryOrError

classLibraryHierarchy :: ClassLibrary -> [(String, [String])]
classLibraryHierarchy cl =
  let cp = map classCategoryPartsOrError cl
      grp = groupBy ((==) `on` fst)
      jn l = (fst (head l), map snd l)
      collate = map jn . grp
  in collate (nub (sort cp))

classLibraryCategory :: ClassLibrary -> String -> [ClassDefinition]
classLibraryCategory lib cat = filter ((== cat) . classCategoryOrError) lib
