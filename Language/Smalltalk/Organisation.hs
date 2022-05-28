{- | Class organisation.

Classes and methods each reside in categories.
Class categories are (by convention) of the form "Major-Minor" forming a two-level hierarchy.
-}
module Language.Smalltalk.Organisation where

import Data.Function {- base -}
import Data.List {- base -}

import Language.Smalltalk.Ansi {- stsc3 -}

-- | Set of all categories in library.
classLibraryAllCategories :: ClassLibrary -> [String]
classLibraryAllCategories = sort . nub . map classCategoryOrError

-- | Set of all categories in library organised as (Major,[Minor]).
classLibraryCategoryHierarchy :: ClassLibrary -> [(String, [String])]
classLibraryCategoryHierarchy cl =
  let cp = map classCategoryPartsOrError cl
      grp = groupBy ((==) `on` fst)
      jn l = (fst (head l), map snd l)
      collate = map jn . grp
  in collate (nub (sort cp))

-- | Select class definitions by category.
classLibraryCategory :: ClassLibrary -> String -> [ClassDefinition]
classLibraryCategory lib cat = filter ((== cat) . classCategoryOrError) lib
