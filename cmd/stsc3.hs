import Data.List {- base -}

import qualified Music.Theory.Directory.Find {- hmt-base -}
import qualified Music.Theory.Io {- hmt-base -}
import qualified Music.Theory.Opt as Opt {- hmt-base -}
import qualified Music.Theory.String as String {- hmt-base -}

import qualified Sound.Sc3.Common.Help as Help {- hsc3 -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Parser.Happy as St.Happy {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}

import qualified Language.Smalltalk.Ansi.Print.Som as Som {- stsc3 -}
import qualified Language.Smalltalk.FileOut as FileOut {- stsc3 -}
import qualified Language.Smalltalk.Som as Som {- stsc3 -}

import qualified Language.Smalltalk.Stc.Ast as Stc {- stsc3 -}
import qualified Language.Smalltalk.Stc.Ast.Print as Stc {- stsc3 -}
import qualified Language.Smalltalk.Stc.Lexer as Stc {- stsc3 -}
-- import qualified Language.Smalltalk.Stc.Ndef as Sc {- stsc3 -}

import qualified Language.Smalltalk.Stc.Parser as Stc {- stsc3 -}
import qualified Language.Smalltalk.Stc.Translate as Stc {- stsc3 -}

-- | Parse and then pretty print Smalltalk program, using Parsec parser.
st_cat_parsec :: String -> String
st_cat_parsec = St.smalltalkProgram_pp . St.stParse St.smalltalkProgram

-- | Parse and then pretty print Smalltalk program, using Alex/Happy parser.
st_cat_happy :: String -> String
st_cat_happy = St.smalltalkProgram_pp . St.Happy.parseSmalltalkProgram

-- | Parse and then pretty print Smalltalk program, using indicated parser.
st_cat :: String -> FilePath -> IO ()
st_cat which fn = do
  st <- readFile fn
  putStrLn ((if which == "parsec" then st_cat_parsec else st_cat_happy) st)

-- | Parse and then pretty print .stc program.
cat_fragments :: (String -> Stc.StcInitializerDefinition) -> FilePath -> IO ()
cat_fragments parse fn = do
  txt_fragments <- Help.read_file_fragments fn
  let expr_fragments = map parse txt_fragments
  mapM_ (putStrLn . Stc.stcInitializerDefinitionPrint) expr_fragments

stc_cat_fragments :: FilePath -> IO ()
stc_cat_fragments = cat_fragments Stc.stcParseToStc

spl_cat_fragments :: FilePath -> IO ()
spl_cat_fragments = cat_fragments Stc.splParseToStc

-- | Parse class library file, a sequence of class definitions.
stc_parse_class_definition_seq :: String -> [Stc.StcClassDefinition]
stc_parse_class_definition_seq = Stc.parseClassDefinitionSeq . Stc.alexScanTokens

{- | Read and print library.

stc_cat_library "/home/rohan/sw/stsc3/help/expr/library.sc"
stc_cat_library "/home/rohan/rd/j/2022-04-08/before-pim.sc"
-}
stc_cat_library :: FilePath -> IO ()
stc_cat_library fn =
  let rw = putStrLn . unlines . map Stc.stcClassDefinitionPrint . stc_parse_class_definition_seq
  in rw =<< readFile fn

-- | Parse class extensions file, a sequence of class extensions.
stc_parse_class_extension_seq :: String -> [Stc.StcClassExtension]
stc_parse_class_extension_seq = Stc.parseClassExtensionSeq . Stc.alexScanTokens

{- | Read and print extensions.

> stc_cat_extensions "/home/rohan/sw/stsc3/help/expr/extensions.sc"
> stc_cat_extensions "/home/rohan/sw/sc3-rdl/sc/Bag.ext.sc"
-}
stc_cat_extensions :: FilePath -> IO ()
stc_cat_extensions fn =
  let rw = putStrLn . unlines . map Stc.stcClassExtensionPrint . stc_parse_class_extension_seq
  in rw =<< readFile fn

{- | Read and re-print Som class definition file.

> som_cat "/home/rohan/opt/src/SOM-st/SOM/Smalltalk/Object.som"
-}
som_cat :: FilePath -> IO ()
som_cat fn =
  let rw = putStrLn . Som.classDefinitionPrintSom . Som.parseSomClassDefinition
  in rw =<< readFile fn

{- | Read Som class definition and write in FileOut format.

> cd_som_to_fileout True "/home/rohan/opt/src/SOM-st/SOM/Smalltalk/Array.som" "/dev/stdout"
-}
cd_som_to_fileout :: Bool -> FilePath -> FilePath -> IO ()
cd_som_to_fileout do_sort som_fn fileout_fn = do
  cd <- Som.somLoadClassDefinitionFromFile som_fn
  let cd' = if do_sort then St.classDefinitionSortMethods cd else cd
  writeFile fileout_fn (FileOut.fileOutClassDefinition cd')

tidy_method_source :: String -> String
tidy_method_source src =
  let ln = map String.delete_trailing_whitespace (lines src)
  in unlines (filter (not . null) ln)

{- | Read FileOut class definition and write in Som format.

> cd_fileout_to_som (True, True) "/home/rohan/rd/j/2022-05-04/Smalltalk-80/ArrayedCollection.st" "/dev/stdout"
-}
cd_fileout_to_som :: (Bool, Bool) -> FilePath -> FilePath -> IO ()
cd_fileout_to_som (do_sort, do_tidy) fileout_fn som_fn = do
  cd <- FileOut.fileOutLoadClassDefinitionFile fileout_fn
  let cd' = if do_sort then St.classDefinitionSortMethods cd else cd
      cd'' = if do_tidy then St.classDefinitionEditMethodSources tidy_method_source cd' else cd'
  writeFile som_fn (Som.classDefinitionPrintSom cd'')

cd_som_to_som :: (Bool, Bool) -> FilePath -> FilePath -> IO ()
cd_som_to_som (do_sort, do_tidy) input_fn output_fn = do
  cd <- Som.somLoadClassDefinitionFromFile input_fn
  let cd' = if do_sort then St.classDefinitionSortMethods cd else cd
      cd'' = if do_tidy then St.classDefinitionEditMethodSources tidy_method_source cd' else cd'
  writeFile output_fn (Som.classDefinitionPrintSom cd'')

ext_fileout_to_som :: (Bool, Bool) -> FilePath -> FilePath -> IO ()
ext_fileout_to_som (do_sort, do_tidy) fileout_fn som_fn = do
  (nm, mth) <- FileOut.fileOutLoadClassExtensionFile fileout_fn
  let cd = St.classDefinitionFromMethods (nm, Nothing, Nothing) mth
      cd' = if do_sort then St.classDefinitionSortMethods cd else cd
      cd'' = if do_tidy then St.classDefinitionEditMethodSources tidy_method_source cd' else cd'
  writeFile som_fn (Som.classDefinitionPrintSom cd'')

writeAllSomClassDef :: (Bool, Bool) -> FilePath -> FileOut.FileOutLibrary -> IO ()
writeAllSomClassDef (do_sort, do_tidy) som_dir lib = do
  let srt = if do_sort then St.classDefinitionSortMethods else id
      tidy = if do_tidy then St.classDefinitionEditMethodSources tidy_method_source else id
  mapM_ (Som.writeSomClassDefinition som_dir . tidy . srt) (FileOut.fileOutLibraryClassDefinitions lib)

lib_fileout_to_som :: (Bool, Bool) -> FilePath -> FilePath -> IO ()
lib_fileout_to_som opt fileout_fn som_dir = do
  lib <- FileOut.fileOutLoadPartial fileout_fn
  writeAllSomClassDef opt som_dir lib

cd_sort :: [St.ClassDefinition] -> [St.ClassDefinition]
cd_sort l =
  let g = St.classDefinitionsInheritanceGraph l
  in St.classDefinitionGraphSort g

dir_som_to_fileout :: String -> FilePath -> FilePath -> IO ()
dir_som_to_fileout cat som_dir fileout_fn = do
  all_fn <- Music.Theory.Directory.Find.dir_find_ext ".som" som_dir
  let ext_fn = filter (".ext." `isInfixOf`) all_fn
      mod_fn = filter (".mod." `isInfixOf`) all_fn
      cls_fn = sort (all_fn \\ (ext_fn ++ mod_fn))
      readCd fn = Som.somLoadClassDefinitionFromFile fn
      readMd fn = Som.somLoadClassExtensionOfModificationFromFile fn
  cls_cd <- mapM readCd cls_fn
  ext_md <- mapM readMd ext_fn
  mod_md <- mapM readMd mod_fn
  let cls_fo = unlines (map FileOut.fileOutClassDefinition (map (St.classDefinitionSetCategory (Just cat)) (cd_sort cls_cd)))
      mth_fo = unlines (map FileOut.fileOutMethodDefinition (concat ext_md ++ concat mod_md))
  writeFile fileout_fn (cls_fo ++ mth_fo)

{-
-- | Fragment input file and run stcToJs at each fragment.
stc_to_js :: FilePath -> IO ()
stc_to_js fn = do
  txt_fragments <- Help.read_file_fragments fn
  mapM_ (putStrLn . Stc.stcToJs) txt_fragments
-}

opt_def :: [Opt.OptUsr]
opt_def =
  [ ("category", "Uncategorised", "string", "set class category")
  , ("sort", "False", "bool", "sort methods in class")
  , ("tidy", "False", "bool", "tidy method source")
  ]

help :: [String]
help =
  [ "stsc3 command [arguments]"
  , " rewrite ndef"
  , " som cat <som-file>"
  , " { spl | stc } cat { fragment | library | extensions } <supercollider-file...>"
  , " st cat { parsec | happy } <smalltalk-file...>"
  , " translate class [opt] { fileout | som } { fileout | som } <input-file> <output-file>"
  , " translate directory [opt] som fileout <input-directory> <output-file>"
  , " translate library [opt] fileout som <input-file> <output-directory>"
  , " translate [ stream ] { spl | stc } { js | sc | scm | st | ast } [ <input-file> <output-file> ]"
  ]

main :: IO ()
main = do
  (o, a) <- Opt.opt_get_arg True help opt_def
  let trs in_ty out_ty =
        case (in_ty, out_ty) of
          ("spl", "st") -> Stc.splToSt
          ("stc", "st") -> Stc.stcToSt
          ("stc", "js") -> Stc.stcToJs (Just "sc.") -- Nothing
          ("stc", "stc") -> Stc.stcToStc
          ("stc", "scm") -> Stc.stcToScheme
          ("stc", "ast") -> Stc.stcToAst
          _ -> error "stsc3: unknown translation"
  case a of
    -- ["rewrite", "ndef"] -> interact Stc.stcUgenToNdef
    "som" : "cat" : fn_seq ->
      mapM_
        ( \fn ->
            putStrLn fn >> som_cat fn
        )
        fn_seq
    src : "cat" : "fragment" : fn_seq ->
      mapM_
        ( \fn ->
            case src of
              "stc" -> putStrLn fn >> stc_cat_fragments fn
              "spl" -> putStrLn fn >> spl_cat_fragments fn
              _ -> error "stsc3: unknown source"
        )
        fn_seq
    "stc" : "cat" : "library" : fn_seq ->
      mapM_
        ( \fn ->
            putStrLn fn >> stc_cat_library fn
        )
        fn_seq
    "stc" : "cat" : "extensions" : fn_seq ->
      mapM_
        ( \fn ->
            putStrLn fn >> stc_cat_extensions fn
        )
        fn_seq
    "st" : "cat" : which : fn_seq ->
      mapM_
        ( \fn ->
            putStrLn fn >> st_cat which fn
        )
        fn_seq
    ["translate", in_ty, out_ty] ->
      interact (trs in_ty out_ty)
    ["translate", in_ty, out_ty, inFile, outFile] ->
      Music.Theory.Io.interactWithFiles inFile outFile (trs in_ty out_ty)
    ["translate", "class", "fileout", "som", fileout_fn, som_fn] ->
      cd_fileout_to_som (Opt.opt_read o "sort", Opt.opt_read o "tidy") fileout_fn som_fn
    ["translate", "class", "som", "fileout", som_fn, fileout_fn] ->
      cd_som_to_fileout (Opt.opt_read o "sort") som_fn fileout_fn
    ["translate", "class", "som", "som", input_fn, output_fn] ->
      cd_som_to_som (Opt.opt_read o "sort", Opt.opt_read o "tidy") input_fn output_fn
    ["translate", "directory", "som", "fileout", som_dir, fileout_fn] ->
      dir_som_to_fileout (Opt.opt_get o "category") som_dir fileout_fn
    ["translate", "extensions", "fileout", "som", fileout_fn, som_fn] ->
      ext_fileout_to_som (Opt.opt_read o "sort", Opt.opt_read o "tidy") fileout_fn som_fn
    ["translate", "library", "fileout", "som", fileout_fn, som_dir] ->
      lib_fileout_to_som (Opt.opt_read o "sort", Opt.opt_read o "tidy") fileout_fn som_dir
    ["translate", "stream", in_ty, out_ty] ->
      Music.Theory.Io.interactWithStdio (trs in_ty out_ty)
    _ ->
      putStrLn (unlines help)
