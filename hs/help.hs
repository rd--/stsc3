{-
read .st graphs (initial fragment) from files at help/graph and write Sc3-Help-Graphs.st.
to avoid pretty printing this just copies the help text, prepending a return mark to the last line.
this means the last line must be a simple expression.
-}

import Data.Char {- base -}
import Data.List {- base -}

import Data.List.Split {- split -}

import System.FilePath {- filepath -}

import Music.Theory.Directory {- hmt-base -}

methods_prefix = "!Sc3HelpGraph methodsFor: 'Sc Help Graphs'!"

-- > file_name_to_method_name "f0-tw-1395878538297892865.st" == "f0Tw1395878538297892865"
file_name_to_method_name fn =
  let p1:p = splitOn "-" (dropExtension fn)
      capitalise x = toUpper (head x) : tail x
  in concat (p1 : map capitalise p)

-- > putStrLn $ runner_text "f0Tw1395878538297892865"
runner_text nm =
  ["\""
  ,"Sc3HelpGraph new " ++ nm ++ " play."
  ,"Sc3 reset."
  ,"\""]

indent_text_by prefix = map (prefix ++) . lines

stsc3_dir = "/home/rohan/sw/stsc3/"
graph_dir = stsc3_dir ++ "help/graph/"

proc_file_text fn ln =
  let cm = ln !! 0
      bdy = drop 1 (init ln)
      ret = last ln
      nm = file_name_to_method_name fn
      pr = cm : concat [runner_text nm, bdy, [ '^' : ret]]
  in unlines (nm : map ("  " ++) pr)

-- > proc_file "f0-tw-1395878538297892865.st" >>= putStrLn
proc_file fn = do
  txt <- readFile (graph_dir ++ fn)
  let frg = takeWhile (/= "") (lines txt)
  return (proc_file_text fn frg)

main = do
  fn_list <- dir_subset_rel [".st"] graph_dir
  mth <- mapM proc_file fn_list
  writeFile (stsc3_dir ++ "st/Sc3HelpGraph.ext.st") (unlines [methods_prefix, intercalate "!\n" mth, "!\n!"])
