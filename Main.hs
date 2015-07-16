module Main(main) where

import Data.List as L
import Language.Preprocessor.Cpphs
import System.IO

main = do
  fileContents <- readFile filePath
  preprocessed <- cppIfdef errorFilePath [("BITS_PER_LONG", "64")] ["/Users/dillon/linux/linux/include/"] boolOptions fileContents
  putStrLn $ L.concatMap (\(posn, l) -> l ++ "\n") preprocessed
--  runCpphs cppOptions filePath "/Users/dillon/Haskell/Bugs/CAnalysis/error_file.txt"

errorFilePath = "/Users/dillon/Haskell/Bugs/CAnalysis/error_file.txt"
filePath = "/Users/dillon/linux/linux/kernel/workqueue.c"

cppOptions =
  CpphsOptions [] [] [] ["/Users/dillon/linux/linux/include/"] [] defaultBoolOptions

boolOptions =
  BoolOptions
        False   -- leave define out of ifdef
        False   -- place #line droppings
        False   -- write #line
        False   -- keep #pragma in final output
        True    -- remove C EOL comments
        True   -- remove C inline comments
        False   -- lex input as Haskell
        False   -- permit stringise and concatenate operators
        True   -- retain newlines in macro expansions
        True    -- remove literate markup
        True    -- issue warnings
