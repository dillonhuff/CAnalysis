import Data.ByteString.Char8 as BS
import Data.List as L
import Language.C.Analysis
import Language.C.Data.Position
import Language.C.Parser
import Language.C.Pretty
import Language.Preprocessor.Cpphs
import System.Process

fileDir = "/Users/dillon/Haskell/Bugs/CAnalysis/"
fileName = "test_parse.c"
filePath = fileDir ++ fileName
preprocessorResPath = "/Users/dillon/Haskell/Bugs/CAnalysis/pre.c"

{-main = do
  parseRes <- parseCFile filePath
  case parseRes of
    Left str -> Prelude.putStrLn str
    Right decls -> Prelude.putStrLn $ show $ pretty decls-}

main = do
  cnts <- Prelude.readFile "pre.c"
  case parsePreprocessedC "pre.c" cnts of
    Left err -> Prelude.putStrLn err
    Right decls -> Prelude.putStrLn $ show $ pretty decls

parseCFile :: String -> IO (Either String GlobalDecls)
parseCFile filePath = do
  preprocessedFileContents <- runClangPreprocessor filePath
  return $ parsePreprocessedC filePath preprocessedFileContents

parsePreprocessedC :: FilePath -> String -> Either String GlobalDecls
parsePreprocessedC filePath preprocessedFileContents =
  case parseC (pack preprocessedFileContents) (initPos filePath) of
    Left err -> Left $ show err
    Right pRes -> let aRes = runTrav_ $ analyseAST pRes in
      case aRes of
        Left errors -> Left $ show errors
        Right (decls, _) -> Right decls

runClangPreprocessor :: FilePath -> IO String
runClangPreprocessor filePath = do
  res <- clangPreprocessCommand filePath
  let cleanRes = stripPreprocessorOutput res in
    do
      Prelude.writeFile preprocessorResPath cleanRes
      return res

stripPreprocessorOutput str =
  L.concat $ L.intersperse "\n" $ L.filter (\l -> L.length l == 0 || L.head l /= '#') $ L.lines str

clangPreprocessCommand filePath = readProcess "clang" ["-E", filePath] []
  
runPreprocessor :: FilePath -> String -> IO String
runPreprocessor filePath contents = do
  preprocessorRes <- runCpphs cppOptions filePath contents
  Prelude.writeFile preprocessorResPath preprocessorRes
  return preprocessorRes

cppOptions =
  CpphsOptions [] [] [("__GNUC__", "5"), ("__x86_64__", "1")] ["/usr/local/include",
                      "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/6.1.0/include",
                      "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include",
                      "/usr/include",
                      "/System/Library/Frameworks",
                      "/Library/Frameworks"] [] boolOptions

boolOptions =
  BoolOptions
        False   -- leave define out of ifdef
        False   -- place #line droppings
        True   -- write #line
        False   -- keep #pragma in final output
        True    -- remove C EOL comments
        True   -- remove C inline comments
        True   -- lex input as Haskell
        False   -- permit stringise and concatenate operators
        False   -- retain newlines in macro expansions
        False    -- remove literate markup
        True    -- issue warnings
