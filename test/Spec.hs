module Main where

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           System.FilePath (takeBaseName, replaceExtension)
import           Test.Tasty (defaultMain, TestTree, testGroup)
import           Test.Tasty.Golden (goldenVsStringDiff, findByExtension)

import           Idris.Main (idris, idrisMain)
import           Idris.Options

import           Codegen.SExp

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  idrisFiles <- testCasesByExtension ".idr"
  compileToBytecode idrisFiles
  return $ testGroup " SExp"
    [ goldenVsStringDiff
        (takeBaseName idrisFile) -- test name
        diff
        goldenFile
        (sexpByteStringFromIdris idrisFile)
    | idrisFile <- idrisFiles
    , let goldenFile = replaceExtension idrisFile ".sexp"
    ]

diff :: String -> String -> [String]
diff ref new = ["diff", "-u", ref, new]

testCasesByExtension :: FilePath -> IO [FilePath]
testCasesByExtension ext = findByExtension [ext] "./test/cases"

compileToBytecode :: [FilePath] -> IO ()
compileToBytecode files = runMain (idrisMain options) where
  options = TypeInType : NoREPL : Quiet : (Filename <$> files)

sexpByteStringFromIdris :: FilePath -> IO LBS.ByteString
sexpByteStringFromIdris idrisFile = either throwError id <$> compile where
  compile = evalIdris (render <$> codegenInfo)
  codegenInfo = idrisCodegenInfoFor [idrisFile] (idrisFile ++ ".sexp")
  render = LBS.fromStrict . Text.encodeUtf8 . renderSExp
  throwError = error . show
