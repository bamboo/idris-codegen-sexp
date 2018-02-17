module Main where

import           IRTS.CodegenCommon
import           Idris.AbsSyntax
import           Idris.Main

import qualified Data.Text.IO as Text

import           System.Environment
import           System.Exit

import           Codegen.SExp

data Opts
  = Opts
  { inputs :: [FilePath]
  , output :: FilePath
  } deriving Show

main :: IO ()
main = do
  opts <- getOpts
  if null (inputs opts)
    then showUsage
    else runMain (sexpMain opts)

getOpts :: IO Opts
getOpts = process (Opts [] "a.sexp") <$> getArgs where
  process opts ("-o":o:xs) = process (opts { output = o }) xs
  process opts (x:xs)      = process (opts { inputs = x:inputs opts }) xs
  process opts []          = opts

showUsage :: IO ()
showUsage = do
  putStrLn "SExp code generator mainly intended to be called by the idris compiler and not directly by a user."
  putStrLn "Usage: idris-codegen-sexp <ibc-files> [-o <output-file>]"
  exitSuccess

sexpMain :: Opts -> Idris ()
sexpMain opts = idrisCodegenInfoFor (inputs opts) (output opts) >>= runIO . codegenSExp

codegenSExp :: CodegenInfo -> IO ()
codegenSExp ci = Text.writeFile (outputFile ci) (renderSExp ci)

