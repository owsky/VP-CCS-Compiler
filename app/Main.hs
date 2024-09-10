module Main (main) where

import Control.DeepSeq (deepseq)
import Data.Maybe (fromMaybe)
import Data.Text.IO (readFile)
import GHC.Generics (Generic)
import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, header, help, helper, info, long, metavar, option, optional, progDesc, short, strArgument, (<**>))
import ProcessLines (processLines)
import System.FilePath (replaceExtension, takeExtension)
import Prelude hiding (lines, null, readFile)

main :: IO ()
main = do
  args <- execParser opts

  let maxInt = fromMaybe 5 $ maxIntArg args -- default max int is 5 for convenience
  let inputFilePath = pathArg args

  outputFilePath <- outputFile inputFilePath

  fileContents <- readFile inputFilePath
  processedLines <- processLines maxInt fileContents
  -- forcing strict evaluation so an empty file is not created in case of errors
  processedLines `deepseq` writeFile outputFilePath (unlines processedLines)

data Args = Args
  { maxIntArg :: Maybe Int,
    pathArg :: FilePath
  }
  deriving (Show, Generic)

argsParser :: Parser Args
argsParser =
  Args
    <$> optional (option auto (long "max" <> short 'm' <> metavar "MAX" <> help "Maximum natural"))
    <*> strArgument (metavar "PATH" <> help "Input program file path")

opts :: ParserInfo Args
opts = info (argsParser <**> helper) (fullDesc <> progDesc "" <> header "")

outputFile :: FilePath -> IO (FilePath)
outputFile path
  | takeExtension path == ".vccs" = return $ replaceExtension path ".ccs"
  | otherwise = fail $ "Input file has the wrong file extension. Expected: .vccs, got: " ++ takeExtension path
