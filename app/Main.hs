module Main where
import Options.Applicative
import Data.Semigroup()
import Data.Text
import System.IO

newtype Checker = CheckTaxes { filename :: FilePath } deriving (Show)

sample :: Parser Checker
sample = CheckTaxes
  <$> strOption (long "filename" <> short 'f' <> help "filepath of invoice.json")

readFileAndPrint :: FilePath -> IO ()
readFileAndPrint filepath = do
  handle <- openFile filepath ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

main :: IO ()
main = execute =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Test haskell programm"
     <> header "hello - a test for optparse-applicative" )


execute :: Checker -> IO ()
execute (CheckTaxes filePath) = do
  putStrLn $ "Check taxes, filename: " ++ filePath
  readFileAndPrint filePath
-- execute _ = return ()
