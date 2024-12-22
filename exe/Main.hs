import Control.Exception (throwIO)
import System.Environment (getArgs)

import Language.Colon.Parsing (parse)
import Language.Colon.Semantic (State(..), denote, emptyEvalState)

main :: IO ()
main = do
  [path] <- getArgs
  source <- readFile path
  input <- getContents
  source <- case parse source of
    Left err -> throwIO err
    Right source -> pure source
  MkState{output} <- case denote source emptyEvalState{input} of
    Left err -> throwIO err
    Right state -> pure state
  putStrLn (reverse output)