module Main where

import Data.Foldable (traverse_)
import Data.List ((!?))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import System.Exit (exitSuccess)
import System.IO (stdin)
import Text.Read qualified as Text

storage :: FilePath
storage = "/home/sjurmi/haskell/todo/.todos"

main :: IO ()
main = do
  putStrLn "TODO"
  putStrLn "----"
  todos <- Text.lines <$> TIO.readFile storage
  traverse_ (print @(Int, Text)) ([0 ..] `zip` todos)
  cmd <- TIO.hGetLine stdin
  newTodos <- doInput todos cmd
  TIO.writeFile storage (Text.unlines newTodos)
  main

doInput :: [Text] -> Text -> IO [Text]
doInput todos cmd = do
  case Text.readMaybe @Int (Text.unpack cmd) of
    Just i -> delete i todos
    _ ->
      case cmd of
        "" -> do
          _ <- exitSuccess
          pure todos
        todo -> pure (todo : todos)

delete :: Int -> [Text] -> IO [Text]
delete i l = case l !? i of
  Nothing -> pure l
  Just it -> pure $ filter (/= it) l
