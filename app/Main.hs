module Main where

import Control.Monad (forever)
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
main =
  forever $ do
    putStrLn "TODO"
    putStrLn "----"
    todos <- Text.lines <$> TIO.readFile storage
    traverse_ (print @(Int, Text)) ([0 ..] `zip` todos)
    cmd <- TIO.hGetLine stdin
    newTodos <- doInput todos cmd
    TIO.writeFile storage (Text.unlines newTodos)

doInput :: [Text] -> Text -> IO [Text]
doInput todos cmd = do
  case Text.readMaybe @Int (Text.unpack cmd) of
    Just i -> pure (delete i todos)
    _ ->
      case cmd of
        "" -> do
          _ <- exitSuccess
          pure todos
        todo -> pure (todo : todos)

delete :: Int -> [Text] -> [Text]
delete i l = case l !? i of
  Nothing -> l
  Just it -> filter (/= it) l
