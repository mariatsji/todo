module Main where

import Control.Monad (forever)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import GHC.Base (when)
import System.Exit (exitSuccess)
import Text.Read qualified as Text

storage :: FilePath
storage = "/home/sjurmi/haskell/todo/.todos"

main :: IO ()
main =
  forever $ do
    welcome
    todos <- Text.lines <$> TIO.readFile storage
    printTodos todos
    cmd <- TIO.getLine
    when (cmd == "") exitSuccess
    let newTodos = append cmd todos
    writeTodos newTodos
  where
    welcome :: IO ()
    welcome = do
      TIO.putStrLn "TODO"
      TIO.putStrLn "----"

    printTodos :: [Text] -> IO ()
    printTodos = traverse_ (print @(Int, Text)) . zip [0 ..]

    writeTodos :: [Text] -> IO ()
    writeTodos list = TIO.writeFile storage (Text.unlines list)

append :: Text -> [Text] -> [Text]
append cmd todos =
  either
    (const $ cmd : todos)
    delete
    (parseInt cmd)
  where
    parseInt :: Text -> Either String Int
    parseInt = Text.readEither @Int . Text.unpack

    delete :: Int -> [Text]
    delete i =
      let (upTo, after) = splitAt i todos
       in upTo ++ drop 1 after
