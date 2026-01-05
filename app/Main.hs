module Main where

import Control.Monad (forever)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import System.Exit (exitSuccess)
import Text.Read qualified as Text

storage :: FilePath
storage = "/home/sjurmi/haskell/todo/.todos"

data Exit = Exit

main :: IO ()
main =
  forever $ do
    welcome
    todos <- Text.lines <$> TIO.readFile storage
    printTodos todos
    cmd <- TIO.getLine
    bitraverse
      (const exitSuccess)
      (TIO.writeFile storage . Text.unlines)
      (doInput cmd todos)
  where
    welcome :: IO ()
    welcome = do
      TIO.putStrLn "TODO"
      TIO.putStrLn "----"

    printTodos :: [Text] -> IO ()
    printTodos = traverse_ (print @(Int, Text)) . zip [0 ..]

doInput :: Text -> [Text] -> Either Exit [Text]
doInput "" _ = Left Exit
doInput cmd todos =
  either
    (const $ Right $ cmd : todos)
    (Right . delete)
    (parseInt cmd)
  where
    parseInt :: Text -> Either String Int
    parseInt = Text.readEither @Int . Text.unpack

    delete :: Int -> [Text]
    delete i =
      let (upTo, after) = splitAt i todos
       in upTo ++ drop 1 after
