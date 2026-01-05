module Main where

import Control.Monad (forever)
import Data.Foldable (traverse_)
import Data.List ((!?))
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
    todos <- Text.lines <$> TIO.readFile storage
    TIO.putStrLn "TODO"
    TIO.putStrLn "----"
    traverse_ (print @(Int, Text)) ([0 ..] `zip` todos)
    cmd <- TIO.getLine
    case doInput todos cmd of
      Left Exit -> exitSuccess
      Right newTodos -> TIO.writeFile storage (Text.unlines newTodos)

doInput :: [Text] -> Text -> Either Exit [Text]
doInput todos cmd =
  case parseCmd cmd of
    Right i -> Right (delete i todos)
    Left _ ->
      case cmd of
        "" -> Left Exit
        todo -> Right $ todo : todos
  where
    parseCmd :: Text -> Either String Int
    parseCmd = Text.readEither @Int . Text.unpack

    delete :: Int -> [Text] -> [Text]
    delete i l = case l !? i of
      Nothing -> l
      Just it -> filter (/= it) l
