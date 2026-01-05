module Main where

import Control.Monad (forever)
import Data.Bitraversable (Bitraversable (bitraverse))
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
    bitraverse
      (const exitSuccess)
      (TIO.writeFile storage . Text.unlines)
      (doInput todos cmd)

doInput :: [Text] -> Text -> Either Exit [Text]
doInput _ "" = Left Exit
doInput todos cmd =
  either
    (const $ Right $ cmd : todos)
    (\i -> Right $ delete i todos)
    (parseInt cmd)
  where
    parseInt :: Text -> Either String Int
    parseInt = Text.readEither @Int . Text.unpack

    delete :: Int -> [Text] -> [Text]
    delete i l = case l !? i of
      Nothing -> l
      Just it -> filter (/= it) l
