module Common where

import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text

readTextLines :: String -> IO [Text]
readTextLines p = do
  f <- readFile p
  return $ map pack (lines f)