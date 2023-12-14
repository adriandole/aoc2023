module Day7 where

import Common
import Data.Either
import Data.List
import Data.Ord
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Printf

data Hand = Hand
  { cards :: String,
    bet :: Int
  }
  deriving (Eq, Show)

pGame :: Parser [Hand]
pGame = some $ do
  cards <- count 5 anySingle <* space1
  Hand cards <$> decimal <* space1

handRank :: Hand -> Int
handRank h =
  let byLen = sortBy (comparing (Down . length)) (group (sort (cards h)))
      l1 = if not (null byLen) then length $ head byLen else error "Invalid hand"
      l2 = if length byLen >= 2 then length $ head $ tail byLen else 0
   in case l1 of
        1 -> 1 -- high card
        2 -> case l2 of
          2 -> 3 -- two pair
          1 -> 2 -- one pair
          _ -> error "Invalid hand"
        3 -> case l2 of
          2 -> 5 -- full house
          1 -> 4 -- three of a kind
        4 -> 6 -- four of a kind
        5 -> 7 -- five of a kind
        _ -> error "Invalid hand"

handRankJ :: Hand -> Int
handRankJ h =
  let noJ = filter (/= 'J') (cards h)
      jCount = length (cards h) - length noJ
      byLen = sortBy (comparing (Down . length)) (group (sort noJ))
   in case jCount of
        0 -> handRank h
        5 -> 7
        n ->
          let topCard = head $ head byLen
              subHand = h {cards = noJ ++ replicate n topCard}
           in handRank subHand

cardRank cards c = case elemIndex c cards of
  Just x -> x
  Nothing -> error $ "Invalid card: " ++ [c]

instance Ord Hand where
  hand1 <= hand2 = (handRank hand1, cardsRank hand1) <= (handRank hand2, cardsRank hand2)
    where
      cardsRank h = map (cardRank "23456789TJQKA") $ cards h

winnings :: [Hand] -> Int
winnings hands = sum $ zipWith (*) [1 ..] (map bet $ sort hands)

jCmp h1 h2 = compare (handRankJ h1, cardsRankJ h1) (handRankJ h2, cardsRankJ h2)
  where
    cardsRankJ h = map (cardRank "J23456789TQKA") $ cards h

winningsJ :: [Hand] -> Int
winningsJ hands = sum $ zipWith (*) [1 ..] (map bet $ sortBy jCmp hands)

day7 :: Text -> IO ()
day7 f = do
  let game = fromRight (error "Parse error") (parse pGame "day7.txt" f)
  --   parseTest pGame (pack f)
  printf "Part 1: %d\n" $ winnings game
  printf "Part 2: %d\n" $ winningsJ game
