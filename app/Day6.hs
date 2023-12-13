module Day6 where
import Text.Printf

data Race = Race {
    t :: Int,
    d :: Int
}

r1 = [Race 57 291, Race 72 1172, Race 69 1176, Race 92 2026]
r2 = Race 57726992 291117211762026

wins r held = (held * (t r - held)) > d r
ways r = length $ filter (wins r) [1..t r]

day6 :: IO ()
day6 = do
    printf "Part 1: %d\n" $ product $ map ways r1
    printf "Part 2: %d\n" $ ways r2