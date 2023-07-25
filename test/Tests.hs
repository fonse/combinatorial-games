module Main where
import Game
import Combinatorial
import Test.HUnit
import qualified System.Exit as Exit
import Games.Hackenbush

half = Game [0] [1]
quarter = Game [0] [half]

tests :: Test
tests = "Hackenbush" ~: TestList [
            "1x Blue" ~: toGame [blue] @?= 1,
            "2x Blue" ~: toGame [blue, blue] @?= 2,
            "1x Red" ~: toGame [red] @?= -1,
            "2x Red" ~: toGame [red, red] @?= -2,
            "1x Green" ~: toGame [green] @?= star,
            "2x Green" ~: toGame [green, green] @?= 0,
            "2x Blue + 1x Red + 1x Green" ~: toGame [blue, blue, red, green] @?= 1 + star,
            "Blue Red" ~: toGame [stack [Blue, Red]] @?= half,
            "Blue Red Red" ~: toGame [stack [Blue, Red, Red]] @?= quarter,
            "Flower with blue petal + 1x Green" ~: toGame [flower [blue], green] @?= up,
            "Flower with 3x blue petals and 2x red petals" ~: toGame [flower [blue, blue, blue, red, red]] @?= up + star
        ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess