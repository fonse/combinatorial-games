module Main where
import Game
import Combinatorial
import Test.HUnit
import qualified System.Exit as Exit
import Games.Hackenbush
import Games.ToadsAndFrogs

testGame :: Combinatorial a => String -> a -> Game -> Test
testGame msg x g = TestCase $ assertEqual msg g (toGame x)

testsHackenbush :: Test
testsHackenbush = "Hackenbush" ~:
    TestList [
        testGame "1x Blue" [blue] 1,
        testGame "2x Blue" [blue, blue] 2,
        testGame "1x Red" [red] (-1),
        testGame "2x Red" [red, red] (-2),
        testGame "1x Green" [green] star,
        testGame "2x Green" [green, green] 0,
        testGame "2x Blue + 1x Red + 1x Green" [blue, blue, red, green] (1 + star),
        testGame "Blue Red" [stack [Blue, Red]] (1/2),
        testGame "Blue Red Red" [stack [Blue, Red, Red]] (1/4),
        testGame "Flower with blue petal + 1x Green" [flower [blue], green] up,
        testGame "Flower with 3x blue petals and 2x red petals" [flower [blue, blue, blue, red, red]] (up + star)
    ]

testsToadsAndFrogs :: Test
testsToadsAndFrogs = "ToadsAndFrogs" ~:
    TestList [
        testGame "Stalemate" [Toad, Toad, Frog, Frog] 0,
        testGame "Miny-1/4" [Toad, Toad, Toad, Frog, Blank, Frog] (miny (1/4)),
        testGame "2*" [Toad, Toad, Blank, Blank, Blank, Frog] (2 + star)
    ]

main :: IO ()
main = do
    resultHackenbush <- runTestTT testsHackenbush
    resultToadsAndFrogs <- runTestTT testsToadsAndFrogs
    let failed = sum $ failures <$> [resultHackenbush, resultToadsAndFrogs]
    if failed > 0 then Exit.exitFailure else Exit.exitSuccess