module Main where
import Test.HUnit
import qualified System.Exit as Exit
import CombinatorialInstancesTests
import GameTests

main :: IO ()
main = do
  resultWellKnownGames <- runTestTT testWellKnownGames
  resultSimplify <- runTestTT testSimplify
  resultArithmetic <- runTestTT testArithmetic
  resultTemperature <- runTestTT testTemperature
  resultAtomicWeight <- runTestTT testAtomicWeight

  resultHackenbush <- runTestTT testsHackenbush
  resultToadsAndFrogs <- runTestTT testsToadsAndFrogs

  let failed = sum $ failures <$>
        [
          resultWellKnownGames,
          resultSimplify,
          resultArithmetic,
          resultTemperature,
          resultAtomicWeight,
          resultHackenbush,
          resultToadsAndFrogs
        ]

  if failed > 0 
    then Exit.exitFailure 
    else Exit.exitSuccess