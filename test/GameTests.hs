module GameTests where

import Game
import Combinatorial
import Test.HUnit
import Games.Hackenbush
import Games.ToadsAndFrogs

testWellKnownGames :: Test
testWellKnownGames = "Well known Games" ~:
  TestList [
    TestCase $ assertEqual "0" 0 (Game [] []),
    TestCase $ assertEqual "1" 1 (Game [0] []),
    TestCase $ assertEqual "-1" (-1) (Game [] [0]),
    TestCase $ assertEqual "1/2" (1/2) (0 .| 1)
  ]
  
testSimplify :: Test
testSimplify = "Game simplification" ~:
  TestList [
    TestCase $ assertEqual "{ 0 1 * | 2 5}" (3/2) (Game [0, 1, star] [2, 5]),
    TestCase $ assertEqual "{ ↑ | ↓ }" star (up .| down),
    TestCase $ assertEqual "*4 + *5" star (nim 4 + nim 5)
  ]

testArithmetic :: Test
testArithmetic = "Game arithmetic" ~:
  TestList [
    TestCase $ assertEqual "1 + 1" 2 ((1 :: Game)  + (1 :: Game)),
    TestCase $ assertEqual "* + *" 0 (star + star),
    TestCase $ assertEqual "↑ + ↓" 0 (up + down),
    TestCase $ assertEqual "*4 + *5" star (nim 4 + nim 5)
  ]

testTemperature :: Test
testTemperature = "Heating and cooling games" ~:
  TestList [
    TestCase $ assertEqual "Mean value of { 2|-1, 0 || -2|-4}" (-5/4) (meanValue g),
    TestCase $ assertEqual "Temperature of { 2|-1, 0 || -2|-4}" (7/4) (temperature g),
    TestCase $ assertEqual "Cooling { 2|-1, 0 || -2|-4} to its freezing point" (-5/4 + star) (cool (7/4) g),
    TestCase $ assertEqual "Heating ↑" (1 .|| 0 .| (-2)) (heat 1 up),
    TestCase $ assertEqual "Thermal dissociation" (Thermal 1 [down `HeatedBy` 1, star `HeatedBy` 2]) (thermalDissociation $ 1 + heat 1 down + heat 2 star)
  ]
    where
      g = Game [2 .| (-1), 0] [(-2) .| (-4)]

testAtomicWeight :: Test
testAtomicWeight = "Game atomic weight" ~:
  TestList [
    TestCase $ assertEqual "1" Nothing (atomic 1),
    TestCase $ assertEqual "*" (Just 0) (atomic star),
    TestCase $ assertEqual "↑" (Just 1) (atomic up),
    TestCase $ assertEqual "↓" (Just (-1)) (atomic down),
    TestCase $ assertEqual "⇑" (Just 2) (atomic $ up + up),
    TestCase $ assertEqual "3.↓" (Just (-3)) (atomic $ down + down + down),
    TestCase $ assertEqual "⇑|↓ + ⇑|⇓" (Just (1/2 + star)) (atomic $ (up+up) .| down + (up+up) .| (down+down))
  ]
      