module Main where

import           Anagram
import qualified Data.MultiSet                 as MultiSet

-- represent our dictionary as a function from word length to a list of words of that length
dict :: Int -> [String]
dict 1 = ["a"]
dict 3 = ["cat", "dog"]
dict 4 = ["poop"]
dict 5 = ["butts"]
dict _ = []

poop = "poop"
poopdog = "pogdoop"
catdog = "dcatgo"
poopbutts = "potoubpts"
buttspoopcatdog = "godtacpoopsttbu"
nosolution1 = "godtacpoopsttb"
nosolution2 = "godtacpoopsttbuu"

-- Some test cases, all working

main :: IO ()
-- main = putStrLn $ show (solve (MultiSet.fromList poopdog) dict [4, 3])
main =
  putStrLn $ show (solve (MultiSet.fromList buttspoopcatdog) dict [5, 4, 3, 3])
-- main =
  -- putStrLn $ show (solve (MultiSet.fromList nosolution1) dict [5, 4, 3, 3])
-- main = putStrLn $ show (solve (MultiSet.fromList "a") dict [1])
-- main = putStrLn $ show (solve (MultiSet.fromList "a") dict [1])
