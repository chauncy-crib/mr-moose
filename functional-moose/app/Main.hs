{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Anagram
import           Data.Aeson
import           Data.Text

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.MultiSet                 as MultiSet
import qualified Data.ByteString.Lazy          as B
import           Control.Applicative
import           Control.Monad



jsonPath = "/Users/davidabrahams/mr-moose/words_by_length_dict.json"

getJSON :: String -> IO B.ByteString
getJSON s = B.readFile s

-- represent our dictionary as a function from word length to a list of words of that length
-- dict :: Int -> [String]
-- dict 1 = ["a"]
-- dict 3 = ["cat", "dog"]
-- dict 4 = ["poop"]
-- dict 5 = ["butts"]
-- dict _ = []


dictFromMap :: (Map Int [String]) -> (Int -> [String])
dictFromMap m = \i -> case Map.lookup i m of
  Just l  -> l
  Nothing -> []

poop = "poop"
poopdog = "pogdoop"
catdog = "dcatgo"
poopbutts = "potoubpts"
buttspoopcatdog = "godtacpoopsttbu"
nosolution1 = "godtacpoopsttb"
nosolution2 = "godtacpoopsttbuu"


moosePuzzle = "mfctosesannhddcoieisanartheowgvtesebhr"

-- Some test cases, all working

-- main :: IO ()
-- main = putStrLn $ show (solve (MultiSet.fromList poopdog) dict [4, 3])

-- main =
  -- putStrLn $ show (solve (MultiSet.fromList nosolution1) dict [5, 4, 3, 3])
-- main = putStrLn $ show (solve (MultiSet.fromList "a") dict [1])
-- main = putStrLn $ show (solve (MultiSet.fromList "a") dict [1])
--
--
main :: IO ()
main = do
 -- Get JSON data and decode it
  d <-
    (eitherDecode <$> getJSON jsonPath) :: IO (Either String (Map Int [String]))
  case d of
    Left err -> putStrLn err
    Right (m :: Map Int [String]) ->
      let jsonDict = dictFromMap m
          solution =
            -- solve (MultiSet.fromList moosePuzzle) jsonDict [10, 7, 6, 6, 5, 4]
            solve (MultiSet.fromList moosePuzzle) jsonDict [10, 7, 6, 6, 5, 4]
      in  putStrLn $ show solution

 --
 --
 --
 --
