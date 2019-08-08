
module Anagram where

import           Data.MultiSet                  ( MultiSet )
import qualified Data.MultiSet                 as MultiSet

-- Given the characters remaining, and a list of words to try, return the first
-- word which can be spelled, along with the remaining untried words.
findWord :: (MultiSet Char) -> [String] -> (Maybe (String, [String]))
findWord remainingChars [] = Nothing
findWord remainingChars (word : rest) =
  if ((MultiSet.fromList word) `MultiSet.isSubsetOf` remainingChars)
    then (Just (word, rest))
    else findWord remainingChars rest

-- Given a set of characters remaining, a dictionary (represented as a function from
-- word length to List of Strings), a list of word lengths remaining, return the
-- first solution to the anagram
solve :: (MultiSet Char) -> (Int -> [String]) -> [Int] -> (Maybe [String])
solve remainingChars dict remainingWordLengths = case remainingWordLengths of
  wordLength : rest ->
    let words    = dict wordLength
        thisWord = findWord remainingChars words
    in  case thisWord of
          Just (word, rest) -> Nothing
          _                 -> Nothing


--   _ -> Just []



