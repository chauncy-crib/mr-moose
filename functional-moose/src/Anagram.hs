
module Anagram
  ( solve
  )
where

import           Data.MultiSet                  ( MultiSet )
import qualified Data.MultiSet                 as MultiSet

-- Given the characters remaining, and a list of words to try, return the first
-- word which can be spelled, the remaining untried words, and the remaining characters
findWord
  :: (MultiSet Char) -> [String] -> (Maybe (String, [String], (MultiSet Char)))
findWord remainingChars [] = Nothing
findWord remainingChars (word : rest) =
  if (wordSet `MultiSet.isSubsetOf` remainingChars)
    then (Just (word, rest, MultiSet.difference remainingChars wordSet))
    else findWord remainingChars rest
  where wordSet = MultiSet.fromList word


solveCurrentWord
  :: [String] -- the current list of candidate words. Should all be the same length
  -> (MultiSet Char) -- the bag of characters remaining
  -> [Int] -- the word lengths we need to form, after this word.
  -> (Int -> [String]) -- the dictionary
  -> (Maybe [String]) -- the solution to the whole problem
solveCurrentWord currentWords remainingChars remainingLengths dict =
  let firstWord = findWord remainingChars currentWords
  in  case firstWord of
        Just (word, rest, rem) ->
          let maybeSoln = solve rem dict remainingLengths
          in  case maybeSoln of
                Just words -> Just (word : words) -- if we have found a solution, return it
                Nothing ->
                  solveCurrentWord rest remainingChars remainingLengths dict -- otherwise, try a new word
        _ -> Nothing


-- Given a set of characters remaining, a dictionary (represented as a function from
-- word length to List of Strings), a list of word lengths remaining, return the
-- first solution to the anagram
solve :: (MultiSet Char) -> (Int -> [String]) -> [Int] -> (Maybe [String])
solve remainingChars _ [] = if null remainingChars then Just [] else Just [] -- if we have a solution, we expect to have no characters in the bag
solve remainingChars dict (wordLength : rest) =
  let words = dict wordLength
  in  solveCurrentWord words remainingChars rest dict




