module Anagram
  ( solve
  )
where

import           Debug.Trace
import           Data.MultiSet                  ( MultiSet )
import qualified Data.MultiSet                 as MultiSet
import qualified Data.Maybe                    as Maybe

-- Given the characters remaining, and a list of words to try, return the first
-- word which can be spelled, the remaining untried words, and the remaining characters
findWord :: MultiSet Char -> [String] -> Maybe (String, [String], MultiSet Char)
findWord remainingChars [] = Nothing
findWord remainingChars (word : rest) =
  if wordSet `MultiSet.isSubsetOf` remainingChars
    then Just (word, rest, MultiSet.difference remainingChars wordSet)
    else findWord remainingChars rest
  where wordSet = MultiSet.fromList word

shrinkDict :: (Int -> [String]) -> Int -> [String] -> (Int -> [String])
shrinkDict dict thisLength unCheckedThisLength i =
  if i == thisLength then unCheckedThisLength else dict i



-- Given a set of characters remaining, a dictionary (represented as a function from
-- word length to List of Strings), a list of word lengths remaining, return the
-- first solution to the anagram
solve :: MultiSet Char -> (Int -> [String]) -> [Int] -> [String] -> [[String]]
solve chars _ [] acc =
  if null chars then trace ("Found solution: " ++ show acc) [acc] else [] -- if we have a solution, we expect to have no characters in the bag
solve chars dict (wordLength : rest) acc =
  let words          = dict wordLength
      maybeFirstWord = findWord chars words -- try to spell a word
  in  case maybeFirstWord of
        Just (firstWord, uncheckedWords, remainingChars) ->
          let solutionsNotUsingFirstWord = solve
                chars
                (shrinkDict dict wordLength uncheckedWords)
                (wordLength : rest)
                acc
              solutionsUsingFirstWord = solve
                remainingChars
                (shrinkDict dict wordLength (firstWord : uncheckedWords))
                rest
                (firstWord : acc)
          in  solutionsUsingFirstWord ++ solutionsNotUsingFirstWord
        Nothing -> []
