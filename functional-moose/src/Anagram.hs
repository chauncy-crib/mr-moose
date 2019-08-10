module Anagram
  ( solve
  , traceSolve
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


-- Same as solve, except it prints the current accumulator before recursing
traceSolve chars dict wordLengths acc =
  traceShow acc (solve chars dict wordLengths acc)


-- Given a set of characters remaining, a dictionary (represented as a function from
-- word length to List of Strings), a list of word lengths remaining, return the
-- first solution to the anagram
solve
  :: MultiSet Char -> (Int -> [String]) -> [Int] -> [String] -> Maybe [String]
solve chars _ [] acc = if null chars then Just acc else Nothing -- if we have a solution, we expect to have no characters in the bag
solve chars dict (wordLength : rest) acc =
  let words          = dict wordLength
      maybeFirstWord = findWord chars words -- try to spell a word
  in  case maybeFirstWord of
        Just (firstWord, uncheckedWords, remainingChars) ->
          case maybeSolution of -- recursively solve the problem
            Just finalSolution -> Just (firstWord : finalSolution) -- we found a solution!
            Nothing -> traceSolve chars -- our choice for the first word did not lead to a solution, so try again.
                                  (shrinkDict dict wordLength uncheckedWords) -- remove the words we have already considered from the dictionary
                                  (wordLength : rest)
                                  acc
         where
          maybeSolution = traceSolve
            remainingChars
            (shrinkDict dict wordLength (firstWord : uncheckedWords)) -- we are allowing the algorithm to spell the same word twice
            rest
            (firstWord : acc)
        Nothing -> Nothing
