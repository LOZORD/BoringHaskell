-- Based off of the Peter Norvig Python implementation:
-- http://norvig.com/spell-correct.html
module Spell where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Set as Set

wordProbability :: Fractional v => Map.Map String v -> String -> v
wordProbability allWords myWord =
  let total = sum (Map.elems allWords)
      prob = maybe 0 (id) (Map.lookup myWord allWords)
  in prob / total

correction :: (Fractional v, Ord v) => Map.Map String v -> String -> String
correction allWords myWord =
  let myCandidates = candidates allWords myWord
      getWP = wordProbability allWords
      compWP a b = compare (getWP a) (getWP b)
  in List.maximumBy compWP myCandidates

nonEmpty :: Foldable t => t a -> Bool
nonEmpty = not.null

orSet :: [Set.Set a] -> Set.Set a
orSet sets = maybe Set.empty id (List.find (nonEmpty) sets)

candidates :: Map.Map String v -> String -> Set.Set String
candidates allWords word =
  let known = knownWords allWords
      knownNoEdit = known (edits 0 word)
      knownEdits1 = known (edits 1 word)
      knownEdits2 = known (edits 2 word)
      unknown = Set.singleton word
  in  orSet [knownNoEdit, knownEdits1, knownEdits2, unknown]

knownWords :: Map.Map String v -> Set.Set String -> Set.Set String
knownWords allWords mySet = Set.filter inDictionary mySet
  where inDictionary someWord = Map.member someWord allWords

edits :: Integer -> String -> Set.Set String
edits 0 word = Set.singleton word
edits 1 word = edits1 word
edits num word =
  let prevEdits = (edits (num - 1) word) :: Set.Set String
      addNewEdits prevEdit newSet = Set.union (edits1 prevEdit) newSet
  in Set.foldr addNewEdits prevEdits prevEdits

edits1 :: String -> Set.Set String
edits1 word =
  let letters = ['a'..'z']
      size = length word
      splits = [ List.splitAt i word | i <- [0..(size + 1)] ]
      deletes = map deletion splits
      transposes = map transpose splits
      replaces = [ replace pair char | pair <- splits, char <- letters ]
      inserts = [ insert pair char | pair <- splits, char <- letters ]
      allEdits = deletes ++ transposes ++ replaces ++ inserts
      actualEdits = filter (nonEmpty) allEdits
  in Set.fromList actualEdits

deletion :: (String, String) -> String
deletion (left, r:ight) = left ++ ight
deletion _ = [] 

transpose :: (String, String) -> String
transpose (left, r:i:ght) = left ++ i:r:ght
transpose _ = []

replace :: (String, String) -> Char -> String
replace (left, r:ight) c = left ++ c:ight
replace _ _ = []

insert :: (String, String) -> Char -> String
insert (left, right) c = left ++ c:right
