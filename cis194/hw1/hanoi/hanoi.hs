-- Exercise 5
-- some "typedefs"/"type synonyms"
type Peg = String
type Move = (Peg, Peg)

-- we take the number of discs and the three Pegs to work with
-- and return a list of Moves
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- if there are no discs to move, we don't have any Moves!
-- (underscores mean that we don't care about arguments)
hanoi 0        _            _             _           = []
-- otherwise, follow the recipe from the exercise
hanoi numDiscs startPegName finishPegName tempPegName =
  let stepOneResults    = hanoi (numDiscs - 1) startPegName tempPegName finishPegName
      stepTwoResults    = [(startPegName, finishPegName)]
      stepThreeResults  = hanoi (numDiscs - 1) tempPegName finishPegName startPegName
      -- concatenate the results into a larger list
  in  stepOneResults ++ stepTwoResults ++ stepThreeResults
