import Control.Exception (assert)
import Data.Word (Word8)
import Data.Bits

type State = Bool

type Line = [State]

type Rule = Word8

--Length of 3
data Neighbourhood = Neighbourhood {
      leftNeighbour   :: State
    , middleNeighbour :: State
    , rightNeighbour  :: State
    }
  deriving Eq

stateToStr :: State -> String
stateToStr s = (if s then "T"
                     else "F")

instance Show Neighbourhood where
  show (Neighbourhood l m r) = show $ (stateToStr l) ++ (stateToStr m) ++ (stateToStr r)

--neighbourhoodNumber :: Neighbourhood -> Word8
--neighbourhoodNumber neighbourhood = (head neighbourhood)

--type Neighbourhoods = [Neighbourhood]

startingOut :: Line
startingOut = [True]

findNeighbourhoods :: Line -> [Neighbourhood]
findNeighbourhoods line = findNeighbourhoodsNaive $ (False:False:line ++ [False,False])

findNeighbourhoodsNaive :: Line -> [Neighbourhood]
findNeighbourhoodsNaive (a:b:c:xs) = (Neighbourhood a b c) : (findNeighbourhoodsNaive (b:c:xs))
findNeighbourhoodsNaive _ = []

neighbourhoodNumber :: Neighbourhood -> Word8
neighbourhoodNumber a = (if leftNeighbour a then 2^2
                                            else 0) +
                        (if middleNeighbour a then 2^1
                                              else 0) +
                        (if rightNeighbour a then 2^0
                                             else 0)

ruleApplyToNeighbourhood :: Rule -> Neighbourhood -> State
ruleApplyToNeighbourhood rule neighbourhood = 0 < (rule .&. (2^(neighbourhoodNumber neighbourhood)))

ruleApplyToNeighbourhoods :: Rule -> [Neighbourhood] -> Line
ruleApplyToNeighbourhoods rule neighbourhoods =
  let ruleApplyToNeighbourhoodsRuleIncluded = ruleApplyToNeighbourhood rule
  in map ruleApplyToNeighbourhoodsRuleIncluded neighbourhoods

ruleApply :: Rule -> Line -> Line
ruleApply rule line = ruleApplyToNeighbourhoods rule (findNeighbourhoods line)

ruleGeneratorFromInitial :: Line -> Rule -> [Line]
ruleGeneratorFromInitial initial rule = initial : (ruleGeneratorFromInitial (ruleApply rule initial) rule)

ruleGenerator :: Rule -> [Line]
ruleGenerator = ruleGeneratorFromInitial startingOut

--Rule30 partially applied versions
rule30 :: Rule
rule30 = 30

rule30Apply :: Line -> Line
rule30Apply = ruleApply rule30

rule30GeneratorFromInitial :: Line -> [Line]
rule30GeneratorFromInitial line = ruleGeneratorFromInitial line rule30

rule30Generator :: [Line]
rule30Generator = ruleGenerator rule30

main :: IO ()
main = do
  print "Elementary Cellular Automata"
  print $ findNeighbourhoodsNaive [False, True, True, False]
  --print $ findNeighbourhoods startingOut
  --print $ ruleApplyToNeighbourhoods rule30 [(Neighbourhood False True False)]

  print "rule30Generator"
  mapM_ print (take 4 rule30Generator)

  print "neighbourhoodNumber"
  print $ neighbourhoodNumber (Neighbourhood True True True)
  print $ neighbourhoodNumber (Neighbourhood True True False)
  print $ neighbourhoodNumber (Neighbourhood True False True)
  print $ neighbourhoodNumber (Neighbourhood True False False)
  print $ neighbourhoodNumber (Neighbourhood False True True)
  print $ neighbourhoodNumber (Neighbourhood False True False)
  print $ neighbourhoodNumber (Neighbourhood False False True)
  print $ neighbourhoodNumber (Neighbourhood False False False)

  print "ruleApplyToNeighbourhood"
  print $ ruleApplyToNeighbourhood rule30 (Neighbourhood True True True)
  print $ ruleApplyToNeighbourhood rule30 (Neighbourhood True True False)
  print $ ruleApplyToNeighbourhood rule30 (Neighbourhood True False True)
  print $ ruleApplyToNeighbourhood rule30 (Neighbourhood True False False)
  print $ ruleApplyToNeighbourhood rule30 (Neighbourhood False True True)
  print $ ruleApplyToNeighbourhood rule30 (Neighbourhood False True False)
  print $ ruleApplyToNeighbourhood rule30 (Neighbourhood False False True)
  print $ ruleApplyToNeighbourhood rule30 (Neighbourhood False False False)
