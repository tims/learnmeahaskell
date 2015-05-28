import qualified Data.Map.Strict as Map
import Data.List
import Data.Function
import Data.Ord

main :: IO ()
main = return ()

data Label a = In a | Out a | Undecided a deriving (Show, Eq, Ord)

data ArgumentFramework a = AF {
 forwardAttacks :: Map.Map a [a],
 reverseAttacks :: Map.Map a [a]
} deriving (Show, Eq, Ord)

sortGroupBy f = groupBy ((==) `on` f) . sortBy (comparing f)

mkAF :: (Ord a) => [(a,a)] -> ArgumentFramework a
mkAF attacks = let
    fwd = (map (\atts -> (fst (head atts), map snd atts)) (sortGroupBy fst attacks))
    rev = (map (\atts -> (snd (head atts), map fst atts)) (sortGroupBy snd attacks))
    in AF (Map.fromList fwd) (Map.fromList rev)

attackers :: (Ord a) => ArgumentFramework a -> a -> [a]
attackers af a = case (Map.lookup a (reverseAttacks af)) of
    Just a  -> a
    Nothing -> []

grounded_labelling :: (Show a, Eq a, Ord a) => ArgumentFramework a -> [Label a] -> [Label a]
grounded_labelling af labelling =
    let ins = [a | In a <- labelling]
        outs = [a | Out a <- labelling]
        undecided = [a | Undecided a <- labelling]

        ins' = ins ++ [a | a <- undecided, hasNoValidAttacks a outs (attackers af a)]
        outs' = outs ++ [a | a <- undecided, hasValidAttack a ins (attackers af a)]
        undecided' = [a | a <- undecided, notElem a (ins' ++ outs')]

        labelling' = (map In ins') ++ (map Out outs') ++ (map Undecided undecided')
    in
        if length undecided == length undecided' then
            labelling'
        else
            grounded_labelling af labelling'

hasNoValidAttacks :: (Eq a) => a -> [a] -> [a] -> Bool
hasNoValidAttacks a outs attackers = and [ elem x outs | x <- attackers]

hasValidAttack :: (Eq a) =>  a -> [a] -> [a] -> Bool
hasValidAttack a ins attackers  = or [ elem x ins | x <- attackers]