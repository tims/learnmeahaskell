import qualified Data.Map.Strict as Map
import Data.List
import Data.Function
import Data.Ord

main :: IO ()
main = return ()

data Label a = In a | Out a | Undecided a deriving (Show, Eq)

data ArgumentFramework a = AF [(a,a)] deriving (Show, Eq)

-- record syntax
-- data ArgumentFramework2 a = AF2 (M.Map a [a]) (M.Map a [a])
data ArgumentFramework2 a = AF2 {
 forwardAttacks :: Map.Map a [a],
 reverseAttacks :: Map.Map a [a] } deriving (Show, Eq)

sortGroupBy f = groupBy ((==) `on` f) . sortBy (comparing f)

mkAF :: (Ord a) => [(a,a)] -> ArgumentFramework2 a
mkAF attacks = let
    fwd = (map (\atts -> (fst (head atts), map snd atts)) (sortGroupBy fst attacks))
    rev = (map (\atts -> (snd (head atts), map fst atts)) (sortGroupBy snd attacks))
    in AF2 (Map.fromList fwd) (Map.fromList rev)

grounded_labelling :: (Show a, Eq a) => ArgumentFramework a -> [Label a] -> [Label a]
grounded_labelling af labelling =
    let ins = [a | In a <- labelling]
        outs = [a | Out a <- labelling]
        undecided = [a | Undecided a <- labelling]

        ins' = ins ++ [a | a <- undecided, hasNoValidAttacks a outs af]
        outs' = outs ++ [a | a <- undecided, hasValidAttack a ins af]
        undecided' = [a | a <- undecided, notElem a (ins' ++ outs')]

        labelling' = (map In ins')
            ++ (map Out outs')
            ++ (map Undecided undecided')
    in
        if length undecided == length undecided' then
            labelling'
        else
            grounded_labelling af labelling'

hasNoValidAttacks :: (Eq a) => a -> [a] -> ArgumentFramework a -> Bool
hasNoValidAttacks a outs (AF attacks) =
    and [ elem x outs | (x,y) <- attacks, y == a]

hasValidAttack :: (Eq a) =>  a -> [a] -> ArgumentFramework a -> Bool
hasValidAttack a ins (AF attacks) =
    or [ elem x ins | (x,y) <- attacks, y == a]