import Data.List

data Label a = In a | Out a | Undecided a deriving (Show, Eq)

data ArgumentFramework a = AF [(a, a)] deriving (Show, Eq)

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