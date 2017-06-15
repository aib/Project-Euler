pipN = 7
dieSides = 6

-- Multicombination, multiset or combination with repetitions
combsWithRep :: (Num k, Eq k) => k -> [a] -> [[a]]
combsWithRep 0 _ = [[]]
combsWithRep _ [] = []
combsWithRep k xxs@(x:xs) = map (x:) (combsWithRep (k-1) xxs) ++ combsWithRep k xs

-- Combination
combs :: (Num k, Eq k) => k -> [a] -> [[a]]
combs 0 _ = [[]]
combs _ [] = []
combs k xxs@(x:xs) = map (x:) (combs (k-1) xs) ++ combs k xs

allRolls :: [(Int, Int)]
allRolls = [(x,y) | x <- take dieSides [0..], y <- take dieSides [0..]]

result :: Ord a => [a] -> [a] -> [(Int, Int)] -> [Ordering]
result die1 die2 rolls =
    map (singleResult die1 die2) rolls
  where
    singleResult die1 die2 (roll1, roll2) = compare (die1 !! roll1) (die2 !! roll2)

prob2win :: Fractional a => [Ordering] -> a
prob2win results =
    fromIntegral (length (filter (LT ==) results)) / fromIntegral (length results)

isNontransitive :: Ord a => [[a]] -> Bool
isNontransitive dieSet =
    let a = dieSet !! 0
        b = dieSet !! 1
        c = dieSet !! 2
    in
        (((prob2win $ result a b allRolls) > 0.5) || ((prob2win $ result a c allRolls) > 0.5)) &&
        (((prob2win $ result b a allRolls) > 0.5) || ((prob2win $ result b c allRolls) > 0.5)) &&
        (((prob2win $ result c a allRolls) > 0.5) || ((prob2win $ result c b allRolls) > 0.5))

allDice = combs 3 $ combsWithRep dieSides [1..pipN]

main = print . length $ filter id (map isNontransitive allDice)
