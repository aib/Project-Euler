import Math.NumberTheory.ArithmeticFunctions
import Data.Set as S
import Data.Map as M

properDivSum x = sum . S.toList . (S.delete x) . divisors $ (x :: Integer)

pdsMap = M.fromList [(n,properDivSum n) | n <- [1..9999]]

amicable n = (pds /= return n) && (pds >>= ((flip M.lookup) pdsMap)) == return n
  where
    pds = M.lookup n pdsMap

main = print $
    sum $ Prelude.map fst $ M.toList (M.filterWithKey (const . amicable) pdsMap)
