import Control.Applicative

main = print $
    liftA2 (-) ((^2).sum) (sum . map (^2)) [1..100]
