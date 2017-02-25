module ListUtils where

applyReplacement :: [(Int,a)] -> [a] -> [a]
applyReplacement = go 0
  where
    go _ [] ys = ys
    go i ((ix,x):ixs) (_:ys)
      | i == ix = x : go (i+1) ixs ys
    go i ixs (y:ys) = y : go (i+1) ixs ys
    go _ _ _ = error "mergeSubset: mismatched arguments"

selectSubset :: [Int] -> [a] -> [a]
selectSubset = go 0
  where
    go i (ix:ixs) (x:xs) | i == ix = x : go (i+1) ixs xs
    go i ixs (_:xs) = go (i+1) ixs xs
    go _ [] _ = []
    go _ _ _ = error "selectSubset: bad subset"

partitionSubset :: [Int] -> [a] -> ([a], [a])
partitionSubset ixs_ = go 0 ixs_
  where
    go i (j:js) (x:xs)
      | i == j     = case go (i+1) js xs of
                       (l,r) -> (x:l,r)
    go i js (x:xs) = case go (i+1) js xs of
                       (l,r) -> (l,x:r)
    go _ _ []      = ([],[])
