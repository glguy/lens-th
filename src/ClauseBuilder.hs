{-# Language RecordWildCards, TemplateHaskell #-}

module ClauseBuilder where

import Language.Haskell.TH
import Types
import ListUtils

generateClause :: AbstractLensLikeClause -> ClauseQ
generateClause allc@AbstractLensLikeClause{..} =
  do f   <- newName "f"
     vs  <- newNames "x" allcConArity

     let mk = buildMk allc vs
         es = [ [| $(varE f) $(varE (vs !! i)) |] | i <- allcFields ]
         body = case es of
                  []   -> [| pure $mk |]
                  x:xs -> foldl (\acc e -> [| $acc <*> $e |])
                                [| $mk <$> $x |]
                                xs

     let fPat | null es   = wildP -- avoids unused variable warning
              | otherwise = varP f

     clause [fPat, conP allcConName (map varP vs)] (normalB body) []

-- | Generate an expression that takes the "new" values, if any
-- and fills in the given old values otherwise. The generator
-- eta-reduces the result.
buildMk :: AbstractLensLikeClause -> [Name] -> ExpQ
buildMk AbstractLensLikeClause{..} vs =
  simplify (reverse allcFields) allcConArity

  where
    -- implementation of eta reduction
    simplify (x:xs) n | x == n-1 = simplify xs (n-1)
    simplify xs n = generate (reverse xs) n

    generate xs n =
      do ys <- newNames "y" (length xs)
         lamE' (map varP ys)
           $ foldl appE (conE allcConName)
           $ map varE
           $ applyReplacement (zip xs ys)
           $ take n vs

-- | Generate @count@ new 'Name's given a base name.
newNames :: String {- ^ base name -} -> Int {- ^ count -} -> Q [Name]
newNames base n = sequence [ newName (base++show i) | i <- [0..n-1] ]

-- | Smart 'lamE' that avoids generating a lambda expression without
-- any parameters.
lamE' :: [PatQ] -> ExpQ -> ExpQ
lamE' [] = id
lamE' xs = lamE xs
