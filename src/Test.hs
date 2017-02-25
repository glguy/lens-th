{-# Language RankNTypes, GADTs, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Test where

import LensBuilder
import Types
import Control.Lens

data Two a b c where
  Two :: Show a => { field1 :: a, field2 :: Int } -> Two a () c
  Y   :: { fieldY :: c } -> Two a b c
  Z   :: { fieldZ :: b } -> Two a b c

data Phantom a = Phantom { fieldP :: Int }

data MergeTest1 a = M1 { m1, m2 :: a} | M2 { m3 :: a }
data MergeTest2 a = M3 { m4 :: a} | M4 { m5 :: a }

concat <$> sequence
      [ makeLensForFields  defaultConfig ''Two (\n -> Just (n ++ "Lens"))
      , makeLensForFieldsP defaultConfig ''Two (`elem` ['field1,'field2]) "f3"
      , makeLensForFieldsP defaultConfig ''Two (const False) "f4"
      , makeLensForFieldsP defaultConfig ''Two (`elem` ['field1,'fieldZ]) "f5"
      , makeLensForFields  defaultConfig ''Phantom (\n -> Just (n ++ "Lens"))
      , makeLensForFields  defaultConfig ''MergeTest1 (\_ -> Just "mergeTraversal1")
      , makeLensForFields  defaultConfig ''MergeTest2 (\_ -> Just "mergeTraversal2")
      ]

-- Can't change type due to @Show a@
checkField1Lens :: Traversal' (Two a b c) a
checkField1Lens = field1Lens

checkField2Lens :: Traversal' (Two a b c) Int
checkField2Lens = field2Lens

checkFieldYLens :: Traversal (Two a b c) (Two a b c') c c'
checkFieldYLens = fieldYLens

-- Can't change due to equality constraint on b in 'Two'
checkFieldZLens :: Traversal' (Two a b c) b
checkFieldZLens = fieldZLens

checkFieldPLens :: Lens (Phantom a) (Phantom a') Int Int
checkFieldPLens = fieldPLens

checkF3 :: Traversal' (Two Int b c) Int
checkF3 = f3

checkF4 :: Traversal (Two a b c) (Two a b c) d d'
checkF4 = f4

checkF5 :: Traversal' (Two a a c) a
checkF5 = f5

checkMergeTraversal1 :: Traversal (MergeTest1 a) (MergeTest1 a') a a'
checkMergeTraversal1 = mergeTraversal1

checkMergeTraversal2 :: Lens (MergeTest2 a) (MergeTest2 a') a a'
checkMergeTraversal2 = mergeTraversal2


data Pair a b = Pair { proj1 :: a, proj2 :: b }

makeLensForFields defaultConfig ''Pair (\_ -> Just "pairBoth")

