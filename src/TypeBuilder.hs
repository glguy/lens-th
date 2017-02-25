{-# Language TemplateHaskell #-}
module TypeBuilder (generateType) where

import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Types
import ListUtils
import Control.Lens (Lens, Lens', Traversal, Traversal')
import Data.List
import qualified Data.Map as Map
import           Data.Map (Map)

data ConstraintNeeded = FunctorNeeded | ApplicativeNeeded

generateType :: LensConfig -> DatatypeInfo -> [AbstractLensLikeClause] -> TypeQ
generateType cfg di allcs =
  do (s,t,a,b) <- generateTypeComponents di allcs
     let mkT n xs  = pure (foldl AppT (ConT n) xs)
         simple    = cfgSimple cfg || s == t && a == b
     case constraintNeeded allcs of
       FunctorNeeded
         | simple    -> mkT ''Lens'      [s,  a  ]
         | otherwise -> mkT ''Lens       [s,t,a,b]
       ApplicativeNeeded
         | simple    -> mkT ''Traversal' [s,  a  ]
         | otherwise -> mkT ''Traversal  [s,t,a,b]


constraintNeeded :: [AbstractLensLikeClause] -> ConstraintNeeded
constraintNeeded xs
  | all (\allc -> length (allcFields allc) == 1) xs = FunctorNeeded
  | otherwise                                       = ApplicativeNeeded


mkFreshSubst :: [Name] -> Q (Map Name Type)
mkFreshSubst vs =
  sequence $ Map.fromList [ (v, VarT <$> newName (nameBase v ++ "'")) | v <- vs ]

generateTypeComponents ::
   DatatypeInfo ->
   [AbstractLensLikeClause] ->
   Q (Type,Type,Type,Type)
generateTypeComponents di allcs =

  do focusName <- newName "a"

     let focusType       = VarT focusName
         focusTypes      = concatMap fst fieldPartitions
         idleTypes       = concatMap snd fieldPartitions
         contexts        = concatMap constructorContext cons
         independentVars = (focusName : map tvName (datatypeVars di))
                        \\ freeVariables idleTypes `union`
                           freeVariables contexts

     sub  <- unify (focusType : focusTypes)
     sub' <- mkFreshSubst independentVars

     let s = applySubstitution sub  (datatypeType di)
         t = applySubstitution sub' s
         a = applySubstitution sub  focusType
         b = applySubstitution sub' a

     return (s,t,a,b)
  where
    cons = datatypeCons di
    conAndInfo = [ (ci,v)
                   | ci <- cons
                   , let Just v = find (\allc -> allcConName allc == constructorName ci)
                                          allcs
                   ]

    fieldPartitions =
       [ partitionSubset (allcFields allc) (constructorFields ci)
           | (ci,allc) <- conAndInfo ]

