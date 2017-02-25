module LensBuilder where

import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Data.List
import Data.Maybe
import ClauseBuilder
import TypeBuilder
import Types

makeLensForFields :: LensConfig -> Name -> (String -> Maybe String) -> DecsQ
makeLensForFields cfg tyCon fieldNamer =
  do di <- reifyDatatype tyCon
     makeLensForFields' cfg di fieldNamer

makeLensForField :: LensConfig -> Name -> Name -> String -> DecsQ
makeLensForField cfg tyCon fieldName lensName =
  do di <- reifyDatatype tyCon
     makeLensForField' cfg di fieldName lensName

makeLensForFieldsP :: LensConfig -> Name -> (Name -> Bool) -> String -> DecsQ
makeLensForFieldsP cfg tyCon fieldP name =
  do di <- reifyDatatype tyCon
     makeLensForFieldsP' cfg di fieldP name

------------------------------------------------------------------------

makeLensForFields' :: LensConfig -> DatatypeInfo -> (String -> Maybe String) -> DecsQ
makeLensForFields' cfg di fieldNamer = concat <$> sequence declss
  where
    fieldPredicate lensName fieldName = fieldNamer (nameBase fieldName) == Just lensName

    declss =
      [ makeLensForFieldsP' cfg di (fieldPredicate lensName) lensName
      | lensName <- allLenses ]

    allLenses
      = nub
      $ mapMaybe (fieldNamer . nameBase)
      $ concatMap (conFieldNames . constructorVariant)
      $ datatypeCons di


makeLensForFieldsP' :: LensConfig -> DatatypeInfo -> (Name -> Bool) -> String -> DecsQ
makeLensForFieldsP' cfg di fieldP name =
  makeLens' cfg di name (buildAllc <$> datatypeCons di)
  where
    buildAllc c =
      AbstractLensLikeClause
        (constructorName c)
        (length (constructorFields c))
        (findIndices fieldP (conFieldNames (constructorVariant c)))

conFieldNames :: ConstructorVariant -> [Name]
conFieldNames (RecordConstructor xs) = xs
conFieldNames _ = []

makeLensForField' :: LensConfig -> DatatypeInfo -> Name -> String -> DecsQ
makeLensForField' cfg di fieldName lensName =
  do let matches (RecordConstructor fields)
           | Just i <- elemIndex fieldName fields = [i]
         matches _ = []
         allcs = [ AbstractLensLikeClause
                     (constructorName c)
                     (length (constructorFields c))
                     (matches (constructorVariant c))
                 | c <- datatypeCons di ]
     makeLens' cfg di lensName allcs

makeLens' :: LensConfig -> DatatypeInfo -> String -> [AbstractLensLikeClause] -> DecsQ
makeLens' cfg di name xs = sequence (sigDecls ++ funDecls ++ pragmas)
  where
    name' = mkName name

    sigDecls | cfgSignatures cfg = [sigD name' (generateType cfg di xs)]
             | otherwise         = []

    funDecls = [funD name' (map generateClause xs)]

    pragmas  | cfgInline cfg = [pragInlD name' Inline FunLike AllPhases]
             | otherwise     = []
