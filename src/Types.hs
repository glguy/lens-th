module Types where

import Language.Haskell.TH

data AbstractLensLikeClause = AbstractLensLikeClause
  { allcConName  :: Name
  , allcConArity :: Int
  , allcFields   :: [Int] -- Zero-based, sorted, unique
  }

data LensConfig = LensConfig
  { cfgSimple     :: Bool
  , cfgSignatures :: Bool
  , cfgInline     :: Bool
  }
  deriving Show

defaultConfig :: LensConfig
defaultConfig = LensConfig
  { cfgSimple     = False
  , cfgSignatures = True
  , cfgInline     = True
  }
