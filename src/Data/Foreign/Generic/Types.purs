module Data.Foreign.Generic.Types where

type Options =
  { sumEncoding :: SumEncoding
  , unwrapSingleConstructors :: Boolean
  , unwrapSingleArguments :: Boolean
  , constructorTagTransform :: String -> String
  }

-- | The encoding of sum types for your type.
-- | `TaggedObject`s will be encoded in the form `{ [tagFieldName]: "ConstructorTag", [contentsFieldName]: "Contents"}`.
data SumEncoding
  = TaggedObject
    { tagFieldName :: String
    , contentsFieldName :: String
    }
