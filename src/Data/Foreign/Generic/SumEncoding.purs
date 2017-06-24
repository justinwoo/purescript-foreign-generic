module SumEncoding where

import Prelude

import Control.Alt ((<|>))
import Data.Foreign (F, Foreign, ForeignError(..), fail, readString, toForeign)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

genericDecodeUnaryConstructors
  :: forall a rep
   . Generic a rep
  => GenericDecodeUnarySum rep
  => Options
  -> Foreign
  -> F a
genericDecodeUnaryConstructors opts = map to <<< decodeSum opts

genericEncodeUnaryConstructors
  :: forall a rep
   . Generic a rep
  => GenericEncodeUnarySum rep
  => Options
  -> a
  -> Foreign
genericEncodeUnaryConstructors opts = encodeSum opts <<< from

class GenericDecodeUnarySum a where
  decodeSum :: Options -> Foreign -> F a

class GenericEncodeUnarySum a where
  encodeSum :: Options -> a -> Foreign

instance sumGenericDecodeUnarySum
  :: (GenericDecodeUnarySum a, GenericDecodeUnarySum b)
  => GenericDecodeUnarySum (Sum a b) where
  decodeSum opts f = Inl <$> decodeSum opts f <|> Inr <$> decodeSum opts f

instance ctorNoArgsGenericDecodeUnarySum
  :: (IsSymbol name)
  => GenericDecodeUnarySum (Constructor name NoArguments) where
  decodeSum {constructorTagTransform} f = do
    tag <- constructorTagTransform <$> readString f
    unless (constructorTagTransform tag == ctorName) $
      fail (ForeignError ("Expected " <> show ctorName <> " tag for unary constructor literal " <> ctorName))
    pure $ Constructor NoArguments
    where
      ctorName = constructorTagTransform $ reflectSymbol (SProxy :: SProxy name)

instance sumGenericEncodeUnarySum
  :: (GenericEncodeUnarySum a, GenericEncodeUnarySum b)
  => GenericEncodeUnarySum (Sum a b) where
  encodeSum opts (Inl a) = encodeSum opts a
  encodeSum opts (Inr b) = encodeSum opts b

instance ctorNoArgsGenericEncodeUnarySum
  :: (IsSymbol name)
  => GenericEncodeUnarySum (Constructor name NoArguments) where
  encodeSum {constructorTagTransform} _ = toForeign ctorName
    where
      ctorName = constructorTagTransform $ reflectSymbol (SProxy :: SProxy name)
