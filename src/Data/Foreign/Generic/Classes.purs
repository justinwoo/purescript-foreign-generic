module Data.Foreign.Generic.Classes where

import Prelude
import Data.StrMap as S
import Control.Alt ((<|>))
import Control.Monad.Except (mapExcept)
import Data.Array (length, zipWith, (..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), fail, isNull, isUndefined, readArray, readBoolean, readChar, readInt, readNumber, readString, toForeign)
import Data.Foreign.Generic.Types (Options, SumEncoding(..))
import Data.Foreign.Index (index)
import Data.Generic.Rep (Argument(Argument), Constructor(Constructor), Field(Field), NoArguments(NoArguments), NoConstructors, Product(Product), Rec(Rec), Sum(Inr, Inl))
import Data.List (List(..), fromFoldable, null, singleton, toUnfoldable, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence)
import Type.Proxy (Proxy(..))

foreign import writeNull :: Foreign
foreign import writeUndefined :: Foreign

newtype Null a = Null (Maybe a)

derive instance newtypeNull :: Newtype (Null a) _
derive instance eqNull :: (Eq a) => Eq (Null a)
derive instance ordNull :: (Ord a) => Ord (Null a)

instance showNull :: (Show a) => Show (Null a) where
  show x = "(Null " <> show (unwrap x) <> ")"

newtype Undefined a = Undefined (Maybe a)

derive instance newtypeUndefined :: Newtype (Undefined a) _
derive instance eqUndefined :: (Eq a) => Eq (Undefined a)
derive instance ordUndefined :: (Ord a) => Ord (Undefined a)

instance showUndefined :: (Show a) => Show (Undefined a) where
  show x = "(Undefined " <> show (unwrap x) <> ")"

newtype NullOrUndefined a = NullOrUndefined (Maybe a)

derive instance newtypeNullOrUndefined :: Newtype (NullOrUndefined a) _
derive instance eqNullOrUndefined :: (Eq a) => Eq (NullOrUndefined a)
derive instance ordNullOrUndefined :: (Ord a) => Ord (NullOrUndefined a)

instance showNullOrUndefined :: (Show a) => Show (NullOrUndefined a) where
  show x = "(NullOrUndefined " <> show (unwrap x) <> ")"

class Decode a where
  read :: Foreign -> F a

instance foreignDecode :: Decode Foreign where
  read = pure

instance stringDecode :: Decode String where
  read = readString

instance charDecode :: Decode Char where
  read = readChar

instance booleanDecode :: Decode Boolean where
  read = readBoolean

instance numberDecode :: Decode Number where
  read = readNumber

instance intDecode :: Decode Int where
  read = readInt

instance arrayDecode :: Decode a => Decode (Array a) where
  read = readArray >=> readElements where
    readElements :: Array Foreign -> F (Array a)
    readElements arr = sequence (zipWith readElement (0 .. length arr) arr)

    readElement :: Int -> Foreign -> F a
    readElement i value = mapExcept (lmap (map (ErrorAtIndex i))) (read value)

instance nullDecode :: Decode a => Decode (Null a) where
  read x
    | isNull x = pure (Null Nothing)
    | otherwise = Null <<< Just <$> read x

instance undefinedDecode :: Decode a => Decode (Undefined a) where
  read x
    | isUndefined x = pure (Undefined Nothing)
    | otherwise = Undefined <<< Just <$> read x

instance nulloOrUndefinedDecode :: Decode a => Decode (NullOrUndefined a) where
  read x
    | isNull x || isUndefined x = pure (NullOrUndefined Nothing)
    | otherwise = NullOrUndefined <<< Just <$> read x

class Encode a where
  write :: a -> Foreign

instance foreignEncode :: Encode Foreign where
  write = id

instance stringEncode :: Encode String where
  write = toForeign

instance charEncode :: Encode Char where
  write = toForeign

instance booleanEncode :: Encode Boolean where
  write = toForeign

instance numberEncode :: Encode Number where
  write = toForeign

instance intEncode :: Encode Int where
  write = toForeign

instance arrayEncode :: Encode a => Encode (Array a) where
  write = toForeign <<< map write

instance nullAsForeign :: Encode a => Encode (Null a) where
  write (Null a) = maybe writeNull write a

instance undefinedAsForeign :: Encode a => Encode (Undefined a) where
  write (Undefined a) = maybe writeUndefined write a

instance nullOrUndefinedAsForeign :: Encode a => Encode (NullOrUndefined a) where
  write (NullOrUndefined a) = write (Null a)

class GenericDecode a where
  decodeOpts :: Options -> Foreign -> F a

class GenericEncode a where
  encodeOpts :: Options -> a -> Foreign

class GenericDecodeArgs a where
  decodeArgs :: Int -> List Foreign -> F { result :: a
                                         , rest :: List Foreign
                                         , next :: Int
                                         }

class GenericEncodeArgs a where
  encodeArgs :: a -> List Foreign

class GenericDecodeFields a where
  decodeFields :: Foreign -> F a

class GenericEncodeFields a where
  encodeFields :: a -> S.StrMap Foreign

class GenericCountArgs a where
  countArgs :: Proxy a -> Either a Int

instance genericDecodeNoConstructors :: GenericDecode NoConstructors where
  decodeOpts opts _ = fail (ForeignError "No constructors")

instance genericEncodeNoConstructors :: GenericEncode NoConstructors where
  encodeOpts opts a = encodeOpts opts a

instance genericDecodeConstructor
  :: (IsSymbol name, GenericDecodeArgs rep, GenericCountArgs rep)
  => GenericDecode (Constructor name rep) where
  decodeOpts opts f =
      if opts.unwrapSingleConstructors
        then Constructor <$> readArguments f
        else case opts.sumEncoding of
               TaggedObject { tagFieldName, contentsFieldName } -> do
                 tag <- mapExcept (lmap (map (ErrorAtProperty contentsFieldName))) do
                   tag <- index f tagFieldName >>= readString
                   unless (tag == ctorName) $
                     fail (ForeignError ("Expected " <> show ctorName <> " tag"))
                   pure tag
                 args <- mapExcept (lmap (map (ErrorAtProperty contentsFieldName)))
                           (index f contentsFieldName >>= readArguments)
                 pure (Constructor args)
    where
      ctorName = reflectSymbol (SProxy :: SProxy name)

      numArgs = countArgs (Proxy :: Proxy rep)

      readArguments args =
        case numArgs of
          Left a -> pure a
          Right 1 | opts.unwrapSingleArguments -> do
            { result, rest } <- decodeArgs 0 (singleton args)
            unless (null rest) $
              fail (ForeignError "Expected a single argument")
            pure result
          Right n -> do
            vals <- readArray args
            { result, rest } <- decodeArgs 0 (fromFoldable vals)
            unless (null rest) $
              fail (ForeignError ("Expected " <> show n <> " constructor arguments"))
            pure result

instance genericEncodeConstructor
  :: (IsSymbol name, GenericEncodeArgs rep)
  => GenericEncode (Constructor name rep) where
  encodeOpts opts (Constructor args) =
      if opts.unwrapSingleConstructors
        then maybe (toForeign {}) toForeign (encodeArgsArray args)
        else case opts.sumEncoding of
               TaggedObject { tagFieldName, contentsFieldName } ->
                 toForeign (S.singleton tagFieldName (toForeign ctorName)
                           `S.union` maybe S.empty (S.singleton contentsFieldName) (encodeArgsArray args))

    where
      ctorName = reflectSymbol (SProxy :: SProxy name)

      encodeArgsArray :: rep -> Maybe Foreign
      encodeArgsArray = unwrapArguments <<< toUnfoldable <<< encodeArgs

      unwrapArguments :: Array Foreign -> Maybe Foreign
      unwrapArguments [] = Nothing
      unwrapArguments [x] | opts.unwrapSingleArguments = Just x
      unwrapArguments xs = Just (toForeign xs)

instance genericDecodeSum
  :: (GenericDecode a, GenericDecode b)
  => GenericDecode (Sum a b) where
  decodeOpts opts f = Inl <$> decodeOpts opts' f <|> Inr <$> decodeOpts opts' f
    where
      -- Reuse the unwrapSingleConstructors flag, since we cannot have a single
      -- constructor at this point anyway.
      opts' = opts { unwrapSingleConstructors = false }

instance genericEncodeSum
  :: (GenericEncode a, GenericEncode b)
  => GenericEncode (Sum a b) where
  encodeOpts opts (Inl a) = encodeOpts (opts { unwrapSingleConstructors = false }) a
  encodeOpts opts (Inr b) = encodeOpts (opts { unwrapSingleConstructors = false }) b

instance genericDecodeArgsNoArguments :: GenericDecodeArgs NoArguments where
  decodeArgs i Nil = pure { result: NoArguments, rest: Nil, next: i }
  decodeArgs _ _ = fail (ForeignError "Too many constructor arguments")

instance genericEncodeArgsNoArguments :: GenericEncodeArgs NoArguments where
  encodeArgs _ = mempty

instance genericDecodeArgsArgument
  :: Decode a
  => GenericDecodeArgs (Argument a) where
  decodeArgs i (x : xs) = do
    a <- mapExcept (lmap (map (ErrorAtIndex i))) (read x)
    pure { result: Argument a, rest: xs, next: i + 1 }
  decodeArgs _ _ = fail (ForeignError "Not enough constructor arguments")

instance genericEncodeArgsArgument
  :: Encode a
  => GenericEncodeArgs (Argument a) where
  encodeArgs (Argument a) = singleton (write a)

instance genericDecodeArgsProduct
  :: (GenericDecodeArgs a, GenericDecodeArgs b)
  => GenericDecodeArgs (Product a b) where
  decodeArgs i xs = do
    { result: resA, rest: xs1, next: i1 } <- decodeArgs i xs
    { result: resB, rest, next } <- decodeArgs i1 xs1
    pure { result: Product resA resB, rest, next }

instance genericEncodeArgsProduct
  :: (GenericEncodeArgs a, GenericEncodeArgs b)
  => GenericEncodeArgs (Product a b) where
  encodeArgs (Product a b) = encodeArgs a <> encodeArgs b

instance genericDecodeArgsRec
  :: GenericDecodeFields fields
  => GenericDecodeArgs (Rec fields) where
  decodeArgs i (x : xs) = do
    fields <- mapExcept (lmap (map (ErrorAtIndex i))) (decodeFields x)
    pure { result: Rec fields, rest: xs, next: i + 1 }
  decodeArgs _ _ = fail (ForeignError "Not enough constructor arguments")

instance genericEncodeArgsRec
  :: GenericEncodeFields fields
  => GenericEncodeArgs (Rec fields) where
  encodeArgs (Rec fs) = singleton (toForeign (encodeFields fs))

instance genericDecodeFieldsField
  :: (IsSymbol name, Decode a)
  => GenericDecodeFields (Field name a) where
  decodeFields x = do
    let name = reflectSymbol (SProxy :: SProxy name)
    -- If `name` field doesn't exist, then `y` will be `undefined`.
    Field <$> (index x name >>= read)

instance genericEncodeFieldsField
  :: (IsSymbol name, Encode a)
  => GenericEncodeFields (Field name a) where
  encodeFields (Field a) =
    let name = reflectSymbol (SProxy :: SProxy name)
    in S.singleton name (write a)

instance genericDecodeFieldsProduct
  :: (GenericDecodeFields a, GenericDecodeFields b)
  => GenericDecodeFields (Product a b) where
  decodeFields x = Product <$> decodeFields x <*> decodeFields x

instance genericEncodeFieldsProduct
  :: (GenericEncodeFields a, GenericEncodeFields b)
  => GenericEncodeFields (Product a b) where
  encodeFields (Product a b) = encodeFields a `S.union` encodeFields b

instance genericCountArgsNoArguments :: GenericCountArgs NoArguments where
  countArgs _ = Left NoArguments

instance genericCountArgsArgument :: GenericCountArgs (Argument a) where
  countArgs _ = Right 1

instance genericCountArgsProduct
  :: (GenericCountArgs a, GenericCountArgs b)
  => GenericCountArgs (Product a b) where
  countArgs _ =
    case countArgs (Proxy :: Proxy a), countArgs (Proxy :: Proxy b) of
      Left a , Left b  -> Left (Product a b)
      Left _ , Right n -> Right n
      Right n, Left _  -> Right n
      Right n, Right m -> Right (n + m)

instance genericCountArgsRec :: GenericCountArgs (Rec fields) where
  countArgs _ = Right 1
