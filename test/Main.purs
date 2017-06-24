module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON)
import Data.Foreign.Generic.Types (Options)
import Data.Foreign.JSON (parseJSON)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.String (toLower, toUpper)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import SumEncoding (class GenericDecodeUnarySum, class GenericEncodeUnarySum, genericDecodeUnaryConstructors, genericEncodeUnaryConstructors)
import Test.Assert (assert, assert', ASSERT)
import Test.Types (Fruit(..), IntList(..), RecordTest(..), Tree(..), TupleArray(..), UndefinedTest(..))

buildTree :: forall a. (a -> TupleArray a a) -> Int -> a -> Tree a
buildTree _ 0 a = Leaf a
buildTree f n a = Branch $ buildTree (bimap f f) (n - 1) (f a)

-- A balanced binary tree of depth N
makeTree :: Int -> Tree Int
makeTree n = buildTree (\i -> TupleArray (Tuple (2 * i) (2 * i + 1))) n 0

throw :: forall eff. String -> Eff (assert :: ASSERT | eff) Unit
throw = flip assert' false

testRoundTrip
  :: ∀ a eff
   . Eq a
  => Decode a
  => Encode a
  => a
  -> Eff ( console :: CONSOLE
         , assert :: ASSERT
         | eff
         ) Unit
testRoundTrip x = do
  let json = encodeJSON x
  log json
  case runExcept (decodeJSON json) of
    Right y -> assert (x == y)
    Left err -> throw (show err)

testOption
  :: ∀ a rep eff
   . Eq a
  => Generic a rep
  => GenericEncodeUnarySum rep
  => GenericDecodeUnarySum rep
  => Options
  -> String
  -> a
  -> Eff ( console :: CONSOLE
         , assert :: ASSERT
         | eff
         ) Unit
testOption options string value = do
  let json = unsafeStringify $ genericEncodeUnaryConstructors options value
  log json
  case runExcept $ Tuple <$> decode' json <*> decode' string of
    Right (Tuple x y) -> assert (value == y && value == x)
    Left err -> throw (show err)
  where
    decode' = genericDecodeUnaryConstructors options <=< parseJSON

testUnaryConstructorLiteral :: forall e.
  Eff
    ( console :: CONSOLE
    , assert :: ASSERT
    | e
    )
    Unit
testUnaryConstructorLiteral = do
    testOption (makeCasingOptions toUpper) "\"FrIkAnDeL\"" Frikandel
    testOption (makeCasingOptions toLower) "\"FrIkAnDeL\"" Frikandel
  where
    makeCasingOptions f = defaultOptions
      { constructorTagTransform = f }

main :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
main = do
  testRoundTrip (RecordTest { foo: 1, bar: "test", baz: 'a' })
  testRoundTrip (Cons 1 (Cons 2 (Cons 3 Nil)))
  testRoundTrip (UndefinedTest {a: NullOrUndefined (Just "test")})
  testRoundTrip (UndefinedTest {a: NullOrUndefined Nothing})
  testRoundTrip [NullOrUndefined (Just "test")]
  testRoundTrip [NullOrUndefined (Nothing :: Maybe String)]
  testRoundTrip (Apple)
  testRoundTrip (makeTree 0)
  testRoundTrip (makeTree 5)
  testUnaryConstructorLiteral
