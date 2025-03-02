{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Defines reusable abstractions for testing RoundTrip properties of CBOR instances
module Test.Cardano.Ledger.Binary.RoundTrip
  ( roundTripSpec,
    roundTripFailureExpectation,
    roundTripExpectation,
    roundTripCborExpectation,
    roundTripAnnExpectation,
    embedTripSpec,
    embedTripExpectation,
    embedTripAnnExpectation,
    RoundTripFailure (..),
    Trip (..),
    mkTrip,
    cborTrip,
    roundTrip,
    roundTripTwiddled,
    roundTripAnn,
    roundTripAnnTwiddled,
    embedTrip,
    embedTripAnn,
    embedTripLabel,
  )
where

import Cardano.Ledger.Binary
import Control.Monad (guard)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy
import qualified Data.Text as Text
import Data.Typeable
import Test.Cardano.Ledger.Binary.TreeDiff (CBORBytes (..), showExpr, showHexBytesGrouped)
import Test.Cardano.Ledger.Binary.Twiddle (Twiddle (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (label)

-- =====================================================================

-- | Tests the roundtrip property using QuickCheck generators
roundTripSpec ::
  forall t.
  (Show t, Eq t, Typeable t, Arbitrary t) =>
  Version ->
  Trip t t ->
  Spec
roundTripSpec version trip =
  prop (show (typeRep $ Proxy @t)) $ roundTripExpectation version trip

-- | Tests the embedtrip property using QuickCheck generators
embedTripSpec ::
  forall a b.
  (Show a, Typeable a, Typeable b, Arbitrary a, HasCallStack) =>
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  Trip a b ->
  (b -> a -> Expectation) ->
  Spec
embedTripSpec encVersion decVersion trip f =
  prop ("From: " ++ show (typeRep $ Proxy @a) ++ " To " ++ show (typeRep $ Proxy @b)) $
    embedTripExpectation encVersion decVersion trip f

-- Tests that a decoder error happens
roundTripFailureExpectation :: forall a. (ToCBOR a, FromCBOR a) => Version -> a -> Expectation
roundTripFailureExpectation version x =
  case roundTrip version (cborTrip @a) x of
    Left _ -> pure ()
    Right _ ->
      expectationFailure $
        "Should not have deserialized: " ++ showExpr (CBORBytes (serialize' version x))

-- | Verify that round triping through the binary form holds.
--
-- In other words check that:
--
-- > deserialize version . serialize version === id
-- > serialize version . deserialize version . serialize version === serialize version
roundTripExpectation ::
  (Show t, Eq t, Typeable t, HasCallStack) =>
  Version ->
  Trip t t ->
  t ->
  Expectation
roundTripExpectation version trip t =
  case roundTrip version trip t of
    Left err -> expectationFailure $ "Failed to deserialize encoded: " ++ show err
    Right tDecoded -> tDecoded `shouldBe` t

roundTripCborExpectation ::
  forall t.
  (Show t, Eq t, ToCBOR t, FromCBOR t, HasCallStack) =>
  Version ->
  t ->
  Expectation
roundTripCborExpectation version = roundTripExpectation version (cborTrip @t)

roundTripAnnExpectation ::
  (Show t, Eq t, ToCBOR t, FromCBOR (Annotator t), HasCallStack) =>
  Version ->
  t ->
  Expectation
roundTripAnnExpectation version t =
  case roundTripAnn version t of
    Left err -> expectationFailure $ "Failed to deserialize encoded: " ++ show err
    Right tDecoded -> tDecoded `shouldBe` t

embedTripExpectation ::
  forall a b.
  (Typeable b, HasCallStack) =>
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  Trip a b ->
  (b -> a -> Expectation) ->
  a ->
  Expectation
embedTripExpectation encVersion decVersion trip f t =
  case embedTrip encVersion decVersion trip t of
    Left err -> expectationFailure $ "Failed to deserialize encoded: " ++ show err
    Right tDecoded -> f tDecoded t

embedTripAnnExpectation ::
  forall a b.
  (ToCBOR a, FromCBOR (Annotator b), HasCallStack) =>
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  (b -> a -> Expectation) ->
  a ->
  Expectation
embedTripAnnExpectation encVersion decVersion f a =
  case embedTripAnn encVersion decVersion a of
    Left err -> expectationFailure $ "Failed to deserialize encoded: " ++ show err
    Right b -> b `f` a

-- =====================================================================

data RoundTripFailure = RoundTripFailure
  { -- | Version that was used during encoding
    rtfEncoderVersion :: Version,
    -- | Version that was used during decoding
    rtfDecoderVersion :: Version,
    -- | Produced encoding
    rtfEncoding :: Encoding,
    -- | Serialized encoding using the version in this failure
    rtfEncodedBytes :: BSL.ByteString,
    -- | Re-serialized bytes, if there was a mismatch between the binary form and th
    -- reserialization of the data type.
    rtfReEncodedBytes :: Maybe (BSL.ByteString),
    -- | Error received while decoding the produced bytes and dropping the value. Normally
    -- it will be `Nothing`, unless the error produced did not match the
    -- `rtfDecoderError`, in which case it will be `Just` the error.
    rtfDropperError :: Maybe DecoderError,
    -- | Error received while decoding the produced bytes. It is possible for a dropper to
    -- produce an error, while decoder going through successfully, which constitues a test
    -- failure. In such a case this field will be `Nothing`, however `rtfDropperError`
    -- will be set to `Just`. Whenever both `rtfDropperError` and `rtfDecoderError` are
    -- `Nothing` it means that the decoding went though just fine, but there was a
    -- mismatch in the binary format, i.e. reserialization produced a mismatched result,
    -- in which case `rtfReEncodedBytes` will be set to `Just`
    rtfDecoderError :: Maybe DecoderError
  }

instance Show RoundTripFailure where
  show RoundTripFailure {..} =
    unlines $
      [ "Encoder Version: " ++ show rtfEncoderVersion,
        "Decoder Version: " ++ show rtfDecoderVersion,
        showMaybeDecoderError "Decoder" rtfDecoderError,
        showMaybeDecoderError "Dropper" rtfDropperError,
        prettyTerm
      ]
        ++ showHexBytesGrouped bytes
    where
      showMaybeDecoderError name = \case
        Nothing -> "No " ++ name ++ " error"
        Just err -> name ++ " error: " ++ showDecoderError err
      bytes = BSL.toStrict rtfEncodedBytes
      prettyTerm =
        case decodeFullDecoder' rtfDecoderVersion "Term" decodeTerm bytes of
          Left err -> "Could not decode as Term: " ++ show err
          Right term -> showExpr term

-- | A definition of a CBOR trip through binary representation of one type to
-- another. In this module this is called an embed. When a source and target type is the
-- exact same one then it would be a dual and is expected to round trip.
data Trip a b = Trip
  { tripEncoder :: a -> Encoding,
    tripDecoder :: forall s. Decoder s b,
    tripDropper :: forall s. Decoder s ()
  }

cborTrip :: forall a b. (ToCBOR a, FromCBOR b) => Trip a b
cborTrip = Trip toCBOR fromCBOR (dropCBOR (Proxy @b))

-- | Construct a `Trip` using encoder and decoder, with dropper set to the decoder which
-- drops the value
mkTrip :: forall a b. (a -> Encoding) -> (forall s. Decoder s b) -> Trip a b
mkTrip encoder decoder = Trip encoder decoder (() <$ decoder)

roundTrip :: forall t. Typeable t => Version -> Trip t t -> t -> Either RoundTripFailure t
roundTrip version trip val = do
  (val', encoding, encodedBytes) <- embedTripLabelExtra (typeLabel @t) version version trip val
  let reserialized = serializeEncoding version (tripEncoder trip val')
  if reserialized /= encodedBytes
    then
      Left $
        RoundTripFailure version version encoding encodedBytes (Just reserialized) Nothing Nothing
    else Right val'

roundTripTwiddled ::
  forall t.
  (Twiddle t, FromCBOR t) =>
  Version ->
  t ->
  Gen (Either RoundTripFailure t)
roundTripTwiddled version x = do
  tw <- twiddle version x
  pure (roundTrip version (Trip (const (encodeTerm tw)) fromCBOR (dropCBOR (Proxy @t))) x)

roundTripAnn :: (ToCBOR t, FromCBOR (Annotator t)) => Version -> t -> Either RoundTripFailure t
roundTripAnn v = embedTripAnn v v

roundTripAnnTwiddled ::
  (Twiddle t, FromCBOR (Annotator t)) => Version -> t -> Gen (Either RoundTripFailure t)
roundTripAnnTwiddled version x = do
  tw <- twiddle version x
  pure (decodeAnn version version (encodeTerm tw))

decodeAnn ::
  forall t.
  FromCBOR (Annotator t) =>
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  Encoding ->
  Either RoundTripFailure t
decodeAnn encVersion decVersion encoding =
  first (RoundTripFailure encVersion decVersion encoding encodedBytes Nothing Nothing . Just) $
    decodeFullAnnotator decVersion (label (Proxy @(Annotator t))) fromCBOR encodedBytes
  where
    encodedBytes = serializeEncoding encVersion encoding

embedTripLabel ::
  forall a b.
  Text.Text ->
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  Trip a b ->
  a ->
  Either RoundTripFailure b
embedTripLabel lbl encVersion decVersion trip s =
  (\(val, _, _) -> val) <$> embedTripLabelExtra lbl encVersion decVersion trip s

embedTripLabelExtra ::
  forall a b.
  Text.Text ->
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  Trip a b ->
  a ->
  Either RoundTripFailure (b, Encoding, BSL.ByteString)
embedTripLabelExtra lbl encVersion decVersion (Trip encoder decoder dropper) s =
  case decodeFullDecoder decVersion lbl decoder encodedBytes of
    Right val
      | Nothing <- dropperError -> Right (val, encoding, encodedBytes)
      | Just err <- dropperError ->
          Left $
            RoundTripFailure encVersion decVersion encoding encodedBytes Nothing (Just err) Nothing
    Left err ->
      let mErr = dropperError >>= \dropErr -> guard (dropErr /= err) >> dropperError
       in Left $
            RoundTripFailure encVersion decVersion encoding encodedBytes Nothing mErr (Just err)
  where
    encoding = encoder s
    encodedBytes = serializeEncoding encVersion encoding
    dropperError =
      case decodeFullDecoder decVersion lbl dropper encodedBytes of
        Left err -> Just err
        Right () -> Nothing

-- | Can we serialise a type, and then deserialise it as something else?
embedTrip ::
  forall a b.
  Typeable b =>
  -- | Version for the encoder
  Version ->
  -- | Version for the decoder
  Version ->
  Trip a b ->
  a ->
  Either RoundTripFailure b
embedTrip = embedTripLabel (Text.pack (show (typeRep $ Proxy @b)))

embedTripAnn ::
  forall a b.
  (ToCBOR a, FromCBOR (Annotator b)) =>
  Version ->
  Version ->
  a ->
  Either RoundTripFailure b
embedTripAnn encVersion decVersion = decodeAnn encVersion decVersion . toCBOR

typeLabel :: forall t. Typeable t => Text.Text
typeLabel = Text.pack (show (typeRep $ Proxy @t))
