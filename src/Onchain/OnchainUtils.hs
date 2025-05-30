module Onchain.OnchainUtils where

import PlutusLedgerApi.V1.Value (AssetClass (..), assetClassValueOf)
import PlutusTx.Builtins (serialiseData)
import PlutusLedgerApi.V3

-- **  Helper Functions

-- | Converts a typed lambda to untyped
mkUntypedLambda ::
  (ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinUnit)
mkUntypedLambda f c = check $ f (parseData c "Invalid context")
 where
  parseData mdata message = case fromBuiltinData mdata of
    Just d -> d
    _ -> traceError message
{-# INLINEABLE mkUntypedLambda #-}

-- **  Helper Functions

isMintingNFT :: AssetClass -> Value -> Bool
isMintingNFT ac txInfoMint = traceIfFalse "NFT not minted" $ assetClassValueOf txInfoMint ac #== 1
{-# INLINEABLE isMintingNFT #-}

isBurningNFT :: AssetClass -> Value -> Bool
isBurningNFT ac txInfoMint = traceIfFalse "NFT not burned" $ assetClassValueOf txInfoMint ac #== pnegate 1
{-# INLINEABLE isBurningNFT #-}

tokenNameFromTxOutRef :: TxId -> Integer -> TokenName
tokenNameFromTxOutRef (TxId txIdbs) txIdx = TokenName (takeByteString 28 $ blake2b_256 (txIdbs #<> (serialiseData . toBuiltinData) txIdx))
{-# INLINEABLE tokenNameFromTxOutRef #-}

generateRefAndUserAC :: AssetClass -> (AssetClass, AssetClass)
generateRefAndUserAC (AssetClass (ac, TokenName bs)) = (AssetClass (ac, TokenName (refTokenPrefixBS #<> bs)), AssetClass (ac, TokenName (userTokenPrefixBS #<> bs)))
{-# INLINEABLE generateRefAndUserAC #-}

type ValueConstraint = Value -> Bool
type AddressConstraint = Address -> Bool

isGivenInlineDatum :: (ToData a) => a -> OutputDatum -> Bool
isGivenInlineDatum datum outdat = case outdat of
  OutputDatum da -> toBuiltinData datum #== getDatum da
  _ -> trace "Datum must exsist and must be inlined" False
{-# INLINEABLE isGivenInlineDatum #-}

isTxOutWithInlineDatumAnd :: (ToData a) => a -> ValueConstraint -> AddressConstraint -> TxOut -> Bool
isTxOutWithInlineDatumAnd datum toValue toAddress tout =
  pand
    [ toValue (txOutValue tout)
    , toAddress (txOutAddress tout)
    , isGivenInlineDatum datum (txOutDatum tout)
    ]
{-# INLINEABLE isTxOutWithInlineDatumAnd #-}

hasTxOutWithInlineDatumAnd :: (ToData a) => a -> ValueConstraint -> AddressConstraint -> [TxOut] -> Bool
hasTxOutWithInlineDatumAnd datum toValue toAddress = pany (isTxOutWithInlineDatumAnd datum toValue toAddress)
{-# INLINEABLE hasTxOutWithInlineDatumAnd #-}

-- -- | Helper function to check if a 'TxOut' contains exactly 1 quantity of an AssetClass
-- outHas1of :: TxOut -> AssetClass -> Bool
-- outHas1of tout ac = assetClassValueOf (txOutValue tout) ac #== 1
-- {-# INLINEABLE outHas1of #-}

-- -- | Helper function to check if a 'TxInInfo' contains exactly 1 quantity of an AssetClass
-- inputHas1of :: TxInInfo -> AssetClass -> Bool
-- inputHas1of = outHas1of . txInInfoResolved
-- {-# INLINEABLE inputHas1of #-}

noConstraint :: b -> Bool
noConstraint = const True
{-# INLINEABLE noConstraint #-}

isTxOutWith :: ValueConstraint -> AddressConstraint -> TxOut -> Bool
isTxOutWith toValue toAddress txout = toValue (txOutValue txout) && toAddress (txOutAddress txout)
{-# INLINEABLE isTxOutWith #-}

hasTxOutWith :: ValueConstraint -> AddressConstraint -> [TxOut] -> Bool
hasTxOutWith toValue addr = pany (isTxOutWith toValue addr)
{-# INLINEABLE hasTxOutWith #-}

hasTxInWith :: ValueConstraint -> AddressConstraint -> [TxInInfo] -> Bool
hasTxInWith toValue addr = hasTxOutWith toValue addr . map txInInfoResolved
{-# INLINEABLE hasTxInWith #-}

-- | Helper function: check that the inputs contain a given token
hasTxInWithToken :: AssetClass -> [TxInInfo] -> Bool
hasTxInWithToken tokenId = hasTxInWith ((#== 1) . (`assetClassValueOf` tokenId)) noConstraint
{-# INLINEABLE hasTxInWithToken #-}

-- TODO update with new builtins

integerToBs24 :: Integer -> BuiltinByteString
integerToBs24 = dropByteString 1 . serialiseData . toBuiltinData -- Removing First Byte  (works for value > 24)
{-# INLINEABLE integerToBs24 #-}

refTokenPrefixBS :: BuiltinByteString
refTokenPrefixBS = integerToBs24 (0x000643b0 :: Integer) -- TODO update with new builtins
{-# INLINEABLE refTokenPrefixBS #-}

userTokenPrefixBS :: BuiltinByteString
userTokenPrefixBS = integerToBs24 (0x000de140 :: Integer) -- TODO update with new builtins
{-# INLINEABLE userTokenPrefixBS #-}
