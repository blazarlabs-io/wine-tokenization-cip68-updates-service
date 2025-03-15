module Onchain.Types where

import PlutusTx (makeIsDataSchemaIndexed, makeLift)

import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V2 (PubKeyHash)
import PlutusLedgerApi.V3 (Map, TxOutRef)
import PlutusTx.AssocMap (lookup, safeFromList)
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Prelude qualified as P

type Metadata = Map BuiltinByteString BuiltinByteString

metadataVersion :: Integer
metadataVersion = 1

data BatchQuantity
    = BatchQuantity
    { initialQuantity :: Integer
    , remainingQuantity :: Integer
    }
    deriving (Generic, Show)

deriving anyclass instance HasBlueprintDefinition BatchQuantity

instance P.Eq BatchQuantity where
    (==) :: BatchQuantity -> BatchQuantity -> Bool
    (==) (BatchQuantity i r) (BatchQuantity i' r') = i #== i' && r #== r'

makeIsDataSchemaIndexed ''BatchQuantity [('BatchQuantity, 0)]

data WineTokenType
    = BatchToken BatchQuantity
    | BottleToken AssetClass
    deriving (Generic, Show)

deriving anyclass instance HasBlueprintDefinition WineTokenType

makeIsDataSchemaIndexed ''WineTokenType [('BatchToken, 0), ('BottleToken, 0)]

instance P.Eq WineTokenType where
    (==) :: WineTokenType -> WineTokenType -> Bool
    (==) (BatchToken q1) (BatchToken q2) = q1 #== q2
    (==) (BottleToken ac1) (BottleToken ac2) = ac1 #== ac2
    (==) _ _ = False

data WineTokenMeta
    = WineTokenMeta
    { wiName :: BuiltinByteString
    , wiDesc :: BuiltinByteString
    , wiImageURI :: BuiltinByteString
    }
    deriving (Generic, Show)

deriving anyclass instance HasBlueprintDefinition WineTokenMeta

makeIsDataSchemaIndexed ''WineTokenMeta [('WineTokenMeta, 0)]

data WineTokenData
    = WineTokenData
    { wdInfoURI :: BuiltinByteString
    , wdMaybeDataURI :: Maybe BuiltinByteString
    , wdMaybeIncriptionsURI :: Maybe BuiltinByteString
    }
    deriving (Generic, Show)

deriving anyclass instance HasBlueprintDefinition WineTokenData

makeIsDataSchemaIndexed ''WineTokenData [('WineTokenData, 0)]

data WineToken
    = WineToken
    { wtMeta :: WineTokenMeta
    , wtData :: WineTokenData
    , wdTokenType :: WineTokenType
    }
    deriving (Generic, Show)

deriving anyclass instance HasBlueprintDefinition WineToken

makeIsDataSchemaIndexed ''WineToken [('WineToken, 0)]

{- | The datum datatype which should be locked with the batch ref NFT.
| This datatype is following the CIP-68 Datum Metadata Standard.
-}
data WineTokenCIP68Datum = WineTokenCIP68Datum
    { metadata :: Metadata --- ^  Map k v (where k and v arr  UTF-8 encoded @BuiltinByteString@s)
    , version :: Integer --- ^ version of CIP-68 Datum Metadata Standard.
    , extra :: WineTokenType --- ^ Plutus data
    }
    deriving (Generic, Show)

deriving anyclass instance HasBlueprintDefinition WineTokenCIP68Datum

makeIsDataSchemaIndexed ''WineTokenCIP68Datum [('WineTokenCIP68Datum, 0)]

-- |  Type to indicate the parameters of the Wine Minting Policy.
newtype WineMintingParams
    = WineMintingParams
    { adminPkh :: PubKeyHash
    }
    deriving (Generic, Show)

deriving anyclass instance HasBlueprintDefinition WineMintingParams

makeIsDataSchemaIndexed ''WineMintingParams [('WineMintingParams, 0)]
makeLift ''WineMintingParams

-- | Custom redeemer type to indicate minting and spending mode.
data WineMintingRedeemer
    = MintWineToken TxOutRef WineToken
    | Burn AssetClass
    | Update WineToken
    | MintBottle
    deriving (Generic, Show)

deriving anyclass instance HasBlueprintDefinition WineMintingRedeemer

makeIsDataSchemaIndexed ''WineMintingRedeemer [('MintWineToken, 0), ('Burn, 1), ('Update, 2), ('MintBottle, 3)]

----------------
-- NFTs METADATA
----------------

-- | Make a @WineBatchDatum@
mkWineCIP68Datum ::
    WineToken ->
    WineTokenCIP68Datum
mkWineCIP68Datum (WineToken WineTokenMeta{..} WineTokenData{..} wdTokenType) =
    WineTokenCIP68Datum
        { metadata =
            safeFromList $
                [ (encodeUtf8 "description", wiDesc)
                , (encodeUtf8 "image", wiImageURI)
                , (encodeUtf8 "name", wiName)
                , (encodeUtf8 "batch_info", wdInfoURI)
                ]
                    #<> ( case wdMaybeDataURI of
                            Nothing -> []
                            Just dataURI -> [(encodeUtf8 "batch_data", dataURI)]
                        )
                    #<> ( case wdMaybeIncriptionsURI of
                            Nothing -> []
                            Just inscriptionsURI -> [(encodeUtf8 "inscr_data", inscriptionsURI)]
                        )
        , version = metadataVersion
        , extra = wdTokenType
        }
{-# INLINEABLE mkWineCIP68Datum #-}

fromWineCIP68Datum :: WineTokenCIP68Datum -> WineToken
fromWineCIP68Datum (WineTokenCIP68Datum metadata _ wdTokenType) =
    WineToken
        (WineTokenMeta name desc img)
        (WineTokenData info mdata minscr)
        wdTokenType
  where
    lookupMetdata label = fromMaybe @BuiltinByteString "" $ lookup (encodeUtf8 label) metadata
    name = lookupMetdata "name"
    desc = lookupMetdata "description"
    img = lookupMetdata "image"
    info = lookupMetdata "batch_info"
    mdata = lookup (encodeUtf8 "batch_data") metadata
    minscr = lookup (encodeUtf8 "inscr_data") metadata
{-# INLINEABLE fromWineCIP68Datum #-}


isQuanityValid :: BatchQuantity -> Bool
isQuanityValid (BatchQuantity i r) = r #> 0 && i #>= r
{-# INLINEABLE isQuanityValid #-}

get1FromBatch :: WineToken -> WineToken
get1FromBatch (WineToken wi (WineTokenData inf dat ins) (BatchToken (BatchQuantity i r))) =
    if r #> 0
        then WineToken wi (WineTokenData inf dat ins) (BatchToken (BatchQuantity i (r #- 1)))
        else traceError "batch is empty"
get1FromBatch _ = traceError "invalid token type"
{-# INLINEABLE get1FromBatch #-}
