module Onchain.WineValidator where

import Onchain.OnchainUtils
import Onchain.Types
import PlutusCore.Builtin.Debug (plcVersion110)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (
    assetClass,
    assetClassValue,
    geq,
 )
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts
import PlutusTx (
    CompiledCode,
    compile,
    liftCode,
    unsafeApplyCode,
 )

-- | The Minting Policy Logic for Wine Batch Tokens.
wineTypedValidator ::
    WineMintingParams ->
    ScriptContext ->
    Bool
wineTypedValidator WineMintingParams{..} (ScriptContext ti@TxInfo{..} (Redeemer redeemerBData) scriptInfo) =
    let signedByAdmin = traceIfFalse "not signed by admin" (adminPkh `pelem` txInfoSignatories)
     in case (scriptInfo, fromBuiltinData redeemerBData) of
            (MintingScript cs@(CurrencySymbol bshash), Just mintingRedeemer) ->
                case mintingRedeemer of
                    (MintWineToken (TxOutRef tid index) new_wt) ->
                        let !nftAC = assetClass cs (tokenNameFromTxOutRef tid index) -- ensure uniqeness
                            (!tokenRefAC, !tokenUserAC) = generateRefAndUserAC nftAC
                         in pand
                                [ signedByAdmin
                                , traceIfFalse "Must spend seed tx" $
                                    spendsOutput ti tid index -- ensure uniqeness
                                , traceIfFalse "Must mint the ref NFT" $
                                    isMintingNFT tokenRefAC txInfoMint
                                , traceIfFalse "Must mint the user NFT" $
                                    isMintingNFT tokenUserAC txInfoMint
                                , traceIfFalse "Must lock ref NFT" $
                                    hasTxOutWithInlineDatumAnd
                                        (mkWineCIP68Datum new_wt) -- has corect inline datum
                                        (`geq` assetClassValue tokenRefAC 1) -- has ref token
                                        (#== scriptHashAddress (ScriptHash bshash)) -- is locked to validator
                                        txInfoOutputs
                                , traceIfFalse "If minting bottle must spend batch token" $
                                    case wdTokenType new_wt of
                                        BatchToken q -> isQuanityValid q
                                        BottleToken batchid ->
                                            hasTxInWithToken
                                                batchid
                                                txInfoInputs -- if bottle : checks if spends batch token
                                ]
                    (Burn ac) ->
                        pand
                            [ signedByAdmin
                            , isBurningNFT ac txInfoMint
                            ]
                    _ -> traceError "wrong redeemer"
            (SpendingScript outRef (Just (Datum bdatum)), Just spendingRedeemer) ->
                case (fromBuiltinData bdatum, findTxInByTxOutRef outRef ti, spendingRedeemer) of
                    (Just (_wt :: WineTokenCIP68Datum), _, Burn ac) ->
                        pand
                            [ signedByAdmin
                            , isBurningNFT ac txInfoMint
                            ]
                    (Just currentDatum, Just (TxInInfo _ ownInput), Update new_wt) ->
                        let
                            ownValue = txOutValue ownInput
                         in
                            pand
                                [ traceIfFalse "Token type must not be changed" $
                                    extra currentDatum #== wdTokenType new_wt
                                , traceIfFalse "Must update and lock ref NFT" $
                                    hasTxOutWithInlineDatumAnd
                                        (mkWineCIP68Datum new_wt) -- has updated inline datum
                                        (`geq` ownValue) -- keeps value
                                        (#== txOutAddress ownInput) -- same address
                                        txInfoOutputs
                                ]
                    (Just currentDatum, Just (TxInInfo _ ownInput), MintBottle) ->
                        let
                            ownValue = txOutValue ownInput
                         in
                            pand
                                [ traceIfFalse "Must update and lock batch ref NFT" $
                                    hasTxOutWithInlineDatumAnd
                                        (mkWineCIP68Datum . get1FromBatch . fromWineCIP68Datum $ currentDatum) -- subtract 1 unit
                                        (`geq` ownValue) -- keeps value
                                        (#== txOutAddress ownInput) -- same address
                                        txInfoOutputs
                                ]
                    (Nothing, _, _) -> traceIfTrue "invalid datum" signedByAdmin -- only admin can spend utxos with invalid datums
                    _ -> traceError "wrong redeemer"
            _ -> traceError "wrong purpose"
{-# INLINEABLE wineTypedValidator #-}

-- | Lose types
wineUntypedValidator :: WineMintingParams -> BuiltinData -> BuiltinUnit
wineUntypedValidator pkh =
    mkUntypedLambda (wineTypedValidator pkh)

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
wineScriptCompiled :: WineMintingParams -> CompiledCode (BuiltinData -> BuiltinUnit)
wineScriptCompiled mp =
    $$(compile [||wineUntypedValidator||])
        `unsafeApplyCode` liftCode plcVersion110 mp
