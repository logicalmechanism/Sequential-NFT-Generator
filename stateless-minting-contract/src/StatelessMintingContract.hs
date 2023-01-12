{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NumericUnderscores    #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module StatelessMintingContract
  ( mintingPlutusScript
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                ( serialise )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import qualified Plutonomy
{-
  Author   : The Ancient Kraken
  Copyright: 2023
-}
data MintTxInInfo = MintTxInInfo
    { txInInfoOutRef   :: PlutusV2.TxOutRef
    , txInInfoResolved :: BuiltinData
    } 
PlutusTx.unstableMakeIsData ''MintTxInInfo

data MintTxInfo = MintTxInfo
    { txInfoInputs          :: [MintTxInInfo] -- Transaction inputs
    , txInfoReferenceInputs :: BuiltinData
    , txInfoOutputs         :: BuiltinData
    , txInfoFee             :: BuiltinData
    , txInfoMint            :: PlutusV2.Value -- The 'Value' minted by this transaction.
    , txInfoDCert           :: BuiltinData
    , txInfoWdrl            :: BuiltinData
    , txInfoValidRange      :: BuiltinData
    , txInfoSignatories     :: BuiltinData
    , txInfoRedeemers       :: BuiltinData
    , txInfoData            :: BuiltinData
    , txInfoId              :: BuiltinData
    }
PlutusTx.unstableMakeIsData ''MintTxInfo

-- | Purpose of the script that is currently running
data MintScriptPurpose = Minting PlutusV2.CurrencySymbol | BuiltinData
PlutusTx.unstableMakeIsData ''MintScriptPurpose

data MintScriptContext = MintScriptContext
  { scriptContextTxInfo :: MintTxInfo
  , scriptContextPurpose :: MintScriptPurpose 
  }
PlutusTx.unstableMakeIsData ''MintScriptContext

ownCurrencySymbol :: MintScriptContext -> PlutusV2.CurrencySymbol
ownCurrencySymbol MintScriptContext{scriptContextPurpose=Minting cs} = cs
ownCurrencySymbol _                                                  = traceError "Lh"
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> MintScriptContext -> Bool
mkPolicy _ context = checkTokenMint && PlutusV2.txOutRefIdx firstTx < 256
  where
    info :: MintTxInfo
    info = scriptContextTxInfo context

    firstTx :: PlutusV2.TxOutRef
    firstTx = txInInfoOutRef $ head $ txInfoInputs info

    -- traceIfFalse (decodeUtf8 $ PlutusV2.unTokenName tkn')
    checkTokenMint :: Bool
    checkTokenMint =
      case Value.flattenValue (txInfoMint info) of
        [(cs, tkn, amt)] -> cs  == ownCurrencySymbol context &&
                            tkn == uniqueTokenName firstTx   &&
                            amt == (1 :: Integer)
        _                -> False

    uniqueTokenName :: PlutusV2.TxOutRef -> PlutusV2.TokenName
    uniqueTokenName txRef = PlutusV2.TokenName { PlutusV2.unTokenName = prependTxHash txHash index }
      where
        prependTxHash :: PlutusV2.BuiltinByteString -> Integer -> PlutusV2.BuiltinByteString
        prependTxHash string counter = sliceByteString 0 32 (consByteString counter (sha3_256 string))

        txHash :: PlutusV2.BuiltinByteString
        txHash = PlutusV2.getTxId $ PlutusV2.txOutRefId txRef

        index :: Integer
        index = PlutusV2.txOutRefIdx txRef
-------------------------------------------------------------------------------
wrappedPolicy :: BuiltinData -> BuiltinData -> ()
wrappedPolicy x y = check (mkPolicy (PlutusV2.unsafeFromBuiltinData x) (PlutusV2.unsafeFromBuiltinData y))

policy :: MintingPolicy
policy = PlutusV2.mkMintingPolicyScript $ $$(PlutusTx.compile [|| wrappedPolicy ||])

plutusScript :: Scripts.Script
plutusScript = PlutusV2.unMintingPolicyScript policy

validator :: PlutusV2.Validator
validator = PlutusV2.Validator plutusScript

scriptAsCbor :: LBS.ByteString
scriptAsCbor = serialise $ Plutonomy.optimizeUPLCWith Plutonomy.aggressiveOptimizerOptions $ validator

mintingPlutusScript :: PlutusScript PlutusScriptV2
mintingPlutusScript = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor