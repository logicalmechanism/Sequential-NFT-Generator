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
  , mintingScriptShortBs
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
  Copyright: 2022
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

data MintScriptContext = MintScriptContext
  { scriptContextTxInfo :: MintTxInfo
  , scriptContextPurpose :: PlutusV2.ScriptPurpose }
PlutusTx.unstableMakeIsData ''MintScriptContext

ownCurrencySymbol :: MintScriptContext -> PlutusV2.CurrencySymbol
ownCurrencySymbol MintScriptContext{scriptContextPurpose=PlutusV2.Minting cs} = cs
ownCurrencySymbol _                                                           = traceError "Lh" -- "Can't get currency symbol of the current validator script"
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> MintScriptContext -> Bool
mkPolicy _ context = do
      { let a = traceIfFalse "Minting" $ checkTokenMint
      ;         traceIfFalse "Error"   $ all (==True) [a]
      }
  where
    info :: MintTxInfo
    info = scriptContextTxInfo context

    txInputs :: [MintTxInInfo]
    txInputs = txInfoInputs info

    firstTx :: PlutusV2.TxOutRef
    firstTx = txInInfoOutRef $ head txInputs

    checkTokenMint :: Bool
    checkTokenMint =
      case Value.flattenValue (txInfoMint info) of
        [(cs, tkn, amt)] -> cs  == ownCurrencySymbol context &&
                            tkn == tkn'                      &&
                            amt == (1 :: Integer)
        _                -> False
      where
        tkn' :: PlutusV2.TokenName
        tkn' =  uniqueTokenName firstTx

    uniqueTokenName :: PlutusV2.TxOutRef -> PlutusV2.TokenName
    uniqueTokenName txRef = PlutusV2.TokenName { PlutusV2.unTokenName = hashTxHash txHash index }
      where
        hashTxHash :: PlutusV2.BuiltinByteString -> Integer -> PlutusV2.BuiltinByteString
        hashTxHash string counter =
          if counter == 0
            then sha3_256 string
            else hashTxHash (sha3_256 string) (counter - 1)

        txHash :: PlutusV2.BuiltinByteString
        txHash = PlutusV2.getTxId $ PlutusV2.txOutRefId txRef

        index :: Integer
        index = PlutusV2.txOutRefIdx txRef
-------------------------------------------------------------------------------
wrappedPolicy :: BuiltinData -> BuiltinData -> ()
wrappedPolicy x y = check (mkPolicy (PlutusV2.unsafeFromBuiltinData x) (PlutusV2.unsafeFromBuiltinData y))

policy :: MintingPolicy
policy = PlutusV2.mkMintingPolicyScript $
  $$(PlutusTx.compile [|| wrappedPolicy ||])

plutusScript :: Scripts.Script
plutusScript = PlutusV2.unMintingPolicyScript policy

validator :: PlutusV2.Validator
validator = PlutusV2.Validator plutusScript

optimizerSettings :: Plutonomy.OptimizerOptions
optimizerSettings = Plutonomy.defaultOptimizerOptions
  { Plutonomy.ooSplitDelay     = False
  , Plutonomy.ooFloatOutLambda = False
  }

scriptAsCbor :: LBS.ByteString
scriptAsCbor = serialise $ Plutonomy.optimizeUPLCWith optimizerSettings $ validator

mintingPlutusScript :: PlutusScript PlutusScriptV2
mintingPlutusScript = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LBS.toStrict $ scriptAsCbor