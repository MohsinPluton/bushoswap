{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Bishoswap where

import           Control.Monad                       (forM_, when)
import           Data.Aeson                          (FromJSON, ToJSON)
import qualified Data.Semigroup                      as Semigroup
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Ledger
import           Ledger.Constraints
import           Ledger.Value                        as Value
import           Plutus.Contract
import qualified Plutus.Contracts.Currency           as Currency
import qualified Plutus.Contracts.Uniswap            as Bishoswap
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)

data BishoswapContracts =
      Init
    | BishoswapStart
    | BishoswapUser Bishoswap.Uniswap
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty BishoswapContracts where
    pretty = viaShow

instance Builtin.HasDefinitions BishoswapContracts where
    getDefinitions = [Init, BishoswapStart]
    getSchema = \case
        BishoswapUser _ -> Builtin.endpointsToSchemas @Bishoswap.UniswapUserSchema
        BishoswapStart  -> Builtin.endpointsToSchemas @Bishoswap.UniswapOwnerSchema
        Init          -> Builtin.endpointsToSchemas @Empty
    getContract = \case
        BishoswapUser us -> Builtin.SomeBuiltin $ Bishoswap.userEndpoints us
        BishoswapStart   -> Builtin.SomeBuiltin Bishoswap.ownerEndpoint
        Init           -> Builtin.SomeBuiltin initContract

initContract :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
initContract = do
    ownPK <- pubKeyHash <$> ownPubKey
    cur   <- Currency.mintContract ownPK [(tn, fromIntegral (length wallets) * amount) | tn <- tokenNames]
    let cs = Currency.currencySymbol cur
        v  = mconcat [Value.singleton cs tn amount | tn <- tokenNames]
    forM_ wallets $ \w -> do
        let pkh = pubKeyHash $ walletPubKey w
        when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ txId tx
    tell $ Just $ Semigroup.Last cur
  where
    amount = 1000000

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 4]]

tokenNames :: [TokenName]
tokenNames = ["A", "B", "C", "D"]

cidFile :: Wallet -> FilePath
cidFile w = "W" ++ show (getWallet w) ++ ".cid"
