{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Main
    ( main
    ) where

import           Control.Monad                       (forM_, void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (Result (..), encode, fromJSON)
import qualified Data.ByteString.Lazy                as LB
import           Data.Default                        (Default (..))
import qualified Data.Monoid                         as Monoid
import qualified Data.Semigroup                      as Semigroup
import           Data.Text                           (Text)
import qualified Plutus.Contracts.Currency           as Currency
import qualified Plutus.Contracts.Uniswap            as Bishoswap
import           Plutus.PAB.Effects.Contract.Builtin (Builtin)
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers, logString)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Prelude                             hiding (init)
import           Wallet.Emulator.Types               (Wallet (..))
import           Wallet.Types                        (ContractInstanceId (..))

import           Bishoswap                             as US

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    shutdown <- PAB.Server.startServerDebug

    cidInit  <- Simulator.activateContract (Wallet 1) Init
    cs       <- flip Simulator.waitForState cidInit $ \json -> case fromJSON json of
                    Success (Just (Semigroup.Last cur)) -> Just $ Currency.currencySymbol cur
                    _                                   -> Nothing
    _        <- Simulator.waitUntilFinished cidInit

    liftIO $ LB.writeFile "symbol.json" $ encode cs
    logString @(Builtin BishoswapContracts) $ "Initialization finished. Minted: " ++ show cs

    cidStart <- Simulator.activateContract (Wallet 1) BishoswapStart
    us       <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Bishoswap.Uniswap))) of
                    Success (Monoid.Last (Just (Right us))) -> Just us
                    _                                       -> Nothing
    logString @(Builtin BishoswapContracts) $ "Bishoswap instance created: " ++ show us

    forM_ wallets $ \w -> do
        cid <- Simulator.activateContract w $ BishoswapUser us
        liftIO $ writeFile (cidFile w) $ show $ unContractInstanceId cid
        logString @(Builtin BishoswapContracts) $ "Bishoswap user contract started for " ++ show w

    void $ liftIO getLine

    shutdown

handlers :: SimulatorEffectHandlers (Builtin BishoswapContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin BishoswapContracts) def def
    $ interpret
    $ Builtin.contractHandler
    $ Builtin.handleBuiltin @BishoswapContracts
