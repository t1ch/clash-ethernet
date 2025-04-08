{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : Clash.Lattice.ECP5.ButterStick.TopEntity
Description : Contains the top entity.
-}
module Clash.Lattice.ECP5.ButterStick.TopEntity
  ( topEntity
  ) where

import Clash.Annotations.TH

import Clash.Explicit.Prelude
import Clash.Prelude ( exposeClockResetEnable )

import Clash.Cores.Crc ( deriveHardwareCrc )
import Clash.Cores.Crc.Catalog ( Crc32_ethernet )
import Clash.Cores.Ethernet.IP.IPv4Types ( IPv4Address(IPv4Address) )
import Clash.Cores.Ethernet.Mac.EthernetTypes ( MacAddress(MacAddress) )
import Clash.Lattice.ECP5.ButterStick.CRG
import Clash.Lattice.ECP5.Prims
import Clash.Lattice.ECP5.RGMII ( RGMIIRXChannel(..), RGMIITXChannel(..), rgmiiTxC, unsafeRgmiiRxC )

import Protocols ( toSignals, (|>) )

import Clash.Cores.Ethernet.Examples.FullUdpStack ( fullStackC )
import Data.Proxy ( Proxy(Proxy) )


$(deriveHardwareCrc (Proxy @Crc32_ethernet) d8 d4)

topEntity
  :: "clk30" ::: Clock Dom30
  -> "user_btn"    ::: Signal DomEthTx (Vec 2 Bit)
  -> "eth"         ::: RGMIIRXChannel DomEth DomDDREth
  -> (
       "eth"       ::: RGMIITXChannel DomDDREth,
       "eth_rst_n" ::: Signal DomEthTx Bit
     )

topEntity clk30 user_btn eth_rx  =
  ( ethTx
  , eth_rst_n
  )
  where
    (clk60, ethTxClk, rst60, ethTxRst) = crg clk30
    en60 = enableGen
    ethRxClk = rgmii_rx_clk eth_rx
    ethRxRst = resetGen @DomEth
    ethRxEn = enableGen @DomEth
    ethTxEn = enableGen @DomEthTx

    -- Extract button state and reset signal
    eth_rst_n = (! 1) <$> user_btn
   --- 82:91:B0:8F:EC:89
    ourMac = MacAddress (0x82 :> 0x91 :> 0xb0 :> 0x8f :> 0xec :> 0x89 :> Nil)
    -- Hardcoded IPv4 and subnet mash
    ourIPv4 = ( IPv4Address (192 :> 168 :> 1 :> 123 :> Nil)
              , IPv4Address (255 :> 255 :> 255 :> 0 :> Nil)
              )

    phyStack
      = exposeClockResetEnable (unsafeRgmiiRxC @DomEth @DomDDREth (delayg d80) iddrx1f) ethRxClk ethRxRst ethRxEn
        |> exposeClockResetEnable (fullStackC ethRxClk ethRxRst ethRxEn ethTxClk ethTxRst ethTxEn (pure ourMac) (pure ourIPv4)) clk60 rst60 en60
        |> exposeClockResetEnable (rgmiiTxC @DomEthTx @DomDDREth (delayg d80) oddrx1f) ethTxClk ethTxRst ethTxEn

    ethTx = snd $ toSignals phyStack (eth_rx, pure ())


makeTopEntity 'topEntity
