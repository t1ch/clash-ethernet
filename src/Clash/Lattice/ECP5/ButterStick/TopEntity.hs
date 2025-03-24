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


-- | The top entity
topEntity
  :: "clk30" ::: Clock Dom30
  -> "eth" ::: RGMIIRXChannel DomEth DomDDREth
  -> (
       "eth" ::: RGMIITXChannel DomDDREth
     )
topEntity clk30 eth0_rx  =
  let
    (clk50, ethTxClk, rst50, ethTxRst) = crg clk30
    en50 = enableGen

    ethRxClk = rgmii_rx_clk eth0_rx
    ethRxRst = resetGen @DomEth
    ethRxEn = enableGen @DomEth
    ethTxEn = enableGen @DomEthTx

    -- Replace this with your FPGA's MAC address
    ourMac = MacAddress (0x00 :> 0x00 :> 0x00 :> 0xff :> 0xff :> 0xff :> Nil)
    -- Hardcoded IPv4 and subnet mash
    ourIPv4 = ( IPv4Address (192 :> 168 :> 1 :> 123 :> Nil)
              , IPv4Address (255 :> 255 :> 255 :> 0 :> Nil)
              )

    phyStack
      = exposeClockResetEnable (unsafeRgmiiRxC @DomEth @DomDDREth (delayg d80) iddrx1f) ethRxClk ethRxRst ethRxEn
        |> exposeClockResetEnable (fullStackC ethRxClk ethRxRst ethRxEn ethTxClk ethTxRst ethTxEn (pure ourMac) (pure ourIPv4)) clk50 rst50 en50
        |> exposeClockResetEnable (rgmiiTxC @DomEthTx @DomDDREth (delayg d0) oddrx1f) ethTxClk ethTxRst ethTxEn


    eth0Tx = snd $ toSignals phyStack (eth0_rx, pure ())

    in
      (
        eth0Tx
      )

makeTopEntity 'topEntity
