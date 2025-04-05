{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE NumericUnderscores #-}

{-# LANGUAGE DataKinds #-}

{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveGeneric #-}



{-|

Module      : Clash.Lattice.ECP5.ButterStick.TopEntity

Description : Top entity for ButterStick board that sends an ARP broadcast when user_btn[0] is pressed

-}

module Clash.Lattice.ECP5.ButterStick.TopEntity where



import Clash.Explicit.Prelude

import Clash.Prelude ( exposeClockResetEnable )

import Clash.Lattice.ECP5.Prims

import Clash.Lattice.ECP5.RGMII

import Clash.Lattice.ECP5.ButterStick.CRG

import Clash.Annotations.TH



import Clash.Signal.BiSignal

import Protocols

import Protocols.Extra.PacketStream



import GHC.Stack (HasCallStack)

import GHC.TypeLits hiding (SNat)

import GHC.Generics (Generic)



-- | Total packet size (64 bytes - Ethernet minimum frame size)

type PacketSize = 64



-- | ARP packet state machine

data ARPState

  = Idle           -- Waiting for button press

  | Sending (Index PacketSize)  -- Sending packet, with current byte index

  deriving (Show, Generic)



instance NFDataX ARPState where

  deepErrorX = errorX "deepErrorX @ARPState"

  rnfX Idle = ()

  rnfX (Sending idx) = rnfX idx



-- | Create ARP packet as a Vec of bytes (total 64 bytes including Ethernet padding)

makeARPPacket :: Vec PacketSize (BitVector 8)

makeARPPacket =

  -- Ethernet Header (14 bytes)

  0xFF :> 0xFF :> 0xFF :> 0xFF :> 0xFF :> 0xFF :>  -- Destination MAC (broadcast)

  0x00 :> 0x11 :> 0x22 :> 0x33 :> 0x44 :> 0x55 :>  -- Source MAC

  0x08 :> 0x06 :>                                  -- EtherType (ARP)



  -- ARP Packet (28 bytes)

  0x00 :> 0x01 :>                                  -- Hardware Type (Ethernet)

  0x08 :> 0x00 :>                                  -- Protocol Type (IPv4)

  0x06 :>                                         -- Hardware Size (MAC = 6 bytes)

  0x04 :>                                         -- Protocol Size (IPv4 = 4 bytes)

  0x00 :> 0x01 :>                                  -- Operation (Request = 1)



  -- Sender MAC

  0x00 :> 0x11 :> 0x22 :> 0x33 :> 0x44 :> 0x55 :>



  -- Sender IP (192.168.1.123)

  0xC0 :> 0xA8 :> 0x01 :> 0x7B :>



  -- Target MAC (zeros for request)

  0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :>



  -- Target IP (192.168.1.124)

  0xC0 :> 0xA8 :> 0x01 :> 0x7C :>



  -- Padding (to minimum Ethernet frame size of 64 bytes)

  replicate d22 0x00



-- | Main circuit that uses the simplest possible approach

topEntity

  :: "eth_tx_clk" ::: Clock DomEthTx

  -> "user_btn"    ::: Signal DomEthTx (Vec 2 Bit)

  -> "eth"         ::: RGMIIRXChannel DomEth DomDDREth

  -> (

       "eth"       ::: RGMIITXChannel DomDDREth,

       "eth_rst_n" ::: Signal DomEthTx Bit

     )

topEntity ethTxClk user_btn rgmiiRxChannel  =

  ( rgmiiSender ethTxClk rst (delayg d55) oddrx1f byteToSend err

  , eth_rst_n

  )

  where

    -- Extract button state and reset signal

    btn0      = (! 0) <$> user_btn

    eth_rst_n = (! 1) <$> user_btn



    -- Convert Bit to Bool for button signal

    btn0Bool = fmap bitToBool btn0



    -- Create a simple reset signal (active low - not asserted)

    rstSignal = pure False

    rst = unsafeFromActiveHigh rstSignal



    -- Simple button edge detector for button 0

    btnReg = register ethTxClk rst enableGen False btn0Bool

    btnPressed = (&&) <$> btn0Bool <*> (not <$> btnReg)



    -- Definition of our simple state machine using recursive bindings

    state  = register ethTxClk rst enableGen Idle nextState



    -- Next state logic

    nextState = (\currentState btnIsPressed ->

                   case currentState of

                     Idle ->

                       if btnIsPressed then Sending 0 else Idle

                     Sending idx ->

                       if idx >= maxBound then Idle

                       else Sending (idx + 1)

                ) <$> state <*> btnPressed



    -- Generate the output based on current state

    output = (\st ->

                case st of

                  Idle         -> (Nothing, False)

                  Sending idx  -> (Just (makeARPPacket !! idx), False)

             ) <$> state



    -- Unbundle the output

    (byteToSend, err) = unbundle output



makeTopEntity 'topEntity
