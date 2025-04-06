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

-- | ARP payload size (28 bytes)
type ARPSize = 28

-- | Ethernet header size (14 bytes)
type EthernetHeaderSize = 14

-- | Ethernet payload minimum size (46 bytes - includes ARP data and any padding)
type EthernetMinPayloadSize = 46

-- | FCS/CRC size (4 bytes)
type CRCSize = 4

-- | Total packet size (14 + 46 + 4 = 64 bytes - Ethernet minimum frame size with CRC)
type PacketSize = EthernetHeaderSize + EthernetMinPayloadSize + CRCSize

-- | ARP packet state machine
data ARPState
  = Idle           -- Waiting for button press
  | Sending (Index PacketSize)  -- Sending packet, with current byte index
  deriving (Show, Generic)

instance NFDataX ARPState where
  deepErrorX = errorX "deepErrorX @ARPState"
  rnfX Idle = ()
  rnfX (Sending idx) = rnfX idx

-- | CRC32 polynomial (standard Ethernet polynomial: 0x04C11DB7)
crcPoly :: BitVector 33
crcPoly = 0x104C11DB7

-- | Calculate CRC32 for a sequence of bytes
calculateCRC32 :: Vec (EthernetHeaderSize + EthernetMinPayloadSize) (BitVector 8) -> BitVector 32
calculateCRC32 bytes = complement $ foldl updateCRC initCRC bytes
  where
    initCRC = complement 0xFFFFFFFF

    updateCRC :: BitVector 32 -> BitVector 8 -> BitVector 32
    updateCRC crc byte =
      let byte' = shiftL (zeroExtend byte) 24
          initialXor = crc `xor` byte'
      in foldl shiftAndXor initialXor (replicate d8 ())

    shiftAndXor :: BitVector 32 -> () -> BitVector 32
    shiftAndXor crc _ =
      let msb = testBit crc 31
          shifted = shiftL crc 1
      in if msb then shifted `xor` (0x04C11DB7) else shifted

-- | Create ARP packet as a Vec of bytes (60 bytes for header+payload, will add 4 byte CRC later)
makeARPPacketWithoutCRC :: Vec (EthernetHeaderSize + EthernetMinPayloadSize) (BitVector 8)
makeARPPacketWithoutCRC =
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
  -- Padding (to minimum Ethernet payload size of 46 bytes)
  replicate d18 0x00

-- | Complete Ethernet frame including CRC
makeARPPacket :: Vec PacketSize (BitVector 8)
makeARPPacket =
  let payload = makeARPPacketWithoutCRC
      crc = calculateCRC32 payload
      -- CRC bytes in little-endian order (LSB first)
      crcBytes = (slice d7 d0 crc) :>
                 (slice d15 d8 crc) :>
                 (slice d23 d16 crc) :>
                 (slice d31 d24 crc) :> Nil
  in payload ++ crcBytes

-- | Main circuit that uses the simplest possible approach
topEntity
  :: "clk30" ::: Clock Dom30
  -> "user_btn"    ::: Signal DomEthTx (Vec 2 Bit)
  -> "eth"         ::: RGMIIRXChannel DomEth DomDDREth
  -> (
       "eth"       ::: RGMIITXChannel DomDDREth,
       "eth_rst_n" ::: Signal DomEthTx Bit
     )

topEntity clk30 user_btn rgmiiRxChannel  =
  ( rgmiiSender ethTxClk rst (delayg d55) oddrx1f byteToSend err
  , eth_rst_n
  )
  where
    (clk60, ethTxClk, rst60, rstEthTx) = crg clk30
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
