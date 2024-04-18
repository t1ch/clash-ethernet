{-# language FlexibleContexts #-}

module Clash.Cores.Ethernet.PacketBuffer
    ( packetBufferC
    , cSignalPacketBufferC
    ) where

import Clash.Cores.Ethernet.PacketStream
import Clash.Cores.Ethernet.Util
import Clash.Prelude
import Data.Maybe

import Protocols ( Circuit(..), fromSignals, (|>) )
import Protocols.Internal ( CSignal(..) )

packetBuffer
  :: forall (dataWidth :: Nat) (sizeBits :: Nat) (dom :: Domain) (metaType :: Type).
  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => KnownNat sizeBits
  => NFDataX metaType
  => 1 <= sizeBits
  => SNat sizeBits
  -- ^ Depth of the packet buffer 2^sizeBits
  -> ( Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
     , Signal dom PacketStreamS2M
     )
  -- ^ Input packetStream
  -> (  Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
     )
  -- ^ Output CSignal s
packetBuffer SNat (fwdIn, bwdIn) = (PacketStreamS2M . not <$> fullBuffer, toMaybe <$> (not <$> emptyBuffer) <*> ramOut)
  where
    --The backing ram
    ramOut = blockRam1 NoClearOnReset (SNat @(2 ^ sizeBits)) (errorX "initial block ram contents") readAddr' writeCommand

      -- write command
    writeCommand = toMaybe <$> writeEnable <*> bundle(wordAddr, fromJustX <$> fwdIn)

    -- Registers : pointers
    wordAddr, packetAddr, readAddr :: Signal dom (Unsigned sizeBits)
    wordAddr = register 0 $ mux dropping' packetAddr $ mux writeEnable (wordAddr + 1) wordAddr
    packetAddr = register 0 $ mux (lastWordIn .&&. writeEnable) (wordAddr + 1) packetAddr
    readAddr' = mux readEnable (readAddr + 1) readAddr
    readAddr = register 0 readAddr'

    -- Registers : status
    dropping', dropping, emptyBuffer :: Signal dom Bool
    -- start dropping packet on abort
    dropping' = abortIn .||. dropping
    dropping = register False $ dropping' .&&. (not <$> lastWordIn)
    emptyBuffer  = register 0 packetAddr .==. readAddr

    -- Only write if there is space and we're not dropping
    writeEnable = writeRequest .&&. (not <$> fullBuffer) .&&. (not <$> dropping')
    -- Read when the word has been received
    readEnable = (not <$> emptyBuffer) .&&. (_ready <$> bwdIn)

    --The status signals
    fullBuffer = (wordAddr + 1) .==. readAddr
    writeRequest = isJust <$> fwdIn
    lastWordIn = maybe False (isJust . _last) <$> fwdIn
    abortIn = maybe False _abort <$> fwdIn

abortOnBackPressure
  :: forall (dataWidth :: Nat) (dom :: Domain) (metaType :: Type).
  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => NFDataX metaType
  -- ^ Depth of the packet buffer 2^sizeBits
  => ( Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
     , Signal dom PacketStreamS2M
     )
  -- ^ Input packetStream
  -> Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
  -- ^ Does not give backpressure, sends an abort forward instead
abortOnBackPressure (fwdIn, bwdIn) = package <$> aborting <*> fwdIn
  where
    aborting = not . _ready <$> bwdIn

    package :: Bool -> Maybe (PacketStreamM2S dataWidth metaType) -> Maybe (PacketStreamM2S dataWidth metaType)
    package _     Nothing         = Nothing
    package True  (Just message)  = Just $ message { _abort = True }
    package False message         = message

packetBufferC
  :: forall (dataWidth :: Nat) (sizeBits :: Nat) (dom :: Domain) (metaType :: Type).
  HiddenClockResetEnable dom
    => KnownNat dataWidth
    => KnownNat sizeBits
    => NFDataX metaType
    => 1 <= sizeBits
    => SNat sizeBits
    -- ^ Depth of the packet buffer, this is equal to 2^sizeBits
    -> Circuit (PacketStream dom dataWidth metaType) (PacketStream dom dataWidth metaType)
packetBufferC sizeBits = forceResetSanity |> fromSignals (packetBuffer sizeBits)

cSignalPacketBufferC :: forall (dataWidth :: Nat) (sizeBits :: Nat) (dom :: Domain) (metaType :: Type).
  HiddenClockResetEnable dom
    => KnownNat dataWidth
    => KnownNat sizeBits
    => NFDataX metaType
    => 1 <= sizeBits
    => SNat sizeBits
    -- ^ Depth of the packet buffer 2^sizeBits
    -> Circuit (CSignal dom (Maybe (PacketStreamM2S dataWidth metaType))) (PacketStream dom dataWidth metaType)
cSignalPacketBufferC size = abortC |> fromSignals (packetBuffer size)
  where
    abortC :: Circuit (CSignal dom (Maybe (PacketStreamM2S dataWidth metaType ))) (PacketStream dom dataWidth metaType)
    abortC = fromSignals wrapAbortOnBackPressure

    wrapAbortOnBackPressure:: ( CSignal dom (Maybe (PacketStreamM2S dataWidth metaType)),
                Signal dom PacketStreamS2M
            )
        -> (CSignal dom (), Signal dom (Maybe (PacketStreamM2S dataWidth metaType)))
    wrapAbortOnBackPressure (CSignal fwdIn, bwdIn)  = (CSignal (pure ()), abortOnBackPressure (fwdIn, bwdIn))
