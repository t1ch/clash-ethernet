{-# language FlexibleContexts #-}

module Clash.Cores.Ethernet.PacketBuffer
    ( packetBuffer
    , packetBufferC
    ) where

import Clash.Cores.Ethernet.PacketStream
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
    -> ( Signal dom (Maybe (PacketStreamM2S dataWidth metaType)),
            Signal dom PacketStreamS2M
        )
    -> Signal dom (Maybe (PacketStreamM2S dataWidth metaType))

packetBuffer SNat (inM2S, inS2M) = outM2S
    where
        --The backing ram
        outM2S = mux emptyBuffer
            (pure Nothing)
            (blockRam1 NoClearOnReset (SNat @(2 ^ sizeBits)) (errorX "initial block ram contents") readAddr writeCommand)

         -- write command
        writeCommand = mux writeEnable
            (Just <$> bundle (wordAddr, inM2S))
            (pure Nothing)

        -- Registers
        wordAddr, packetAddr, readAddr :: Signal dom (Unsigned sizeBits)
        wordAddr = register 0 $ mux writeEnable (wordAddr + 1) wordAddr
        packetAddr = register 0 $ mux (lastWord .&&. writeEnable) (wordAddr + 1) packetAddr
        readAddr = register 0 $ mux readEnable (readAddr + 1) readAddr
        dropping = register False ((fullBuffer .&&. writeRequest) .||. (dropping .&&. (not <$> lastWord)))

        -- Only write if there is space
        writeEnable = writeRequest .&&. (not <$> fullBuffer) .&&. (not <$> dropping)
        -- renove notEmptyBuffer and add this to outM2S
        readEnable = notEmpty .&&. (_ready <$> inS2M)
        notEmpty = not <$> emptyBuffer

        --The status signals
        emptyBuffer  = packetAddr .==. readAddr
        fullBuffer = (wordAddr + 1)  .==. readAddr
        writeRequest = isJust <$> inM2S
        lastWord = isLast <$> inM2S
        isLast :: Maybe (PacketStreamM2S dataWidth metaType) -> Bool
        isLast word = case word of
            Just (PacketStreamM2S { _last = Just _ }) -> True
            _ -> False

-- Fix the type signature of packetBufferC to match the expected type of fromSignals
packetBufferC
    :: forall (dataWidth :: Nat) (sizeBits :: Nat) (dom :: Domain) (metaType :: Type).
    HiddenClockResetEnable dom
        => KnownNat dataWidth
        => KnownNat sizeBits
        => NFDataX metaType
        => 1 <= sizeBits
        => SNat sizeBits
        -> Circuit (PacketStream dom dataWidth metaType) (PacketStream dom dataWidth metaType)

packetBufferC sizeBits = forceResetSanity |> fromPacketStream |> fromSignals wrap
    where
        wrap:: ( CSignal dom (Maybe (PacketStreamM2S dataWidth metaType)),
                    Signal dom PacketStreamS2M
                )
            -> (CSignal dom (), Signal dom (Maybe (PacketStreamM2S dataWidth metaType)))
        wrap (CSignal inFWD, inBWD)  = (CSignal (pure ()), packetBuffer sizeBits (inFWD, inBWD))
