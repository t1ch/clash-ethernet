{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.Ethernet.UpConverter where

-- base
import Prelude

-- maybe
import qualified Data.Maybe as M

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude (type (<=))

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit(HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols
import Protocols
import Protocols.Hedgehog

-- util module
import Test.Cores.Ethernet.Util

-- ethernet modules
import Clash.Cores.Ethernet.UpConverter
import Clash.Cores.Ethernet.PacketStream

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

model :: forall n. C.KnownNat n => [PacketStreamM2S 1 ()] -> [PacketStreamM2S n ()]
model fragments = out
  where
    wholePackets = smearAbort <$> chunkBy (M.isJust . _last) fragments
    chunks = wholePackets >>= chopBy (C.natToNum @n)
    out    = fmap chunkToPacket chunks

fullPackets :: (C.KnownNat n) => [PacketStreamM2S n meta] -> [PacketStreamM2S n meta]
fullPackets [] = []
fullPackets fragments = let lastFragment = (last fragments) { _last = Just 0 }
                        in  init fragments ++ [lastFragment]

-- | Test the upconverter stream instance
upconverterTest :: forall n. 1 <= n => C.SNat n -> Property
upconverterTest C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 0 100) genPackets))    -- Input packets
    (C.exposeClockResetEnable model)              -- Desired behaviour of UpConverter
    (C.exposeClockResetEnable @C.System (ckt @n)) -- Implementation of UpConverter
    (===)                                         -- Property to test
  where
    ckt :: forall (dataWidth :: C.Nat) (dom :: C.Domain).
      C.HiddenClockResetEnable dom
      => 1 <= dataWidth
      => C.KnownNat dataWidth
      => Circuit (PacketStream dom 1 ()) (PacketStream dom dataWidth ())
    ckt = upConverterC

    -- This generates the packets
    genPackets =
      PacketStreamM2S <$>
      genVec Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      Gen.enumBounded <*>
      Gen.enumBounded

prop_upconverter_d1, prop_upconverter_d2, prop_upconverter_d4 :: Property
prop_upconverter_d1 = upconverterTest (C.SNat @1)
prop_upconverter_d2 = upconverterTest (C.SNat @2)
prop_upconverter_d4 = upconverterTest (C.SNat @4)

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (M.Just 1_000))
  $(testGroupGenerator)

