import Prelude

import Test.Tasty

import qualified Test.Cores.Ethernet.PacketStream
import qualified Test.Lattice.ECP5.UART
import qualified Test.Cores.Ethernet.UpConverter
import qualified Test.Cores.Ethernet.DownConverter
import qualified Test.Cores.Ethernet.PacketBuffer
import qualified Test.Cores.Ethernet.MaybeControlProperty
import qualified Test.Cores.Ethernet.InterpacketGapInserter
import qualified Test.Cores.Ethernet.AsyncFIFO

main :: IO ()
main = defaultMain $ testGroup "."
  [ Test.Cores.Ethernet.AsyncFIFO.tests
    , Test.Cores.Ethernet.PacketStream.tests
    , Test.Lattice.ECP5.UART.tests
    , Test.Cores.Ethernet.UpConverter.tests
    , Test.Cores.Ethernet.DownConverter.tests
    , Test.Cores.Ethernet.MaybeControlProperty.tests
    , Test.Cores.Ethernet.PacketBuffer.tests
    , Test.Cores.Ethernet.InterpacketGapInserter.tests
  ]
