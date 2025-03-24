{-# language BangPatterns #-}
{-# language QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : Clash.Lattice.ECP5.ButterStick.CRG
Description : Provides a clock and reset generator for the Colorlight ECP5 board.
-}
module Clash.Lattice.ECP5.ButterStick.CRG where

import Clash.Annotations.Primitive
import Clash.Explicit.Prelude

import Data.String.Interpolate ( i )
import Data.String.Interpolate.Util ( unindent )


createDomain vSystem
  { vName="Dom30"
  , vPeriod=33333
  , vActiveEdge=Rising
  , vResetKind=Asynchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

createDomain vSystem
  { vName="Dom50"
  , vPeriod=20000
  , vActiveEdge=Rising
  , vResetKind=Asynchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

createDomain vSystem
  { vName="DomEth"
  , vPeriod=8000
  , vActiveEdge=Rising
  , vResetKind=Asynchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

createDomain vSystem
  { vName="DomDDREth"
  , vPeriod=4000
  , vActiveEdge=Rising
  , vResetKind=Asynchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

createDomain vSystem
  { vName="DomEthTx"
  , vPeriod=8000
  , vActiveEdge=Rising
  , vResetKind=Asynchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

-- | Simple Clock reset generator for the colorlight ECP5 board
--   Generated using ecppll utility
{-# NOINLINE crg #-}
crg
  :: Clock Dom30
  -- ^ Input clock
  -> (Clock Dom50, Clock DomEthTx, Reset Dom50, Reset DomEthTx)
     -- ^ Output clock and reset
crg clkin = (clk50, clkEthTx, rst50, rstEthTx)
  where
    (clk50, locked50)     = pll50 clkin
    (clkEthTx, lockedEthTx) = pll125 clkin
    rst50    = resetSynchronizer clk50 (unsafeFromActiveLow locked50)
    rstEthTx = resetSynchronizer clkEthTx (unsafeFromActiveLow lockedEthTx)

-- | Generate a 50Mhz clock from 30Mhz
pll50
  :: Clock Dom30
  -- ^ Input 30 Mhz clock
  -> (Clock Dom50, Signal Dom50 Bool)
  -- ^ Output 50Mhz clock and unsynchronized reset signal
pll50 !_ = (clockGen, unsafeToActiveLow resetGen)
{-# ANN pll50 (InlinePrimitive [Verilog] $ unindent [i|
  [ { "BlackBox" :
      { "name"     : "Clash.Lattice.ECP5.ButterStick.CRG.pll50"
      , "kind"     : "Declaration"
      , "template" :
  "// pll50 primary begin
  wire ~GENSYM[clk50][0];
  wire ~GENSYM[pll50_locked][1];

  (* FREQUENCY_PIN_CLKI=\"30\" *)
  (* FREQUENCY_PIN_CLKOP=\"50\" *)
  (* ICP_CURRENT=\"12\" *) (* LPF_RESISTOR=\"8\" *) (* MFG_ENABLE_FILTEROPAMP=\"1\" *) (* MFG_GMCREF_SEL=\"2\" *)
  EHXPLLL #(
    .PLLRST_ENA(\"DISABLED\"),
    .INTFB_WAKE(\"DISABLED\"),
    .STDBY_ENABLE(\"DISABLED\"),
    .DPHASE_SOURCE(\"DISABLED\"),
    .OUTDIVIDER_MUXA(\"DIVA\"),
    .OUTDIVIDER_MUXB(\"DIVB\"),
    .OUTDIVIDER_MUXC(\"DIVC\"),
    .OUTDIVIDER_MUXD(\"DIVD\"),
    .CLKI_DIV(3),
    .CLKOP_ENABLE(\"ENABLED\"),
    .CLKOP_DIV(2),
    .CLKOP_CPHASE(1),
    .CLKOP_FPHASE(0),
    .FEEDBK_PATH(\"CLKOP\"),
    .CLKFB_DIV(10)
  ) ~GENSYM[pll50_inst][2] (
    .RST(1'b0),
    .STDBY(1'b0),
    .CLKI(~ARG[0]),
    .CLKOP(~SYM[0]),
    .CLKFB(~SYM[0]),
    .CLKINTFB(),
    .PHASESEL0(1'b0),
    .PHASESEL1(1'b0),
    .PHASEDIR(1'b1),
    .PHASESTEP(1'b1),
    .PHASELOADREG(1'b1),
    .PLLWAKESYNC(1'b0),
    .ENCLKOP(1'b0),
    .LOCK(~SYM[1])
  );

  assign ~RESULT = {~SYM[0], ~SYM[1]};
  // pll50 primary end"
      }
    }
  ]
  |]) #-}
{-# NOINLINE pll50 #-}

-- | Generate a 125Mhz clock from 30Mhz
pll125
  :: Clock Dom30
  -- ^ Input 30 Mhz clock
  -> (Clock DomEthTx, Signal DomEthTx Bool)
  -- ^ Output 125Mhz clock and unsynchronized reset signal
pll125 !_ = (clockGen, unsafeToActiveLow resetGen)
{-# ANN pll125 (InlinePrimitive [Verilog] $ unindent [i|
  [ { "BlackBox" :
      { "name"     : "Clash.Lattice.ECP5.ButterStick.CRG.pll125"
      , "kind"     : "Declaration"
      , "template" :
  "// pll125 primary begin
  wire ~GENSYM[clkEthTx][0];
  wire ~GENSYM[pll125_locked][1];

  (* FREQUENCY_PIN_CLKI=\"30\" *)
  (* FREQUENCY_PIN_CLKOP=\"125\" *)
  (* ICP_CURRENT=\"12\" *) (* LPF_RESISTOR=\"8\" *) (* MFG_ENABLE_FILTEROPAMP=\"1\" *) (* MFG_GMCREF_SEL=\"2\" *)
  EHXPLLL #(
          .PLLRST_ENA(\"DISABLED\"),
          .INTFB_WAKE(\"DISABLED\"),
          .STDBY_ENABLE(\"DISABLED\"),
          .DPHASE_SOURCE(\"DISABLED\"),
          .OUTDIVIDER_MUXA(\"DIVA\"),
          .OUTDIVIDER_MUXB(\"DIVB\"),
          .OUTDIVIDER_MUXC(\"DIVC\"),
          .OUTDIVIDER_MUXD(\"DIVD\"),
          .CLKI_DIV(3),
          .CLKOP_ENABLE(\"ENABLED\"),
          .CLKOP_DIV(4),
          .CLKOP_CPHASE(2),
          .CLKOP_FPHASE(0),
          .FEEDBK_PATH(\"CLKOP\"),
          .CLKFB_DIV(50)
      ) ~GENSYM[pll125_inst][2] (
          .RST(1'b0),
          .STDBY(1'b0),
          .CLKI(~ARG[0]),
          .CLKOP(~SYM[0]),
          .CLKFB(~SYM[0]),
          .CLKINTFB(),
          .PHASESEL0(1'b0),
          .PHASESEL1(1'b0),
          .PHASEDIR(1'b1),
          .PHASESTEP(1'b1),
          .PHASELOADREG(1'b1),
          .PLLWAKESYNC(1'b0),
          .ENCLKOP(1'b0),
          .LOCK(~SYM[1])
    );

  assign ~RESULT = {~SYM[0], ~SYM[1]};
  // pll125 primary end"
      }
    }
  ]
  |]) #-}
{-# NOINLINE pll125 #-}
