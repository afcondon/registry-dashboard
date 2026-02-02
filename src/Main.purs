-- | Main
-- |
-- | Entry point for the Registry Dashboard application.
module Main where

import Prelude

import Component.Dashboard as Dashboard
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Dashboard.component unit body
