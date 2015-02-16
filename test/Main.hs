
module Main (main) where

import Test.Framework (defaultMain)

import qualified BoardTest as B
import qualified GameTest as G


main = defaultMain (concat [B.tests, G.tests])