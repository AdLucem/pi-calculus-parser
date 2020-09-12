
module Stochastic where

import PiCalculusTypes
import Reduction

-- once again, naive initialization
vSize = 10

probMatrix :: [[Float]]
-- naive initialization- CHANGE THIS
probMatrix = replicate vSize $ replicate vSize 0.0