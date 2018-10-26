
module PiCalculusSemantics where

import qualified PiCalculusTypes as CT

data Relation =
  Interact CT.Process CT.Process CT.Name |
  Is CT.Process CT.Process
instance Show Relation where
  show (Interact pA pB c) =
    (show pA) ++
    " -" ++ (show c) ++
    "-> " ++ (show pB)

data Rule = Rule [Relation] Relation

showPremises :: [Relation] -> String
showPremises (x:[]) = show x
showPremises (x:xs) =
  (show x) ++ ",\n" ++ (showPremises xs)

instance Show Rule where
  show (Rule p s) =
    (showPremises p) ++
    "\n--------------------\n" ++
    (show s)
