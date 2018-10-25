
module ParserTypes where

import qualified PiCalculusTypes as P

{-|
Representing Feature Structures as Processes:

  * Feature Structure : a feature structure consists of a set of features (labels) mapped to:
             - A value (string)
             - A feature structure

We represent feature structures as an association list here.
-}

data Feature = Category | Phonetic
instance Show Feature where
  show Category = "cat : "
  show Phonetic = "phon : "

{-|
** ???? How to implement FeatureStructure as a type while preserving invariants

data FeatureStructure =
  Value String
  | ()
  | [FeatureStructure]
instance Show FeatureStructure where
  show (Value a) = a
  show ls = map showMapping ls where
    showMapping (a, b) = (show a) ++ (show b) ++ "\n"
-}
