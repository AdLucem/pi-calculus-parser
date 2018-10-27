
module ParserTypes where

import qualified PiCalculusTypes as PC

{-|
Representing Feature Structures as Processes:

  * Feature Structure : a feature structure consists of a set of features (labels) mapped to either:
             - A value (string)
             - A feature structure

We represent feature structures as a list of tuples here.
-}

data Feature = Category | Phonetic
instance Show Feature where
  show Category = "cat : "
  show Phonetic = "phon : "

data FeatureStructure =
  Value String
  | Cons (Feature, FeatureStructure) FeatureStructure
instance Show FeatureStructure where
  show (Value a) = a
  show  (Cons x xs) =
    (show $ fst x) ++
    (show $ snd x) ++
    "\n" ++ (show xs) 

{-|
Representing a feature structure as a process:

sendChannel :: PC.Name -> String -> PC.Process
sendChannel sender channel =
  Prefix sender (Action (Emit channel))

receiveChannel :: PC.Name -> String -> PC.Process
receiveChannel receiver channel =
  Prefix receiver (Action (Emit channel))

--toPiCalc :: FeatureStructure -> Int -> PC.Process
--toPiCalc (x:xs) depth = New () 
-}
