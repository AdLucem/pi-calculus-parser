
module PiCalculusTypes where

{-|
Elements of the pi-calculus:

* Name : a name is, well, a name. Can be thought of as a variable "naming" something to be substituted, or naming a channel.

* Value : A value is an unit of information. In this case we consider a value as a string.

* Channel : a 'mailbox' of a sort. Channel names give access to that particular channel. A channel is identified by its name.

-}

type Name = String
data Channel = Output Name | Input Name | Empty

instance Show Channel where
  show (Output name) = "!" ++ name
  show (Input name) = "?" ++ name
  show Empty = "o"

instance Eq Channel where
  (==) (Output x) (Output y) = x == y
  (==) (Input x) (Input y) = x == y
  (==) Empty Empty = True
  (==) a b = False

  (/=) (Output x) (Output y) = x /= y
  (/=) (Input x) (Input y) = x /= y
  (/=) Empty Empty = False
  (/=) a b = True

inverse :: Channel -> Channel -> Bool
inverse (Input x) (Output y) = x == y
inverse (Output x) (Input y) = x == y
inverse _ _ = False


{-|

  * Processes: a process has the following attributes:
               - Can have a termination point
               - Has channels, through which it can emit and receive data/other channels, and thereby change state to another process
-}

data Process =
  Lexeme Name
  | Prefix [Channel] [Channel] Process
  | Parallel [Process]


instance Show Process where
  show (Lexeme a) = a
  show (Prefix ls lsOpt a) =
       (foldl (\ x y -> x ++ (show y) ++ " . ") "" ls) 
    ++ (foldl (\ x y -> x ++ (show y) ++ " . ") "" lsOpt) 
    ++ (show a)
  show (Parallel ls) = 
    foldl (\x y -> x ++ (show y) ++ " | ") "" ls


-- this bit of code is an eyesore
instance Eq Process where
  (==) (Lexeme a) (Lexeme b) = a == b
  (==) (Prefix c cOpt p) (Prefix c_ cOpt_ p_) = 
    (c == c_) && (cOpt == cOpt_) && (p == p_)
  (==) (Parallel ls) (Parallel ls_) = ls == ls_ 
  (==) _ _ = False

{-
  (/=) (Saturated a) (Saturated b) = a /= b
  (/=) (Stable a) (Stable b) = a /= b
  (/=) (Prefix c1 p1) (Prefix c2 p2) = 
    (c1 /= c2) || (p1 /= p2)
  (/=) (Parallel ls) (Parallel ls_) = ls /= ls_
  (/=) _ _ = True
-}
