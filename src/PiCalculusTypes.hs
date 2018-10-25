
module PiCalculusTypes where

{-|
Elements of the pi-calculus:

* Name : a name is, well, a name. Can be thought of as a variable "naming" something to be substituted, or naming a channel.

* Value : A value is an unit of information. In this case we consider a value as a string.

* Channel : a 'mailbox' of a sort. Channel names give access to that particular channel. A channel is identified by its name.

-}

type Value = String
type Channel = String

data Name = Emit String | Receive String | Empty
instance Show Name where
  show Empty = "<>"
  show (Emit a) = (show a) ++ "^"
  show (Receive a) = (show a) ++ "?"

{-|

  * Processes: a process has the following attributes:
               - Can have a termination point
               - Has channels, through which it can emit and receive data/other channels, and thereby change state to another process
-}

data Process =
  Terminate
  | Action Name
  | Prefix Name Process
  | New Name Process
  | Match Name Value Process
  | Replicate Process
  | Parallel Process Process
  | Choice Process Process

instance Show Process where
  show Terminate = "0"
  show (Action a) = show a
  show (Prefix a b) = (show a) ++ "." ++ (show b)
  show (New a b) = "(new " ++ (show a) ++ ")" ++ (show b)
  show (Match a b c) = "[" ++ (show a)
    ++"=" ++ b ++ "]" ++ (show c)
  show (Replicate a) = "!" ++ (show a)
  show (Parallel a b) = (show a) ++ " | " ++ (show b)
  show (Choice a b) = (show a) ++ " + " ++ (show b)
