
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
  show (Output name) = "!" ++ (show name)
  show (Input name) = "?" ++ (show name)
  show Empty = "o"
  
{-|

  * Processes: a process has the following attributes:
               - Can have a termination point
               - Has channels, through which it can emit and receive data/other channels, and thereby change state to another process
-}

data Process =
  Null
  | Action Channel
  | Prefix Channel Process
  | New Channel Process
  | Match Channel Channel Process
  | Replicate Process
  | Parallel Process Process
  | Choice Process Process

instance Show Process where
  show Null = "0"
  show (Action a) = show a
  show (Prefix a b) = (show a) ++ "." ++ (show b)
  show (New a b) = "(new " ++ (show a) ++ ")" ++ (show b)
  show (Match a b c) = "[" ++ (show a)
    ++"=" ++ (show b) ++ "]" ++ (show c)
  show (Replicate a) = "*" ++ (show a)
  show (Parallel a b) = (show a) ++ " | " ++ (show b)
  show (Choice a b) = (show a) ++ " + " ++ (show b)

