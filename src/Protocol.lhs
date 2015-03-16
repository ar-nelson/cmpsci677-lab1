Protocol
========

This project uses a simple network protocol, which is almost a direct
translation of the types and method signatures from the 
[project specification][spec] into Haskell types.

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
>
> module Protocol(
>   Device(..), Mode(..), OnOff(..), State(..), ID(..), MsgID(..),
>   Request(..), Response(..), Message(..)
> ) where
>
>   import System.Random
>
>   data Device = Temp | Motion | Bulb | Outlet deriving (Eq, Show, Read)
>   data Mode   = Home | Away deriving (Eq, Show, Read)
>   data OnOff  = On | Off deriving (Eq, Show, Read)
>   data State  = DegreesCelsius Int |
>                 Power OnOff        |
>                 MotionDetected
>                 deriving (Eq, Show, Read)
>
>   newtype ID = ID Int deriving (Eq, Ord, Show, Read)

A `Request` is an RPC, which the gateway will either forward to a device (if it
contains an `ID`) or handle directly (if it does not contain an `ID`).

>   data Request = Register Device      |
>                  QueryState ID        |
>                  ReportState ID State |
>                  ChangeState ID OnOff |
>                  ChangeMode Mode
>                  deriving (Show, Read)

A `Response` is the return value of an RPC. Some `Response`s are valid return
values, while others (the ones starting with `No` or `Not`) are error responses.

>   data Response = Success                     |
>                   RegisteredAs ID             |
>                   HasState State              |
>                   NoDevice ID                 |
>                   NotSupported Device Request
>                   deriving (Show, Read)

A `Message` is a `Request` or `Response`, with an attached message ID. The
message ID is a randomly-assigned integer used by the gateway to match responses
to requests.

>   data Message = Req MsgID Request  |
>                  Rsp MsgID Response |
>                  UserInput String   |
>                  Unknown String
>                  deriving (Show, Read)
>
>   newtype MsgID = MsgID Int deriving (Eq, Ord, Show, Read, Random)

[spec]: http://lass.cs.umass.edu/~shenoy/courses/spring15/labs/lab1.html

