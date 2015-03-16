Devices
=======

> module Devices(startDevice) where
>   import Control.Concurrent
>   import Control.Concurrent.Chan
>   import Control.Monad (unless)
>   import Network.Socket
>   import System.IO
>   import Safe (readMay)
>
>   import Protocol
>   import Communication

Device Console Interface
------------------------

All devices present a console interface that allows a user to query and/or
update the device's state directly.

>   startDevice :: Device -> HostName -> String -> Bool -> IO ()
>   startDevice dev host port silent =
>     do (send, recv, i) <- connectAndRegister dev host port silent

If the `silent` command-line argument was provided, this interface will not
output anything except the device's ID.

>        let println = unless silent . putStrLn

Every line from standard input is a command, sent as a `UserInput` message as
though it were received over a socket. The command `exit` bypasses this and
shuts down the application.

>            console = do unless silent $ do putStr "> "                         
>                                            hFlush stdout
>                         input <- getLine
>                         if input == "exit"
>                           then println "Goodbye!"
>                           else do writeChan recv $ Right (UserInput input)
>                                   unless silent $ threadDelay 1000
>                                   console
>        println $ nameOf dev ++ " console interface"
>        println $ instructions dev

A background thread runs concurrently with the interface, handling both network
and user input. In the event of an error, this thread will stop and an error
message will display, but the console interface will not immediately close.

>        bgThread <- forkIO $ do why <- bg dev send recv i println
>                                println $ "Background thread died: " ++ why
>        console
>        writeChan send $ Left "Console interface closed."
>
>   bg :: Device            -- Device type
>      -> MessageChan       -- send channel
>      -> MessageChan       -- recv channel
>      -> ID                -- Device ID
>      -> (String -> IO ()) -- println function
>      -> IO String         -- Return value: Error message
>
>   recur = return . Right

There are four kinds of devices: a _temperature sensor_, a _motion sensor_,
a _smart light bulb_, and a _smart outlet_.

>   nameOf :: Device -> String
>   nameOf Temp   = "Temperature Sensor"
>   nameOf Motion = "Motion Sensor"
>   nameOf dev    = "Smart " ++ show dev
>   instructions :: Device -> String
>   instructions Temp = "Enter an integer, 'state', or 'exit'."
>   instructions _ = "Enter 'on', 'off', 'state', or 'exit'."

Temperature Sensor
------------------

The temperature sensor has a local state that can be queried remotely, but can
only be set locally via the console interface.

>   bg Temp send recv _ println = messageLoop recv handle (DegreesCelsius 0)
>     where handle :: MessageHandler State

Entering an integer at the console interface will set the current temperature,
and entering `state` will print the current temperature.

>           handle st (UserInput "state") =
>             do case st of DegreesCelsius c -> println $ show c ++ "\0176C"
>                recur st
>           handle st (UserInput s) =
>             case readMay s :: Maybe Int of
>               Just c -> do println $ "Set temp to " ++ show c ++ "\0176C."
>                            recur $ DegreesCelsius c
>               Nothing -> println "Invalid input." >> recur st

Over the network, the sensor responds only to the `QueryState` request.

>           handle st (Req mid (QueryState _)) =
>             do sendRsp mid (HasState st) send
>                recur st
>           handle st (Req mid req) =
>             do sendRsp mid (NotSupported Temp req) send
>                recur st
>           handle st (Unknown s) =
>             do println $ "Unparseable message: '" ++ s ++ "'"
>                recur st
>           handle st _ = recur st

Motion Sensor
-------------

The motion sensor can only push state updates via `ReportState` messages.

>   bg Motion send recv i println =
>     messageLoop recv handle (MotionDetected False)
>     where handle :: MessageHandler State
>           setState v = do println $ "State changed to " ++ show v ++ "."
>                           let st = MotionDetected v
>                           writeChan send $ Right (Brc (ReportState i st))
>                           recur st

The console inputs `on` and `off` simulate the detector seeing motion/no
motion, respectively.

>           handle st (UserInput "on")  = setState True
>           handle st (UserInput "off") = setState False
>           handle st (UserInput "state") = println (show st) >> recur st
>           handle st (UserInput _) = println "Invalid input." >> recur st

Over the network, the sensor responds only to the `QueryState` request.

>           handle st (Req mid (QueryState _)) =
>             do sendRsp mid (HasState st) send
>                recur st
>           handle st (Req mid req) =
>             do sendRsp mid (NotSupported Motion req) send
>                recur st
>           handle st (Unknown s) =
>             do println $ "Unparseable message: '" ++ s ++ "'"
>                recur st
>           handle st _ = recur st

Smart Light Bulbs and Outlets
-----------------------------

Both of these devices behave more or less identically, so they use the same
code. The only functionality these devices provide is an on/off state: the state
can be set and queried via messages.

>   bg dev send recv i println = messageLoop recv handle (Power Off)
>     where handle :: MessageHandler State
>           setState o = do println $ "State changed to " ++ show o ++ "."
>                           recur (Power o)

The console interface allows a user or script to set the state with `on` or
`off`, and query the state with `state`.

>           handle st (UserInput "on")  = setState On
>           handle st (UserInput "off") = setState Off
>           handle st (UserInput "state") = println (show st) >> recur st
>           handle st (UserInput _) = println "Invalid input." >> recur st

The background thread responds to the `QueryState` and `ChangeState` requests.

>           handle st (Req mid (QueryState _)) =
>             do sendRsp mid (HasState st) send
>                recur st
>           handle st (Req mid (ChangeState _ o)) = 
>             do sendRsp mid Success send
>                setState o
>           handle st (Req mid req) =
>             do sendRsp mid (NotSupported dev req) send
>                recur st
>           handle st (Unknown s) =
>             do println $ "Unparseable message: '" ++ s ++ "'"
>                recur st
>           handle st _ = recur st

Connecting to the Gateway
-------------------------

>   connectAndRegister :: Device -> HostName -> String -> Bool
>     -> IO (MessageChan, MessageChan, ID)

Each device, when it starts, contacts the gateway and attempts to

* establish a TCP connection, and
* send a `Register` command and acquire a device ID.

If either step fails, an exception will be thrown and the application will
close.

>   connectAndRegister dev host port silent =
>     do send <- newChan :: IO MessageChan
>        recv <- newChan :: IO MessageChan
>        connectToGateway host port send recv silent
>        mid <- sendReq (Register dev) send
>        rspMsg <- readChan recv
>        case rspMsg of Right (Rsp mid' rsp) -> 
>                         if mid == mid' then handleRsp rsp send recv
>                                        else wrongRsp
>                       Right _ -> wrongRsp
>                       Left s -> error s
>     where handleRsp (RegisteredAs (ID i)) send recv =
>             do putStrLn $ if silent then show i
>                                     else "Connected with ID " ++ show i
>                hFlush stdout
>                return (send, recv, ID i)
>           handleRsp rsp _ _ = error $ 
>             "Invalid response to Register: " ++ show rsp
>           wrongRsp = error "Got something other than response to Register."

