Devices
=======

> module Devices(startDevice) where
>   import Control.Concurrent
>   import Control.Concurrent.Chan
>   import Control.Monad (unless)
>   import Control.Concurrent.STM
>   import Network.Socket
>   import System.IO
>   import System.Random
>   import Text.Read (readMaybe)
>
>   import Protocol
>   import Communication
>
>   startDevice :: Device -> HostName -> String -> Bool -> IO ()

Temperature Sensor
------------------

The temperature sensor has a local state that can be queried remotely, but can
only be set locally via the console interface.

>   startDevice Temp host port silent =
>     do (send, recv, _) <- connectAndRegister Temp host port silent
>        state <- atomically $ newTVar (DegreesCelsius 0)
>        let println = unless silent . putStrLn
>        println "Temperature Sensor console interface"
>        println "Enter an integer, 'state', or 'exit'."

Entering an integer at the console interface will set the current temperature,
entering `state` will print the current temperature, and entering `exit` will
shut down the device.

>        let eval "exit" = println "Goodbye!"
>            eval "state" = do st <- readTVarIO state
>                              case st of DegreesCelsius c ->
>                                           println $ show c ++ "\0176C"
>            eval s = case readMaybe s :: Maybe Int of
>                       Just c -> 
>                         do atomically $ writeTVar state (DegreesCelsius c)
>                            println $ "Set temp to " ++ show c ++ "\0176C."
>                       Nothing -> println "Invalid input."

The background thread responds to only the `QueryState` request.

>            bg = do next <- readChan recv
>                    case next of
>                      Right (Req mid req) -> handle mid req >> bg
>                      Right (Unknown s) -> 
>                        putStrLn $ "Unparseable message: '" ++ s ++ "'"
>                      Left s -> println $ "Background thread died: " ++ s
>                      _ -> bg
>                 where handle mid QueryState{} =
>                         do st <- readTVarIO state
>                            sendRsp mid (HasState st) send
>                       handle mid r = sendRsp mid (NotSupported Temp r) send
>        forkIO bg
>        console eval silent
>        writeChan send $ Left "Console interface closed."

Motion Sensor
-------------

The motion sensor can only push state updates via `ReportState` messages.

>   startDevice Motion host port silent =
>     do (send, recv, i) <- connectAndRegister Motion host port silent
>        let println = unless silent . putStrLn
>        println "Motion Sensor console interface"
>        println "'exit' to exit; anything else + ENTER to send signal."

The console interface will push an update anytime it receives a line of input,
unless that line is `exit`.

>        let eval "exit" = println "Goodbye!"
>            eval _ = do println "Motion detected!"
>                        sendReq (ReportState i MotionDetected) send
>                        return ()

The background thread will respond with `NotSupported` to any and all requests.

>            bg = do next <- readChan recv
>                    case next of
>                      Right (Req mid req) ->
>                        sendRsp mid (NotSupported Motion req) send >> bg
>                      Right (Unknown s) -> 
>                        putStrLn $ "Unparseable message: '" ++ s ++ "'"
>                      Left s -> println $ "Background thread died: " ++ s
>                      _ -> bg
>        forkIO bg
>        console eval silent
>        writeChan send $ Left "Console interface closed."

Smart Light Bulbs and Outlets
-----------------------------

Both of these devices behave more or less identically, so they use the same
code. The only functionality these devices provide is an on/off state: the state
can be set and queried via messages.

>   startDevice dev host port silent =
>     do (send, recv, _) <- connectAndRegister dev host port silent
>        state <- atomically $ newTVar (Power Off)
>        let println = unless silent . putStrLn
>        println $ "Smart " ++ show dev ++ " console interface"
>        println "Enter 'on', 'off', 'state', or 'exit'."
>        let setState o = do atomically $ writeTVar state (Power o)
>                            println $ "State changed to " ++ show o ++ "."

The console interface allows a user or script to set the state with `on` or
`off`, query the state with `state`, and shut down the device with `exit`.

>            eval "on"    = setState On
>            eval "off"   = setState Off
>            eval "state" = readTVarIO state >>= println . show
>            eval "exit"  = println "Goodbye!"
>            eval _       = println "Invalid input."

The background thread responds to the `QueryState` and `ChangeState` requests.

>            bg = do next <- readChan recv
>                    case next of
>                      Right (Req mid req) -> handle mid req >> bg
>                      Right (Unknown s) -> 
>                        putStrLn $ "Unparseable message: '" ++ s ++ "'"
>                      Left s -> println $ "Background thread died: " ++ s
>                      _ -> bg
>                 where handle mid QueryState{} =
>                         do st <- readTVarIO state
>                            sendRsp mid (HasState st) send
>                       handle mid (ChangeState _ o) = 
>                         setState o >> sendRsp mid Success send
>                       handle mid r = sendRsp mid (NotSupported dev r) send
>        forkIO bg
>        console eval silent
>        writeChan send $ Left "Console interface closed."

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
>        connectToGateway host port send recv
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
>                return (send, recv, ID i)
>           handleRsp rsp _ _ = error $ 
>             "Invalid response to Register: " ++ show rsp
>           wrongRsp = error "Got something other than response to Register."

Support Functions
-----------------

>   sendReq :: Request -> MessageChan -> IO MsgID
>   sendReq req chan = do mid <- randomIO :: IO MsgID
>                         writeChan chan $ Right $ Req mid req
>                         return mid
>
>   sendRsp :: MsgID -> Response -> MessageChan -> IO () 
>   sendRsp mid rsp chan = writeChan chan $ Right $ Rsp mid rsp
>
>   console :: (String -> IO ()) -> Bool -> IO ()
>   console fn silent = do unless silent $ putStr "> "
>                          hFlush stdout
>                          input <- getLine
>                          fn input
>                          unless (input == "exit") (console fn silent)

