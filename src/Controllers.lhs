Controllers
===========

This is a slight departure from the specification: controllers are programs
which register themselves with the gateway like devices, receive push updates
from the gateway, and send commands to other devices (which must be specified by
ID when the controller starts).

> module Controllers(startController, Controller(..)) where
>   import Control.Concurrent
>   import Control.Concurrent.Chan
>   import Control.Monad
>   import System.IO
>   
>   import Protocol
>   import Communication
>
>   startController :: Controller -> String -> String -> IO ()
>   data Controller = Heater | Light | UserInterface deriving Show

Common Functionality
--------------------

Most of the controllers start out by asking the user for device IDs.

>   askForDeviceID :: String -> IO ID
>   askForDeviceID name = do putStr $ name ++ " ID: "
>                            hFlush stdout
>                            n <- readLn :: IO Int
>                            return (ID n)

Also, all controllers connect to the gateway and subscribe themselves to push
updates from other devices.

>   connectAndSubscribe :: String -> String -> IO (MessageChan, MessageChan)
>   connectAndSubscribe host port =
>     do send <- newChan :: IO MessageChan
>        recv <- newChan :: IO MessageChan
>        connectToGateway host port send recv
>        mid <- sendReq Subscribe send
>        rspMsg <- readChan recv
>        case rspMsg of Right (Rsp mid' rsp) -> 
>                         if mid == mid' then handleRsp rsp send recv
>                                        else wrongRsp
>                       Right _ -> wrongRsp
>                       Left s -> error s
>     where handleRsp Success send recv =
>             do putStrLn "Subscribed to broadcasts from gateway."
>                return (send, recv)
>           handleRsp rsp _ _ = error $ 
>             "Invalid response to Subscribe: " ++ show rsp
>           wrongRsp = error "Got something other than response to Subscribe."

Finally, some convenient shorthand definitions:

>   recur = return . Right
>   die   = return . Left

Task 1: Preventing Water Pipe Bursts
------------------------------------

The _heater controller_ checks periodically (every second) for temperature
changes from a temperature sensor, then changes the state of a smart outlet if
the temperature passes certain thresholds.

>   heatOnThreshold  = 1
>   heatOffThreshold = 2
>   tempCheckIntervalMicros = 1000000
>
>   startController Heater host port =
>     do (send, recv) <- connectAndSubscribe host port

When the controller starts, it asks the user for the IDs of a temperature sensor
and a smart outlet.

>        tempID   <- askForDeviceID "Temperature Sensor"
>        outletID <- askForDeviceID "Smart Outlet"

The controller then queries every second for the current temperature, updating
the smart outlet if applicable.

>        putStrLn "Running controller..."
>        let handle :: MessageHandler MsgID
>            handle mid' (Rsp mid rsp)
>              | mid == mid' = 
>                  case rsp of 
>                    HasState (DegreesCelsius c) ->
>                      do when (c <= heatOnThreshold) $ do
>                           sendReq (ChangeState outletID On) send
>                           putStrLn "Heat on."
>                         when (c >= heatOffThreshold) $ do
>                           sendReq (ChangeState outletID Off) send
>                           putStrLn "Heat off."
>                         threadDelay tempCheckIntervalMicros
>                         mid'' <- sendReq (QueryState tempID) send
>                         recur mid''
>                    _ -> die $ "Unexpected " ++ show rsp
>              | otherwise = recur mid'
>            handle st r = putStrLn (show r) >> recur st
>        mid <- sendReq (QueryState tempID) send
>        why <- messageLoop recv handle mid
>        putStrLn $ "Controller died: " ++ why

Task 2: Preparing for Spring Break
----------------------------------

