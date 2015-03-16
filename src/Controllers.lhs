Controllers
===========

This is a slight departure from the specification: controllers are programs
which register themselves with the gateway like devices, receive push updates
from the gateway, and send commands to other devices (which must be specified by
ID when the controller starts).

> module Controllers(startHeaterController, startLightController) where
>   import Control.Concurrent
>   import Control.Concurrent.Chan
>   import Control.Monad
>   import System.IO
>   
>   import Protocol
>   import Communication

Task 1: Preventing Water Pipe Bursts
------------------------------------

The _heater controller_ checks periodically (every second) for temperature
changes from a temperature sensor, then changes the state of a smart outlet if
the temperature passes certain thresholds.

>   startHeaterController :: String -> String -> IO ()
>
>   heatOnThreshold  = 1
>   heatOffThreshold = 2
>   tempCheckIntervalMicros = 1000000

When the controller starts, it asks the user for the IDs of a temperature sensor
and a smart outlet.

>   startHeaterController host port =
>     do putStr "Temperature Sensor ID: "
>        hFlush stdout
>        tempN <- readLn :: IO Int
>        putStr "Smart Outlet ID: "
>        hFlush stdout
>        outletN <- readLn :: IO Int
>        let tempID = ID tempN
>            outletID = ID outletN

The controller then queries every second for the current temperature, updating
the smart outlet if applicable.

>        putStrLn "Running controller..."
>        send <- newChan :: IO MessageChan
>        recv <- newChan :: IO MessageChan
>        connectToGateway host port send recv
>        let handle :: MessageHandler MsgID
>            handle mid' (Rsp mid rsp)
>              | mid == mid' = 
>                  case rsp of 
>                    HasState (DegreesCelsius c) ->
>                      do when (c <= heatOnThreshold) $ do
>                           putStrLn "Heat on."
>                           sendReq (ChangeState outletID On) send
>                           return ()
>                         when (c >= heatOffThreshold) $ do
>                           putStrLn "Heat off."
>                           sendReq (ChangeState outletID Off) send
>                           return ()
>                         threadDelay tempCheckIntervalMicros
>                         mid'' <- sendReq (QueryState tempID) send
>                         return $ Right mid''
>                    _ -> return $ Left $ "Unexpected " ++ show rsp
>              | otherwise = return $ Right mid'
>            handle st (Req _ ReportState{}) = return (Right st)
>            handle st r = putStrLn (show r) >> return (Right st)
>        mid <- sendReq (QueryState tempID) send
>        why <- messageLoop recv handle mid
>        putStrLn $ "Controller died: " ++ why

Task 2: Preparing for Spring Break
----------------------------------

>   startLightController :: String -> String -> IO ()
>   startLightController = undefined

