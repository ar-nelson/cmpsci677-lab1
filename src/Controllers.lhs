Controllers
===========

This is a slight departure from the specification: controllers are programs
which register themselves with the gateway like devices, receive push updates
from the gateway, and send commands to other devices (which must be specified by
ID when the controller starts).

> module Controllers(startController, Controller(..)) where
>   import Control.Concurrent
>   import Control.Concurrent.Chan
>   import Control.Concurrent.Suspend.Lifted (mDelay)
>   import Control.Concurrent.Timer
>   import Control.Monad
>   import System.IO
>   
>   import Protocol
>   import Communication
>
>   startController :: Controller -> String -> String -> Bool -> IO ()
>   data Controller = Heater | Light | UserInterface deriving Show

Common Functionality
--------------------

Most of the controllers start out by asking the user for device IDs.

>   askForDeviceID :: String -> Bool -> IO ID
>   askForDeviceID name silent = do unless silent $ do putStr (name ++ " ID: ")
>                                                      hFlush stdout
>                                   n <- readLn :: IO Int
>                                   return (ID n)

Also, all controllers connect to the gateway and subscribe themselves to push
updates from other devices.

>   connectAndSubscribe :: String -> String -> Bool
>     -> IO (MessageChan, MessageChan)
>   connectAndSubscribe host port silent =
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
>             do unless silent $ 
>                  putStrLn "Subscribed to broadcasts from gateway."
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
>   startController Heater host port silent =
>     do (send, recv) <- connectAndSubscribe host port silent
>        let println = unless silent . putStrLn

When the controller starts, it asks the user for the IDs of a temperature sensor
and a smart outlet.

>        tempID   <- askForDeviceID "Temperature Sensor" silent
>        outletID <- askForDeviceID "Smart Outlet" silent

The controller then queries every second for the current temperature, updating
the smart outlet if applicable.

>        println "Running controller..."
>        let handle :: MessageHandler MsgID
>            handle mid' (Rsp mid rsp)
>              | mid == mid' = 
>                  case rsp of 
>                    HasState (DegreesCelsius c) ->
>                      do when (c <= heatOnThreshold) $ do
>                           sendReq (ChangeState outletID On) send
>                           println "Heat on."
>                         when (c >= heatOffThreshold) $ do
>                           sendReq (ChangeState outletID Off) send
>                           println "Heat off."
>                         threadDelay tempCheckIntervalMicros
>                         mid'' <- sendReq (QueryState tempID) send
>                         recur mid''
>                    _ -> die $ "Unexpected " ++ show rsp
>              | otherwise = recur mid'
>            handle st r = println (show r) >> recur st
>        mid <- sendReq (QueryState tempID) send
>        why <- messageLoop recv handle mid
>        println $ "Controller died: " ++ why

Task 2: Preparing for Spring Break
----------------------------------

The _light controller_ detects motion and turns a smart light bulb on and off in
response. It also detects the `ChangeMode` broadcast message and adjusts its
behavior accordingly: if the user is away, it will not turn the light on, but
will send a `TextMessage` broadcast when motion is detected.

>   startController Light host port silent =
>     do (send, recv) <- connectAndSubscribe host port silent
>        let println = unless silent . putStrLn

When the controller starts, it asks the user for the IDs of a temperature sensor
and a smart outlet.

>        motionID <- askForDeviceID "Motion Sensor" silent
>        bulbID   <- askForDeviceID "Smart Light Bulb" silent

A restartable timer is used to turn the light off after a delay. It starts out
initialized: if the light was on before the controller was started, it will tirn
off in 5 minutes.

>        let turnOff = do sendReq (ChangeState bulbID Off) send
>                         println "Turning light off."
>            delay   = mDelay 5 -- 5 minutes
>        timer <- oneShotTimer turnOff delay
>        let resetTimer = do itWorked <- oneShotStart timer turnOff delay
>                            unless itWorked resetTimer

The controller responds only to broadcast (push) messages.

>        println "Running controller..."
>        let awayMsg = TextMessage "Motion detected while user is away!"
>            handle :: MessageHandler Mode
>            handle _ (Brc (ChangeMode st')) =
>              do println $ "Set user mode to " ++ show st'
>                 recur st'
>            handle Home (Brc (ReportState i MotionDetected)) =
>              do when (i == motionID) $ do sendReq (ChangeState bulbID On) send
>                                           println "Turning light on."
>                                           resetTimer
>                 recur Home
>            handle Away (Brc (ReportState i MotionDetected)) =
>              do when (i == motionID) $ writeChan send $ Right (Brc awayMsg)
>                 recur Away
>            handle st (Rsp _ rsp) = println (show rsp) >> recur st
>            handle st _ = recur st
>        why <- messageLoop recv handle Home
>        println $ "Controller died: " ++ why

