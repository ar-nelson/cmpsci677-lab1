Controllers
===========

This is a slight departure from the specification: controllers are programs
which register themselves with the gateway like devices, receive push updates
from the gateway, and send commands to other devices (which must be specified by
ID when the controller starts).

> module Controllers(startController, Controller(..)) where
>   import Control.Concurrent
>   import Control.Concurrent.Chan
>   import Control.Concurrent.STM
>   import Control.Concurrent.Suspend.Lifted (mDelay)
>   import Control.Concurrent.Timer
>   import Control.Monad
>   import Data.Time.Clock
>   import Safe (readMay)
>   import System.IO
>   
>   import Protocol
>   import Communication
>
>   startController :: Controller -> String -> String -> Bool -> IO ()
>   data Controller = Heater | Light | UserInterface | TestLogger

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
>        connectToGateway host port send recv silent
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
initialized: if the light was on before the controller was started, it will turn
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
>            handle Home (Brc (ReportState i (MotionDetected True))) =
>              do when (i == motionID) $ do sendReq (ChangeState bulbID On) send
>                                           println "Turning light on."
>                                           stopTimer timer
>                 recur Home
>            handle Home (Brc (ReportState i (MotionDetected False))) =
>              do when (i == motionID) $ do println "Setting light timer."
>                                           resetTimer
>                 recur Home
>            handle Away (Brc (ReportState i (MotionDetected True))) =
>              do when (i == motionID) $ writeChan send $ Right (Brc awayMsg)
>                 recur Away
>            handle st (Rsp _ rsp) = println (show rsp) >> recur st
>            handle st _ = recur st
>        why <- messageLoop recv handle Home
>        println $ "Controller died: " ++ why

User Interface
--------------

The user interface is a special kind of controller: it takes console input in
the form of Haskell commands, and sends it directly to the gateway. It also
reports responses and broadcast messages to the console.

>   startController UserInterface host port silent =
>     do (send, recv) <- connectAndSubscribe host port silent
>        let println = unless silent . putStrLn

Its console interface is identical to the interface used by devices.

>            console = do unless silent $ do putStr "> "                         
>                                            hFlush stdout
>                         input <- getLine
>                         if input == "exit"
>                           then println "Goodbye!"
>                           else do writeChan recv $ Right (UserInput input)
>                                   unless silent $ threadDelay 100000
>                                   console

It maintains a background thread that handles both user and network input. All
network input is printed to the screen, even in silent mode (although silent
mode uses less pretty printing).

>            handle :: MessageHandler ()
>            handle _ (Req _ req) = do putStrLn $ "REQUEST: " ++ show req
>                                      recur ()
>            handle _ (Rsp _ rsp) = 
>              do putStrLn $ "RESPONSE: " ++ if silent then show rsp
>                                                      else showRsp rsp
>                 recur ()
>            handle _ (Brc brc)   = do putStrLn $ "BROADCAST: " ++ show brc
>                                      recur ()
>            handle _ (Unknown s) = do putStrLn $ "UNPARSEABLE: " ++ s
>                                      recur ()

User input is in the form of valid Haskell expressions for `Request`s or
`Broadcast`s.

>            handle _ (UserInput s) =
>              do case readMay s :: Maybe Request of
>                   Just req -> sendReq req send >> return ()
>                   Nothing -> case readMay s :: Maybe Broadcast of
>                                Just brc -> writeChan send $ Right (Brc brc)
>                                Nothing -> println "Invalid input."
>                 recur ()

The console interface displays a detailed help message on startup.

>        println $ "Connected to host " ++ host ++ ":" ++ port ++ "."
>        println "\nEnter commands in Haskell expression form."
>        println "Examples: \"QueryState (ID 1)\""
>        println "          \"ChangeState (ID 2) On\""
>        println "          \"ChangeMode Home\""
>        println "          \"ChangeMode Away\""
>
>        bgThread <- forkIO $ do why <- messageLoop recv handle ()
>                                println $ "Background thread died: " ++ why
>        console
>        writeChan send $ Left "Console interface closed."

When not in silent mode, responses are pretty-printed to make them more
human-readable.

>     where showRsp :: Response -> String
>           showRsp Success = "Success!"
>           showRsp (RegisteredAs i) = "Registered with " ++ show i
>           showRsp (HasState (DegreesCelsius c)) = show c ++ "\0176C"
>           showRsp (HasState (MotionDetected True)) = "Motion detected!"
>           showRsp (HasState (MotionDetected False)) = "No motion detected."
>           showRsp (HasState (Power p)) = show p
>           showRsp (NoDevice id) = "Error: No device with " ++ show id
>           showRsp (NotSupported dev req) =
>             "Error: " ++ show dev ++ " does not support request " ++ show req

Test Controller
---------------

This controller was written specifically to produce test output compatible with
the lab instructions. It connects to a temperature sensor and a motion sensor.

>   startController TestLogger host port silent =
>     do (send, recv) <- connectAndSubscribe host port silent
>        let println = unless silent . putStrLn
>            getTime = getCurrentTime >>= return . utctDayTime
>        tempID   <- askForDeviceID "Temperature Sensor" silent
>        motionID <- askForDeviceID "Motion Sensor" silent
>        startTime <- getTime
>        println "TEST CONTROLLER -- This is meant to be run by a script."
>        temp <- atomically $ newTVar 0
>        motion <- atomically $ newTVar False

Output lines have the format `<time>:<temp>,<motion>`. If `silent` is true, then
these log lines will be the only output.

>        let log = do time <- getTime
>                     (ctemp, cmotion) <- atomically $ do t <- readTVar temp
>                                                         m <- readTVar motion
>                                                         return (t, m)
>                     putStrLn $ show (time - startTime) ++ ":" ++ show ctemp
>                       ++ "," ++ if cmotion then "1" else "0"
>                     hFlush stdout

Commands from `stdin` which correspond to the commands in the test CSV file are
converted into remote calls.

>            console = do input <- getLine
>                         if input == "exit"
>                           then println "Goodbye!"
>                           else do writeChan recv $ Right (UserInput input)
>                                   console
>            handle :: MessageHandler ()
>            handle _ (UserInput "") = log >> recur ()
>            handle _ (UserInput "Q(Temp)") =
>              do sendReq (QueryState tempID) send
>                 recur ()
>            handle _ (UserInput "Q(Motion)") =
>              do sendReq (QueryState motionID) send
>                 recur ()
>            handle _ (UserInput "Q(Temp);Q(Motion)") =
>              do sendReq (QueryState tempID) send
>                 sendReq (QueryState motionID) send
>                 recur ()
>            handle _ (UserInput s) = putStrLn ("Invalid: " ++ s) >> recur ()

Log output is written any time state information is received, from either a push
or pull message.

>            handle _ (Rsp _ (HasState (DegreesCelsius temp'))) =
>              atomically (writeTVar temp temp') >> log >> recur ()
>            handle _ (Rsp _ (HasState (MotionDetected motion'))) =
>              atomically (writeTVar motion motion') >> log >> recur ()
>            handle _ (Brc (ReportState i (MotionDetected motion')))
>              | motionID == i = do atomically $ writeTVar motion motion'
>                                   log
>                                   recur ()
>              | otherwise = recur ()
>            handle _ _ = recur ()
>        log
>        bgThread <- forkIO $ do why <- messageLoop recv handle ()
>                                println $ "Background thread died: " ++ why
>        console
>        writeChan send $ Left "Console interface closed."

