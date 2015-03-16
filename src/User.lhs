User Interface
==============

> module User(startUserInterface) where
>   import Control.Concurrent.Chan
>   import Network.Socket
>   import System.IO
>   import System.Random
>   import Text.Read (readMaybe)
>
>   import Protocol
>   import Communication

>   startUserInterface :: String -> String -> IO ()
>   startUserInterface host port =
>     do send <- newChan :: IO MessageChan
>        recv <- newChan :: IO MessageChan
>        connectToGateway host port send recv
>        putStrLn $ "Connected to host " ++ host ++ ":" ++ port ++ "."
>        putStrLn "Enter commands in Haskell expression form."
>        putStrLn "Examples: \"QueryState (ID 1)\""
>        putStrLn "          \"ChangeState (ID 2) On\""
>        putStrLn "          \"ChangeMode Home\""
>        putStrLn "          \"ChangeMode Away\""
>        let console = 
>              do putStr "> "
>                 hFlush stdout
>                 input <- getLine
>                 if input /= "exit"
>                   then do case readMaybe input :: Maybe Request of
>                             Just req -> sendReq req
>                             Nothing -> putStrLn "Invalid input."
>                           console
>                   else putStrLn "Goodbye!"
>              where sendReq req = do mid <- randomIO :: IO MsgID
>                                     writeChan send $ Right $ Req mid req
>                                     waitForRsp mid
>                    waitForRsp mid =
>                      do next <- readChan recv
>                         case next of Right (Rsp mid' rsp) ->
>                                        do putStrLn $ showRsp rsp
>                                           if mid == mid' then return ()
>                                                          else waitForRsp mid
>                                      Right m -> do putStrLn $ show m
>                                                    waitForRsp mid
>                                      Left s -> putStrLn $ "Error: " ++ s
>        console
>        writeChan send $ Left "User interface closed."

>   showRsp :: Response -> String
>   showRsp Success = "Success!"
>   showRsp (RegisteredAs i) = "Registered with " ++ show i
>   showRsp (HasState s) = "State: " ++ showState s
>     where showState (DegreesCelsius c) = show c ++ "\0176C"
>           showState MotionDetected = "Motion detected!"
>           showState (Power p) = show p
>   showRsp (NoDevice id) = "Error: No device with " ++ show id
>   showRsp (NotSupported dev req) =
>       "Error: " ++ show dev ++ " does not support request " ++ show req

