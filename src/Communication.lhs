Communication
=============

> module Communication(
>   connectToGateway, messageLoop, socketToChannels, sendReq, sendRsp,
>   MessageChan, MessageHandler
> ) where
>   import Prelude hiding (catch)
>   import Control.Concurrent
>   import Control.Concurrent.Chan
>   import Control.Exception
>   import Control.Monad (unless)
>   import Network.Socket
>   import System.IO
>   import System.Random
>   import Safe (readMay)
>
>   import Protocol

Client TCP Connection
---------------------

Client devices connect to the gatway via a TCP socket (code mostly copied from
[Real World Haskell][rwh-sockets]).

>   connectToGateway :: HostName -> String -> MessageChan -> MessageChan -> Bool
>     -> IO ()
>   connectToGateway host port send recv silent =
>     catch openSocketChannels onError
>     where openSocketChannels = withSocketsDo $
>             do addrinfos <- getAddrInfo Nothing (Just host) (Just port)
>                let serveraddr = head addrinfos
>                sock <- socket (addrFamily serveraddr) Stream defaultProtocol
>                setSocketOption sock KeepAlive 1
>                connect sock (addrAddress serveraddr)
>                socketToChannels sock send recv silent
>           onError = writeChan recv . Left . show :: SomeException -> IO ()

Thread-Safe Sockets
-------------------

The raw socket is not particularly convenient to use, and there are thread
safety concerns, so it is abstracted as a pair of `Message` channels: one in,
one out.

>   --                  socket    send channel   recv channel   silent
>   socketToChannels :: Socket -> MessageChan -> MessageChan -> Bool -> IO ()

Each channel can send/receive `Either` a `Message` or an error `String`.

>   type MessageChan = Chan (Either String Message)

Creating a connection spawns two new threads (one for send, one for recv), each
of which uses the provided channels. If the connection is closed for any reason,
the threads will terminate.

An error value on the send channel will cause the connection to close, while an
error value on the recv channel indicates that something external closed the
connection.

>   sendLoop :: Handle -> MessageChan -> IO ()
>   recvLoop :: Handle -> MessageChan -> IO ()
>
>   socketToChannels sock send recv silent =
>     do h <- socketToHandle sock ReadWriteMode
>        let println = unless silent . putStrLn
>            killChan c = writeChan c . Left
>            recvFail :: SomeException -> IO ()
>            recvFail e = do println $ "Recv thread died: " ++ show e
>                            killChan recv $ show e
>                            killChan send $ show e
>            defMsg = do killChan recv "Connection closed."
>                        killChan send "Connection closed."
>        recvThread <- forkIO $ catch (recvLoop h recv >> defMsg) recvFail
>
>        let sendFail :: SomeException -> IO ()
>            sendFail e = do println $ "Send thread died: " ++ show e
>                            throwTo recvThread e
>        forkIO $ catch (sendLoop h send) sendFail >> hClose h
>        return ()

Messages are sent over the wire as strings, using Haskell's default
`read`/`show` serialization. They are separated by newlines; this works as long
as all machines in the network use the same line endings, but **there may be
problems** if you try to communicate between, say, a Linux machine and a Windows
machine.

>   sendLoop h chan =
>     do next <- readChan chan
>        case next of
>          Right message -> hPutStrLn h (show message) >> sendLoop h chan
>          Left err -> error err
>
>   recvLoop h chan =
>     do message <- fmap parse (hGetLine h)
>        writeChan chan $ Right message
>        recvLoop h chan
>     where parse s = case readMay s :: Maybe Message of Just msg -> msg
>                                                        Nothing -> Unknown s

The Message Loop
----------------

Each client device (everything except the gateway) runs as a message loop, which
receives messages from the gateway or from user input. The message loop has
three parts: the _incoming message channel_, the _message handler_, and the
_state_. On completion, it returns a `String` describing why the loop ended.

>   messageLoop :: MessageChan -> MessageHandler st -> st -> IO String

The message handler is a function that takes a current state and a message,
performs some I/O action(s), then returns either

* `Right st`, a new state for the next message loop iteration, or
* `Left String`, an error message which ends the loop.

>   type MessageHandler st = st -> Message -> IO (Either String st)

Each iteration of the loop reads a message from the channel, executes the
handler, then tail-calls itself to continue the loop (if applicable) with a new
state value.

>   messageLoop c h s = mapRight (mapRight (messageLoop c h) . h s) (readChan c)
>   mapRight fn = (>>= \v -> case v of {Right r -> fn r; Left s -> return s})

Some common support functions for the message loop (mostly for sending messages)
are included here as well.

>   sendReq :: Request -> MessageChan -> IO MsgID
>   sendReq req chan = do mid <- randomIO :: IO MsgID
>                         writeChan chan $ Right $ Req mid req
>                         return mid
>
>   sendRsp :: MsgID -> Response -> MessageChan -> IO () 
>   sendRsp mid rsp chan = writeChan chan $ Right $ Rsp mid rsp

