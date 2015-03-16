Gateway
=======

> module Gateway(startGateway) where
>   import Control.Concurrent
>   import Control.Concurrent.Chan
>   import Control.Concurrent.STM
>   import Data.Foldable (for_)
>   import Data.Map (Map)
>   import qualified Data.Map as Map
>   import Network.Socket
>   import System.IO
>
>   import Protocol
>   import Communication

The TCP Server
--------------

The gateway's core functionality is a TCP server that receives connections from
other devices, then routes messages between the devices.

>   startGateway :: String -> IO ()
>   startGateway port = withSocketsDo $
>     do putStrLn $ "Starting gateway on port " ++ port ++ "..."

The following socket-creation code is adapted from
[Real World Haskell][rwh-sockets]:

To create the server socket, first, look up the port with `getAddrInfo`; this
either throws an exception or returns a nonempty list.

>        addrinfos <- getAddrInfo 
>                     (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
>                     Nothing (Just port)
>        let serveraddr = head addrinfos

Next, create a socket and bind it to the localhost address.

>        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
>        bindSocket sock (addrAddress serveraddr)

Finally, listen for connection requests in an endless loop; `5` is the maximum
queue size for waiting connection requests.

>        putStrLn "Listening for connections."
>        listen sock 5
>        st <- initGatewayState
>        procConnections st sock
>     where procConnections :: GatewayState -> Socket -> IO ()

Now that we have a server socket, each time we get a connection request from
a client, fork a thread to process incoming messages.

>           procConnections st mastersock =
>             do (connsock, clientaddr) <- accept mastersock
>                send <- newChan :: IO MessageChan
>                recv <- newChan :: IO MessageChan
>                socketToChannels connsock send recv
>                atomically (addChannel clientaddr send st)
>                forkFinally (routeMessages st clientaddr send recv Nothing) $
>                  \_ -> atomically (removeChannel clientaddr st)
>                procConnections st mastersock

[rwh-sockets]: http://book.realworldhaskell.org/read/sockets-and-syslog.html

Message Routing
---------------

The gateway takes all messages received from connected clients (whether they are
registered as devices or not), and either handles them itself or routes them to
their destination.

>   routeMessages :: GatewayState -> SockAddr -> MessageChan -> MessageChan
>     -> Maybe ID -> IO ()

Most messages are forwarded; `Request`s are forwarded to a target device (if it
exists), and `Response`s are forwarded to the sender of the relevant `Request`
(determined by the message's `MsgID`, which should be stored in the gateway's
global state).

>   routeMessages st addr send recv myID =
>     do next <- readChan recv
>        case next of
>          Right (Req mid req) -> handleReq mid req >>= recur
>          Right (Rsp mid rsp) ->
>            let fwdTo (Just chan) = writeChan chan next
>                fwdTo Nothing = putStrLn $ 
>                  "Failed to deliver response to message #" ++ show mid
>            in (atomically (getSenderChannel mid st) >>= fwdTo) >> recur myID
>          Right (Unknown s) -> putStrLn $ "Unparseable message: '" ++ s ++ "'"
>          Left err ->
>            do putStrLn $ "Connection to " ++ show addr ++ " closed: " ++ err
>               for_ myID $ \i -> do atomically $ removeDevice i st
>                                    putStrLn $ "Removed device with " ++ show i
>     where
>       recur = routeMessages st addr send recv 
>       sendMsg chan = writeChan chan . Right
>       handleReq :: MsgID -> Request -> IO (Maybe ID)

The `Register` request is handled by the gateway directly, and stores a known
device ID in the gateway's global state.

>       handleReq mid (Register d) = do
>         i <- atomically $ do
>           for_ myID $ (flip removeDevice) st
>           ni <- nextID st
>           let entry = DeviceEntry { deviceID       = ni
>                                   , deviceAddr     = addr
>                                   , deviceType     = d
>                                   , deviceSendChan = send }
>           addDevice entry st
>           return ni
>         putStrLn $ "Registered " ++ show d ++ " with " ++ show i
>         sendMsg send $ Rsp mid (RegisteredAs i)
>         return (Just i)

The `ChangeMode` request is also handled by the gateway, and sets the current
user state to `Home` or `Away`.

>       handleReq mid (ChangeMode mode) = do
>         atomically (writeTVar (userMode st) mode)
>         sendMsg send $ Rsp mid Success
>         return myID

All other requests are forwarded to a specificed device, if it exists.

>       handleReq mid req =
>         fwdReq mid req devID >> return myID
>         where devID = case req of QueryState i -> i
>                                   ReportState i _ -> i
>                                   ChangeState i _ -> i
>       fwdReq mid req i =
>         atomically (getDevice i st) >>= fwdTo
>         where fwdTo (Just e) = do
>                 atomically (addSender mid addr st)
>                 sendMsg (deviceSendChan e) $ Req mid req
>               fwdTo Nothing = sendMsg send $ Rsp mid (NoDevice i)

Global State and STM
--------------------

Thread safety is paramount in the gateway: every connected client has its own
thread, and those threads must maintain shared state that describes

* which channels send messages to which sockets,
* the original senders of all currently outstanding requests,
* the device IDs of all registered devices,
* the next available ID for new devices, and
* the current user state (`Home` vs. `Away`).

>   initGatewayState :: IO GatewayState
>
>   data GatewayState = GatewayState {
>     sendChannels :: TVar (Map SockAddr MessageChan),
>     senders      :: TVar (Map MsgID SockAddr),
>     devices      :: TVar (Map ID DeviceEntry),
>     idCounter    :: TVar ID,
>     userMode     :: TVar Mode
>   }

[Haskell's `stm` module][hs-stm] makes maintaining this state a relatively
simple task.  All modifications to the global gateway state are wrapped in
transactions, using the `atomically` function.

>   initGatewayState = atomically $ do
>     sc <- newTVar (Map.empty :: Map SockAddr MessageChan)
>     s  <- newTVar (Map.empty :: Map MsgID SockAddr)
>     d  <- newTVar (Map.empty :: Map ID DeviceEntry)
>     ic <- newTVar (ID 1)
>     um <- newTVar Home
>     return GatewayState { sendChannels = sc
>                         , devices      = d
>                         , senders      = s
>                         , idCounter    = ic
>                         , userMode     = um }

Registered devices are stored as records containing an ID, a socket address,
a device type, and a send channel.

>   data DeviceEntry = DeviceEntry {
>     deviceID       :: ID,
>     deviceAddr     :: SockAddr,
>     deviceType     :: Device,
>     deviceSendChan :: MessageChan
>   }

A set of IO functions can be used to access and/or manipulate the gateway state.

>   addChannel       :: SockAddr -> MessageChan -> GatewayState -> STM ()
>   removeChannel    :: SockAddr -> GatewayState -> STM ()
>   getDevice        :: ID -> GatewayState -> STM (Maybe DeviceEntry)
>   addDevice        :: DeviceEntry -> GatewayState -> STM ()
>   removeDevice     :: ID -> GatewayState -> STM ()
>   addSender        :: MsgID -> SockAddr -> GatewayState -> STM ()
>   removeSender     :: MsgID -> GatewayState -> STM ()
>   getSenderChannel :: MsgID -> GatewayState -> STM (Maybe MessageChan)
>   nextID           :: GatewayState -> STM ID

The commands are simple CRUD operations, but they use STM to guarantee that they
cannot be interleaved.

>   addChannel addr ch st = modifyTVar (sendChannels st) (Map.insert addr ch)
>   removeChannel addr st = modifyTVar (sendChannels st) (Map.delete addr)
>   getDevice i st        = fmap (Map.lookup i) (readTVar (devices st))
>   addDevice e st        = modifyTVar (devices st) (Map.insert (deviceID e) e)
>   removeDevice i st     = modifyTVar (devices st) (Map.delete i)
>   addSender i addr st   = modifyTVar (senders st) (Map.insert i addr)
>   removeSender i st     = modifyTVar (senders st) (Map.delete i)
>   getSenderChannel i st =
>     do s  <- readTVar (senders st)
>        sc <- readTVar (sendChannels st)
>        removeSender i st
>        return $ Map.lookup i s >>= (flip Map.lookup) sc
>   nextID GatewayState{idCounter = var} = 
>     readTVar var >>= swapTVar var . \(ID n) -> ID (n + 1)

[hs-stm]: https://hackage.haskell.org/package/stm

