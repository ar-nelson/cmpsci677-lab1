Main
====

The `Main` module is the entry point of the program.

> module Main where
>   import System.Environment
>
>   import Protocol
>   import Communication
>   import Gateway
>   import Devices
>   import Controllers

The program takes 3 arguments (the first of which may be either 1 or 2 words),
plus an optional fourth argument (`silent`). If any other arguments are given,
it will display a usage message.

>   usage :: IO ()
>   usage = do
>     prog <- getProgName
>     putStrLn $ unlines [
>       "Usage: " ++ prog ++ " <command> <host> <port> [silent]",
>       "  <command> is one of [temp, motion, bulb, outlet, gateway, control <x>],",
>       "    where <x> is one of [heater, light, user].",
>       "  <host> is the gateway hostname (0.0.0.0 for the gateway itself).",
>       "  <port> is the gateway TCP port.",
>       "  'silent' is optional, and not valid with 'gateway'. If provided,",
>       "    1. console output will be minimal, and",
>       "    2. devices will output only their device ID on a line by itself."]

The first argument is the name of the subprogram to run: either `gateway`,
a device name (`temp`, `motion`, `bulb`, `outlet`), or `control` followed by
a controller name (`heater`, `light`, or `user`).

The second and third arguments are the hostname and port number of the gateway
to connect to (or launch).

>   main :: IO ()
>   main = do
>     args <- getArgs
>     case args of
>       "control" : c : host : port : "silent" : [] -> control c host port True
>       "control" : c : host : port : [] -> control c host port False
>       command : host : port : "silent" : [] -> start command host port True
>       command : host : port : [] -> start command host port False
>       _ -> usage

All further execution occurs in one of the `Gateway`, `Devices` or `Controllers`
modules.

>   start :: String -> String -> String -> Bool -> IO ()
>   start "temp" h p s   = startDevice Temp h p s
>   start "motion" h p s = startDevice Motion h p s
>   start "bulb" h p s   = startDevice Bulb h p s
>   start "outlet" h p s = startDevice Outlet h p s
>   start "gateway" _ port _ = startGateway port
>   start _ _ _ _ = usage
>   
>   control :: String -> String -> String -> Bool -> IO ()
>   control "heater" h p s = startController Heater h p s
>   control "light"  h p s = startController Light h p s
>   control "user"   h p s = startController UserInterface h p s
>   control _ _ _ _ = usage

