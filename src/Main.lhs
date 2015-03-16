% CMPSCI 677 Lab 1: Internet of Things
% Adam R. Nelson
% March 16, 2015

Overview
========

Purpose
-------

This project is a simulation of a network of "smart devices" (sometimes
referred to as the _Internet of Things_). Each simulated device is a CLI
Haskell program that communicates with the other devices via TCP messages
relayed through a central gateway.

Execution
---------

All four entities required by the project (_Sensor_, _Smart Device_, _Gateway_,
and _User_) are contained in a single executable.

> module Main where
>   import System.Environment
>   import Protocol
>   import Communication
>   import Devices
>   import Gateway
>   import Controllers
>   import User
>
>   main :: IO ()
>   main = do
>     args <- getArgs
>     case args of
>       command : host : port : "silent" : [] -> start command host port True
>       command : host : port : [] -> start command host port False
>       _ -> usage

The executable must be given one of six valid command-line arguments.

>   usage :: IO ()
>   usage = do
>     prog <- getProgName
>     putStrLn $ unlines [
>       "Usage: " ++ prog ++ " <command> <host> <port> [silent]",
>       "  <command> is one of [temp, motion, bulb, outlet, gateway, user, heater-control, light-control].",
>       "  <host> is the gateway hostname (0.0.0.0 for the gateway itself).",
>       "  <port> is the gateway TCP port.",
>       "  'silent' is optional; if provided:",
>       "    1. console output will be minimal (except gateway/user), and",
>       "    2. devices will output only their device ID on a line by itself."]

The argument determines the kind of entity that the executable will become.

>   start :: String -> String -> String -> Bool -> IO ()
>   start "temp" h p s   = startDevice Temp h p s
>   start "motion" h p s = startDevice Motion h p s
>   start "bulb" h p s   = startDevice Bulb h p s
>   start "outlet" h p s = startDevice Outlet h p s
>   start "gateway" _ port _ = startGateway port
>   start "user" host port _ = startUserInterface host port
>   start "heater-control" host port _ = startHeaterController host port

