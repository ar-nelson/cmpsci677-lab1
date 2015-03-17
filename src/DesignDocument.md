% CMPSCI 677 Lab 1: Internet of Things
% Adam R. Nelson
% March 16, 2015

Design Document
===============

READ THIS FIRST
---------------

This program was written in literate Haskell. As a result, this document
contains _the entire source code of the project_. **You do not need to read the
entire document**. This section ("Design Document") is the two-page design
document required for the assigment; all subsequent sections are the literate
program itself.

Program Structure
-----------------

All of the subprograms that make up the smart-home network are contained in
a single executable. Command-line arguments determine which subprogram is run.

RPCs are performed via message passing over TCP sockets. Messages are sent as
strings, which are marshalled and unmarshalled using Haskell's built-in `show`
and `read` functions. One _gateway_ relays messages between multiple _devices_
and _controllers_. There are three kinds of messages:

* _Requests_, which are sent to either the gateway or a specific device.
* _Responses_, which are returned to the original sender of a Request.
* _Broadcasts_, which are sent by the gateway to all connected controllers.

The two tasks described in the assignment (_Preventing water pipe bursts_ and
_Preparing for spring break_) were implemented as controllers; these tasks will
only function if their controllers are currently running and connected to the
gateway.

All devices and most controllers have a command-line interface that allows
a user to change/view their state. Also, the user controller (`control user`) is
a general-purpose CLI that can send anything to the gateway, for testing
purposes.

Compiling
---------

Provided that the [Haskell platform][hs] is installed, the project can be
compiled by running `make`. This should install all necessary `cabal` packages,
compile the Haskell program, and copy it to the current directory with the name
`smarthome`.

GHC >= 4.5 is required. If you recently installed the Haskell platform, you may
need to update your package database with `cabal update` before running `make`.

[hs]: https://www.haskell.org/platform/ 

Executing
---------

The executable requires at least 3 arguments: the _subprogram name_, the
_hostname_, and the _port_. For everything except the gateway, the hostname and
port are the gateway's hostname and port. The gateway ignores the hostname.

Here are some examples of valid ways to execute the program, demonstrating all
of the subprogram names:

```bash
    ./smarthome gateway 0.0.0.0 9100
    ./smarthome temp 127.0.0.1 9100
    ./smarthome motion 127.0.0.1 9100
    ./smarthome bulb 127.0.0.1 9100
    ./smarthome outlet 127.0.0.1 9100
    ./smarthome control heater 127.0.0.1 9100
    ./smarthome control light 127.0.0.1 9100
    ./smarthome control user 127.0.0.1 9100
```

`silent` can be added as an extra argument to all of these to limit console
output; this is used by the test scripts.

The test scripts `run-all.sh` and `run-test-case.sh` can be used to run the
program, but _only after it has been compiled with `make`_.

Testing
-------

I did not write Haskell unit tests for this project; network programs are
notoriously hard to unit test reliably, and the networking code was the only
part of the codebase that was likely to be error-prone. (Haskell's type system
is rigid enough that, for non-I/O code, if it compiles, it's probably correct.)

The scripts `run-all.sh` and `run-test-case.sh` are the closest thing to unit
tests that I've provided. These run all of the devices and controllers, and
test the basic functionality.

`run-all.sh` launches all of the subprograms at once, hooks up a bunch of named
pipes (in `./tmp`) to keep them running, and presents the user with the
`control user` CLI. However, it isn't very interesting at first, because the
sensors can't be controlled over the network. To see the controllers in action,
try sending messages to the named pipes in another console, then watch the
output on the user CLI. For example, to see the heater controller:

* On the user CLI, `QueryState (ID 3)` (outlet ID may be different)
    * Should return `RESPONSE: On`
* In another console, `echo "30" > ./tmp/temp-input`
* On the user CLI, `QueryState (ID 3)` again
    * Should return `RESPONSE: Off`

Or, to see the motion detector in the light controller:

* On the user CLI, `ChangeMode Away`
* In another console, `echo "on" > ./tmp/motion-input`
* Should see a `BROADCAST: TextMessage "..."` in the user CLI.

Discussion
----------

This took a lot longer than it should have.

I put a lot of work into this project, more than it probably required. Mostly,
I did it to get better at Haskell. Writing a program like this in Haskell was
not as error-proof as I would have expected; getting the network code to detect
shutdowns and not crash randomly took a _long_ time, even though the functional
parts of the code were as clean and simple as I expected.

What I gained in simplicity of concurrency and data structures, I lost in
complexity of I/O operations, networking, and build process. Haskell isn't
a net gain over other languages for this kind of application, but I don't think
it's a loss either---and, in the end, I designed a much more modular,
extensible application than the specification called for.

\newpage

