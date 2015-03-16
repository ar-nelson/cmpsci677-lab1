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
compiled by running `cabal build`. If it complains about missing packages, try
running `cabal update` first, and/or installing those packages manually with
`cabal install`.

This will produce the executable file
`dist/build/cmpsci677-lab1/cmpsci677-lab1`.

[hs]: https://www.haskell.org/platform/

Executing
---------

The executable requires at least 3 arguments: the _subprogram name_, the
_hostname_, and the _port_. For everything except the gateway, the hostname and
port are the gateway's hostname and port. The gateway ignores the hostname.

Here are some examples of valid ways to execute the program, demonstrating all
of the subprogram names:

```bash
    ./dist/build/cmpsci677-lab1/cmpsci677-lab1 gateway 0.0.0.0 9100
    ./dist/build/cmpsci677-lab1/cmpsci677-lab1 temp 127.0.0.1 9100
    ./dist/build/cmpsci677-lab1/cmpsci677-lab1 motion 127.0.0.1 9100
    ./dist/build/cmpsci677-lab1/cmpsci677-lab1 bulb 127.0.0.1 9100
    ./dist/build/cmpsci677-lab1/cmpsci677-lab1 outlet 127.0.0.1 9100
    ./dist/build/cmpsci677-lab1/cmpsci677-lab1 control heater 127.0.0.1 9100
    ./dist/build/cmpsci677-lab1/cmpsci677-lab1 control light 127.0.0.1 9100
    ./dist/build/cmpsci677-lab1/cmpsci677-lab1 control user 127.0.0.1 9100
```

`silent` can be added as an extra argument to all of these to limit console
output; this is used by the test scripts.

The test scripts `run-all.sh` and `run-test-case.sh` can be used to run the
program, but _only after it has been compiled with `cabal build`_.

Tradeoffs and Known Bugs
------------------------

Writing the program in Haskell allowed me to reason much more easily about
thread safety; the gateway's global state was stored using software
transactional memory, and spawning new threads was simple. I didn't bother to
come up with a compact binary encoding for messages; Haskell's built-in string
serialization was good enough for my purposes.

\newpage

