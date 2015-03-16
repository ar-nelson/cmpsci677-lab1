all: build

build: configure
	cabal build
	cp dist/build/cmpsci677-lab1/cmpsci677-lab1 ./smarthome

configure: deps
	cabal configure

deps: You_Need_To_Install_The_Haskell_Platform
	cabal install containers network stm random timers suspend safe

clean: You_Need_To_Install_The_Haskell_Platform
	cabal clean

You_Need_To_Install_The_Haskell_Platform:
	which cabal
