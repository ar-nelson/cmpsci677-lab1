all: build

build: configure
	cabal build
	cp dist/build/cmpsci677-lab1/cmpsci677-lab1 ./smarthome

configure: deps
	cabal configure

deps: You_Need_To_Install_The_Haskell_Platform
	cabal install containers network stm random timers suspend safe time

doc: You_Need_To_Install_Pandoc
	pandoc -o DesignDocument.pdf src/DesignDocument.md src/Main.lhs src/Protocol.lhs src/Communication.lhs src/Devices.lhs src/Gateway.lhs src/Controllers.lhs --toc --highlight-style=haddock -N -V geometry:margin=1.25in

clean: You_Need_To_Install_The_Haskell_Platform
	cabal clean

You_Need_To_Install_Pandoc:
	which pandoc

You_Need_To_Install_The_Haskell_Platform:
	which cabal
