#!/bin/bash
pandoc -o doc.pdf src/DesignDocument.md src/Main.lhs src/Protocol.lhs src/Communication.lhs src/Devices.lhs src/Gateway.lhs src/Controllers.lhs --toc --highlight-style=haddock -N -V geometry:margin=1.25in
