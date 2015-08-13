#!/bin/bash

PS4='($LINENO)+ '
set -x
set -e

cabal install --haddock-hyperlink-source --dependencies-only && cabal install
