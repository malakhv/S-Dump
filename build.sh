#!/bin/sh

rm -R -f ./build
mkdir -p build
fpc ./src/Program.pas -FEbuild -Fu./src/app -Fu./src/util -osdump
