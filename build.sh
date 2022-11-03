#!/bin/sh

rm -R -f ./build
mkdir -p build
fpc ./src/App.pas -FEbuild -Fu./src/app -Fu./src/util -osdump
