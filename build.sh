#!/bin/sh

PASCAL_KIT="./../PascalKit/src/util"

# Clear build (out) dir
rm -R -f ./build
mkdir -p build

# Compile programm
fpc ./src/Program.pas \
     -Fu./src/app \
     -Fu./src/util \
     -Fu$PASCAL_KIT \
     -FEbuild \
     -osdump
