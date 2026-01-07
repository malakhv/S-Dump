::
:: Build app
::
:: Author: Mikhail.Malakhov
::

@ECHO OFF

SET PASCAL_KIT="./../PascalKit/src/util"

IF EXIST build\ (
    echo Clear build directory...
    rd /s /q build\
) ELSE (
    echo The build directory not found...
)
mkdir build

fpc ./src/Program.pas -FEbuild ^
    -Fu./src/app ^
    -Fu./src/util ^
    -Fu%PASCAL_KIT% ^
    -osdump.exe
