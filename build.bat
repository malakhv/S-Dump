::
:: Build app
::
:: Author: Mikhail.Malakhov
::

rmdir /Q /S build
mkdir build

SET PASCAL_KIT="./../PascalKit/src/util"

fpc.bat ./src/Program.pas -FEbuild ^
    -Fu./src/app ^
    -Fu./src/util ^
    -Fu%PASCAL_KIT% ^
    -osacd.exe
