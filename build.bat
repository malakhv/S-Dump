::
:: Build app
::
:: Author: Mikhail.Malakhov
::

rmdir /Q /S build
mkdir build

fpc ./src/Program.pas -FEbuild -Fu./src/app -Fu./src/util -osdump.exe
