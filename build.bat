::
:: Build app
::
:: Author: Mikhail.Malakhov
::

del /q build\

fpc.bat ./src/Program.pas -FEbuild -Fu./src/app -Fu./src/util -osdump.exe
