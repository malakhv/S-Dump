::
:: Build app
::
:: Author: Mikhail.Malakhov
::

del /q build\

fpc ./src/Program.pas -FEbuild -Fu./src/app -Fu./src/util -osdump.exe
