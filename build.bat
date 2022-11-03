::
:: Build app
::
:: Author: Mikhail.Malakhov
::

del /q build\

fpc.bat ./src/App.pas -FEbuild -Fu./src/app -Fu./src/util -osdump.exe
