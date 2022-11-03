# S-Dump

Small program to see and save to external file a dump (like in hex editors) of any file. It useful to investigate file formats and reverse engineering.

## Programm options

Follow you could see main program command line argument and options, to see all available features, please use `-h` option.

* `-h` or `--help` - Prints program help.
* `FILE_NAME` - A file to process.
* `-o FILE_NAME` or `--out FILE_NAME` - A file to store result.
* `-s OFFSET` - An _offset_ or _start of_ from file beginning to process from it.
* `-c COUNT` - A number of bytes to process.
* `-s` or `--string` - Represents all data as strings.
