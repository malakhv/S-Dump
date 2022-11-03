# S-Dump

S-Dump (Small Dump) is a small program to see and save to external file a dump (like in hex editors) of any file. It useful to investigate file formats and reverse engineering.

## Programm options

Follow you could see main program command line argument and options, to see all available features, please use `-h` option.

* `-h` or `--help` - Prints program help.
* `FILE_NAME` - A file to process.
* `-o FILE_NAME` or `--out FILE_NAME` - A file to store result.
* `-s VALUE` or `--offset VALUE` -  An offset from file beginning to process.
* `-l VALUE` or `--limit` -  A limit of bytes processing.
* `--string` - Represents all data as strings.
