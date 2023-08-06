# S-Dump

S-Dump (Small Dump) is a small program to see and save to external file a dump (like in hex editors) of any file. It useful to investigate file formats and reverse engineering.

## Program options

Follow you could see main program command line argument and options, to see all available features, please use `-h` option.

* `FILE_NAME` - An input file to process.
* `-s (--offset) VALUE` - An offset from file beginning.
* `-l (--limit) VALUE` - A limit of bytes processing.
* `-v (--version)` - The program version.
* `-h (--help)` - Display this information.
* `--verbose` - Turning on all debug messages.

You could see output example below (command `sdump test.txt -s 27 -l 100`):

```
          00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
---------------------------------------------------------
00000010|                                  76 65 20 74 6F
00000020| 20 65 78 74 65 72 6E 61 6C 20 66 69 6C 65 20 61
00000030| 20 64 75 6D 70 20 28 6C 69 6B 65 20 69 6E 0D 0A
00000040| 68 65 78 20 65 64 69 74 6F 72 73 29 20 6F 66 20
00000050| 61 6E 79 20 66 69 6C 65 2E 20 49 74 20 75 73 65
00000060| 66 75 6C 20 74 6F 20 69 6E 76 65 73 74 69 67 61
00000070| 74 65 20 66 69 6C 65 20 66 6F 72 6D 61 74 73
```
