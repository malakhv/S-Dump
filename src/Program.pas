{--------------------------------------------------------------------}
{                                                                    }
{                           S-Dump project                           }
{                                                                    }
{  Copyright (C) 1996-2023 Mikhail Malakhov <malakhv@gmail.com>      }
{                                                                    }
{  Licensed under the Apache License, Version 2.0 (the "License").   }
{  You may not use this file except in compliance with the License.  }
{  You may obtain a copy of the License at                           }
{                                                                    }
{     http://www.apache.org/licenses/LICENSE-2.0                     }
{                                                                    }
{  Unless required by applicable law or agreed to in writing,        }
{  software distributed under the License is distributed on an       }
{  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,      }
{  either express or implied.                                        }
{                                                                    }
{  See the License for the specific language governing permissions   }
{  and limitations under the License.                                }
{                                                                    }
{--------------------------------------------------------------------}

{--------------------------------------------------------------------}
{ Small program to see and save to external file a dump (like in hex }
{ editors) of any file. It can be useful to investigate file formats }
{ and reverse engineering.                                           }
{                                                                    }
{ Created: 03.11.2022                                                }
{ Author: Mikhail.Malakhov                                           }
{--------------------------------------------------------------------}

program sdump;

// Compiler options
{$MODE DELPHI}
{$APPTYPE CONSOLE}
{$H+}

uses
    SysUtils, Dump, ProgVer, ProgMsg, Mikhan.Util.AppArgs,
    Mikhan.Util.StrUtils;

const

    { The name of this program. }
    PROG_NAME = 'S-Dump';

    { The author of this program. }
    PROG_AUTHOR = 'Mikhail.Malakhov';

    { The common debug flag. }
    DEBUG = False;

{ Program command line arguments }
const

    { Program option: A file to store program output, short format. }
    OPT_OUT_SHORT = '-o';
    { Program option: A file to store program output, long format. }
    OPT_OUT_LONG = '--out';

    { Program option: A limit of bytes processing, short format. }
    OPT_LIMIT_SHORT = '-l';
    { Program option: A limit of bytes processing, long format. }
    OPT_LIMIT_LONG = '--limit';

    { Program option: An offset from file beginning to process,
        short format. }
    OPT_OFFSET_SHORT = '-s';
    { Program option: An offset from file beginning to process,
        long format. }
    OPT_OFFSET_LONG = '--offset';

    { Program option: Represents all data as a char array,
        short format. }
    OPT_CHAR_SHORT = '-c';
    { Program option: Represents all data as a char array,
        long format. }
    OPT_CHAR_LONG = '--char';

    { Program option: Additionally print data as a text, short format.
        This option excludes '-c'/'--char'. }
    OPT_TEXT_SHORT = '-t';

    { Program option: Additionally print data as a text, long format.
        This option excludes '-c'/'--char'. }
    OPT_TEXT_LONG = '--text';

{ Program commands }
const

    { Program command: any actions for testing. }
    CMD_TEST = 'test';

{ Program command line arguments }
 var
    OptLimit: Integer;      // See Limit program option
    OptOffset: Integer;     // See Offset program option
    OptFormat: TOutFormat;  // See Char program option
    OptVerbose: Boolean;    // See Verbose program option

{ Global Scope }
var
    AppArgs: TAppArgs;      // Program command line arguments
    InputFile: TFileName;   // Input file path

    I: Integer;
    Arg: TArgument;
    Data: Array of Byte;
    Tmp: String;
    WasRead: Integer;

{ Prints program version. }
procedure PrintVersion();
begin
    WriteLn(PROG_NAME);
    ProgVer.PrintVersion(DEBUG);
    WriteLn('Copyright: ', PROG_AUTHOR);
end;

//
// Program entry point
//
begin

    // Parse input arguments
    AppArgs := TAppArgs.Create();
    AppArgs.Parse();
    OptVerbose := AppArgs.HasVerbose() or DEBUG;
    if OptVerbose then AppArgs.Print();

    // Any actios for testing, if we have it, we'll ignore other
    if AppArgs.Has(CMD_TEST) then
    begin
        // Print program version
        WriteLn('Program version:');
        PrintVersion();
        // Print program command line args
        WriteLn('Program args:');
        AppArgs.Print();
        // TODO Any test stuff
        Exit;
    end;

    // Program Version
    if AppArgs.HasVersion() then
    begin
        PrintVersion(); Exit;
    end;

    // Program Help
    if AppArgs.HasHelp() then
    begin
        PrintHelp(PROG_NAME); Exit;
    end;

    // Program argument: Limit
    if AppArgs.Has(OPT_LIMIT_SHORT, OPT_LIMIT_LONG) then
    begin
        Tmp := AppArgs.GetValue(OPT_LIMIT_SHORT, OPT_LIMIT_LONG);
        if not Mikhan.Util.StrUtils.IsEmpty(Tmp) then
            OptLimit := StrToInt(Tmp)
        else
            OptLimit := 0;
    end;

    // Program argument: Offset
    if AppArgs.Has(OPT_OFFSET_SHORT, OPT_OFFSET_LONG) then
    begin
        Tmp := AppArgs.GetValue(OPT_OFFSET_SHORT, OPT_OFFSET_LONG);
        if not Mikhan.Util.StrUtils.IsEmpty(Tmp) then
            OptOffset := StrToInt(Tmp)
        else
            OptOffset := 0;
    end;

    // Program argument: Char (outpit format, Hex (by default) or Char)
    if AppArgs.Has(OPT_CHAR_SHORT, OPT_CHAR_LONG) then
        OptFormat := ofChar
    else
        OptFormat := ofHex;

    // Program argument: Text (additionally print data as a text)
    if AppArgs.Has(OPT_TEXT_SHORT, OPT_TEXT_LONG) then
        OptFormat := ofHexAndText;

    // Program argument: Input File
    // (first command line argument without value)
    InputFile := '';
    for I := 0 to AppArgs.Count - 1 do
    begin
        Arg := AppArgs[I];
        if not Arg.IsArgument() then Continue;
        InputFile := TFileName(Arg.Key);
        break;
    end;
    if OptVerbose then WriteLn('Input: ', InputFile);
    if Mikhan.Util.StrUtils.IsEmpty(InputFile) then
    begin
        WriteLn(MSG_NO_INPUT); Exit;
    end;

    // The main program action: read and print data
    SetLength(Data, MAX_BYTES);
    WasRead := LoadData(InputFile, OptOffset, OptLimit, Data);
    if WasRead > 0 then
    begin
        SetLength(Data, WasRead);
        WriteLn();
        Dump.Dump(Data, OptOffset, 0, OptFormat);
    end;

end.
