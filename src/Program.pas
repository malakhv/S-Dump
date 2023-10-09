{-------------------------------------------------------------------------}
{                                                                         }
{                             S-Dump project                              }
{                                                                         }
{  Copyright (C) 1996-2023 Mikhail Malakhov <malakhv@gmail.com>           }
{                                                                         }
{  Licensed under the Apache License, Version 2.0 (the "License").        }
{  You may not use this file except in compliance with the License.       }
{  You may obtain a copy of the License at                                }
{                                                                         }
{     http://www.apache.org/licenses/LICENSE-2.0                          }
{                                                                         }
{  Unless required by applicable law or agreed to in writing, software    }
{  distributed under the License is distributed on an "AS IS" BASIS,      }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or        }
{  implied.                                                               }
{                                                                         }
{  See the License for the specific language governing permissions and    }
{  limitations under the License.                                         }
{                                                                         }
{-------------------------------------------------------------------------}

{-------------------------------------------------------------------------}
{ Small program to see and save to external file a dump (like in hex      }
{ editors) of any file. It can be useful to investigate file formats and  }
{ reverse engineering.                                                    }
{                                                                         }
{ Created: 03.11.2022                                                     }
{ Author: Mikhail.Malakhov                                                }
{-------------------------------------------------------------------------}

program sdump;

// Compiler options
{$MODE DELPHI}
{$APPTYPE CONSOLE}
{$H+}
{$T+}

uses
    SysUtils, Classes, Pipes, ProgVer, ProgMsg, Mikhan.Util.AppArgs,
    Mikhan.Util.StrUtils, Mikhan.Util.Dump;

const

    { The name of this program. }
    PROG_NAME = 'S-Dump';

    { The author of this program. }
    PROG_AUTHOR = 'Mikhail.Malakhov';

    { The copyright string. }
    PROG_COPYRIGHT = 'Copyright (C) 1996-2023 Mikhail Malakhov ' +
        '<malakhv@gmail.com>';

    { The common debug flag. }
    DEBUG = False;

const

    { The maximum bytes to processing. }
    MAX_BYTES = 4096;

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
    OPT_OFFSET_LONG = '--skip';

    { Program option: Represents all data as a char array,
      short format. }
    OPT_CHAR_SHORT = '-c';
    { Program option: Represents all data as a char array,
      long format. }
    OPT_CHAR_LONG = '--char';

    { Program option: Use specified text as a program data source, short
      format. This option has a higher priority than file name or pipe
      data. }
    OPT_TEXT_SHORT = '-t';
    { Program option: Use specified text as a program data source, long
      format. This option has a higher priority than file name or pipe
      data. }
    OPT_TEXT_LONG = '--text';

{ Program commands }
const

    { Program command: any actions for testing. }
    CMD_TEST = 'test';

{ Program command line arguments }
 var
    AppArgs: TAppArgs;          // Program command line arguments/options
    OptLimit: Integer;          // See Limit program option
    OptOffset: Integer;         // See Offset program option
    OptFormat: TDumpOutFormat;  // See Char program option
    OptVerbose: Boolean;        // See Verbose program option
    OptInputFile: TArgument;    // See FILE_NAME program option
    HasPipe: Boolean;           // True, if we have a pipe data

{ Global Scope }
var
    I: Integer;
    Data: Array of Byte;
    Tmp: String;
    WasRead: Integer;
    InPipe: TInputPipeStream;
    InStream: TStream;

{
    Loads raw data from stream.
}
function LoadData(const Stream: TStream; Offset: Integer; Limit: Integer;
    var Buf: array of byte): Integer;
var Size, StreamSize: Integer;
begin
    Result := 0;

    // Retrieve stream size, for a pipe data we have a special method
    if Stream is TInputPipeStream then
        StreamSize := (Stream as TInputPipeStream).NumBytesAvailable
    else
        StreamSize := Stream.Size;
    if StreamSize <= 0 then Exit;

    // Check offset and limit
    if Offset <= 0 then Offset := 0;
    if (Limit <= 0) or (Limit > MAX_BYTES) then Limit := MAX_BYTES;

    // How many bytes we want to read?
    Size := Length(Buf);
    if Size > Limit then Size := Limit;
    if Size > StreamSize then Size := StreamSize;

    // Need to print logs?
    if OptVerbose then
    begin
        Write('LoadData {');
        Write('Offset=', Offset,', ');
        Write('Limit=', Limit,', ');
        Write('Size=', Size,', ');
        WriteLn('StreamSize=', StreamSize,'}');
    end;

    // Read data
    try
        if Offset > 0 then Stream.Seek(Offset, soBeginning);
        Result := Stream.Read(Buf, Size);
    except
        WriteLn(MSG_CANNOT_READ_DATA);
        Result := 0;
    end;
end;

{ Prints program version. }
procedure PrintVersion();
begin
    WriteLn(PROG_NAME);
    WriteLn(ProgVer.GetVersion(DEBUG));
    WriteLn(PROG_COPYRIGHT);
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

    // Program Help
    if AppArgs.HasHelp() then
    begin
        PrintHelp(PROG_NAME); Exit;
    end;

    // Program Version
    if AppArgs.HasVersion() then
    begin
        PrintVersion(); Exit;
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
        OptFormat := dfChar
    else
        OptFormat := dfHex; // By default

    //
    // Select data source and print dump, the priority for data source is:
    //  1. Program argument
    //  2. Pipe
    //  3. Input file
    //
    SetLength(Data, MAX_BYTES);

    // Data source: Program argument
    if AppArgs.Has(OPT_TEXT_SHORT, OPT_TEXT_LONG) then
    begin
        if OptVerbose then WriteLn('Data source: Program argument');
        Tmp := AppArgs.GetValue(OPT_TEXT_SHORT, OPT_TEXT_LONG);
        InStream := TStringStream.CreateRaw(Tmp);
        WasRead := LoadData(InStream, OptOffset, OptLimit, Data);
        SetLength(Data, WasRead);
        Mikhan.Util.Dump.Dump(Data, OptOffset, 0, OptFormat);
        Exit;
    end;

    // Data source: Pipe
    InPipe := TInputPipeStream.Create(StdInputHandle);
    HasPipe := InPipe.NumBytesAvailable > 0;
    if HasPipe then
    begin
        if OptVerbose then WriteLn('Data source: Pipe');
        WasRead := LoadData(InPipe, OptOffset, OptLimit, Data);
        SetLength(Data, WasRead);
        Mikhan.Util.Dump.Dump(Data, OptOffset, OptLimit, OptFormat);
        Exit;
    end;

    // Data source: Input File (first command line argument without value)
    OptInputFile.Key := '';
    for I := 0 to AppArgs.Count - 1 do
    begin
        if AppArgs[I].IsArgument() then
        begin
            OptInputFile.Key := AppArgs[I].Key;
            if OptVerbose then WriteLn('Input: ', OptInputFile.Key);
            break;
        end;
    end;
    if Mikhan.Util.StrUtils.IsEmpty(OptInputFile.Key) then
    begin
        WriteLn(MSG_NO_INPUT); Exit;
    end;
    if OptVerbose then WriteLn('Data source: ', OptInputFile.Key);
    InStream := TFileStream.Create(OptInputFile.Key, fmOpenRead);
    WasRead := LoadData(InStream, OptOffset, OptLimit, Data);
    if WasRead > 0 then
    begin
        SetLength(Data, WasRead);
        WriteLn();
        Mikhan.Util.Dump.Dump(Data, OptOffset, 0, OptFormat);
    end;

end.

{-------------------------------------------------------------------------}
