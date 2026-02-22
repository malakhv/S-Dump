{------------------------------------------------------------------------------}
{                                                                              }
{                                S-Dump Project                                }
{                                                                              }
{  Copyright (C) 1996-2026 Mikhail Malakhov, http://mikhan.me/                 }
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License"). You may     }
{  not use this file except in compliance with the License. You may obtain     }
{  a copy of the License at                                                    }
{                                                                              }
{     http://www.apache.org/licenses/LICENSE-2.0                               }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT   }
{  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.            }
{                                                                              }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{ A small program for viewing and saving dump (like in hex editors) of any     }
{ files. It can be useful to investigate file formats and reverse engineering. }
{                                                                              }
{ Project: S-Dump                                                              }
{ Package: None                                                                }
{                                                                              }
{ Dependencies: ProgVer, ProgMsg, Mikhan.Util.*                                }
{ Operating Systems: Win, Linux, MacOS                                         }
{                                                                              }
{ Created: 03.11.2022                                                          }
{ Authors: Mikhail.Malakhov                                                    }
{------------------------------------------------------------------------------}

PROGRAM sdump;                                                       { PROGRAM }

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
    PROG_COPYRIGHT = 'Copyright (C) 1996-2025 Mikhail Malakhov ' +
        '<malakhv@gmail.com>';

    { The common debug flag. }
    DEBUG = False;

const

    { The maximum bytes to processing. }
    MAX_BYTES = 4096;

{ Program command line arguments }
const

    { Program option: A limit of bytes processing, short format. }
    OPT_LIMIT_SHORT = '-l';
    { Program option: A limit of bytes processing, long format. }
    OPT_LIMIT_LONG = '--limit';

    { Program option: An offset from file beginning to process, short format. }
    OPT_OFFSET_SHORT = '-s';
    { Program option: An offset from file beginning to process, long format. }
    OPT_OFFSET_LONG = '--skip';

    { Program option: Represents all data as a char array, short format. }
    OPT_CHAR_SHORT = '-c';
    { Program option: Represents all data as a char array, long format. }
    OPT_CHAR_LONG = '--char';

    { Program option: Use specified text as a program data source, short
      format. This option has a higher priority than file name or pipe
      data. }
    OPT_TEXT_SHORT = '-t';
    { Program option: Use specified text as a program data source, long
      format. This option has a higher priority than file name or pipe
      data. }
    OPT_TEXT_LONG = '--text';

    { Program option: Search something into data, short format. }
    OPT_FIND_SHORT = '-f';
    { Program option: Search something into data, long format. }
    OPT_FIND_LONG = '--find';

{ Program commands }
const

    { Program command: any actions for testing. }
    CMD_TEST = 'test';

{------------------------------------------------------------------------------}

{ Program command line arguments }
 var
    AppArgs: TAppArgs;          // Program command line arguments/options
    OptLimit: Integer;          // See Limit program option
    OptOffset: Integer;         // See Offset program option
    OptFind: Integer;           // See Find program option
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

{------------------------------------------------------------------------------}

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

function FindData(const Stream: TStream; Offset: Integer;
    Val: Integer): Integer;
var Size, StreamSize: Integer;
begin
    Result := -1; // Nothing was found

    // Retrieve stream size, for a pipe data we have a special method
    if Stream is TInputPipeStream then
        StreamSize := (Stream as TInputPipeStream).NumBytesAvailable
    else
        StreamSize := Stream.Size;
    if StreamSize <= 0 then Exit;

    // Check offset and limit
    if Offset <= 0 then Offset := 0;

    // Need to print logs?
    if OptVerbose then
    begin
        Write('FindData {');
        Write('Offset=', Offset,', ');
        WriteLn('StreamSize=', StreamSize,'}');
    end;

    // Find value
    try
        if Offset > 0 then Stream.Seek(Offset, soBeginning);

        //FIND

        //Result := Stream.Read(Buf, Size);

    except
        WriteLn(MSG_CANNOT_READ_DATA); Result := -1;
    end;

end;

{
    Prints program version.
}
procedure PrintVersion();
begin
    WriteLn(PROG_NAME);
    WriteLn(ProgVer.GetVersion(DEBUG));
    WriteLn(PROG_COPYRIGHT);
end;

{------------------------------------------------------------------------------}

BEGIN                                                            { ENTRY POINT }

{------------------------------------------------------------------------------}

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

    // Program argument: Find
    if AppArgs.Has(OPT_FIND_SHORT, OPT_FIND_LONG) then
    begin
        Tmp := AppArgs.GetValue(OPT_FIND_SHORT, OPT_FIND_LONG);
        if not Mikhan.Util.StrUtils.IsEmpty(Tmp) then
            OptFind := StrToInt(Tmp)
        else
            OptFind := -1; // Search first not 0 value
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
        WriteLn();
        Mikhan.Util.Dump.Dump(Data, OptOffset, 0, OptFormat);
        Exit;
    end;

    // Data source: Pipe
    // TODO It works very strange... Need to investigate...
    InPipe := TInputPipeStream.Create(StdInputHandle);
    HasPipe := InPipe.NumBytesAvailable > 0;
    if HasPipe then
    begin
        if OptVerbose then WriteLn('Data source: Pipe');
        WasRead := LoadData(InPipe, OptOffset, OptLimit, Data);
        SetLength(Data, WasRead);
        WriteLn();
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

    if AppArgs.Has(OPT_FIND_SHORT, OPT_FIND_LONG) then
    begin
        OptOffset := FindData(InStream, OptOffset, OptFind);
        if OptOffset < 0 then Exit;
    end;

    WasRead := LoadData(InStream, OptOffset, OptLimit, Data);
    if WasRead > 0 then
    begin
        SetLength(Data, WasRead);
        WriteLn();
        Mikhan.Util.Dump.Dump(Data, OptOffset, 0, OptFormat);
    end;

{------------------------------------------------------------------------------}

END.                                                                     { END }

{------------------------------------------------------------------------------}
