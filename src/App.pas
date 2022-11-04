{-----------------------------------------------------------------}
{                                                                 }
{                       S-Dump project                            }
{                                                                 }
{  Copyright (C) 2022 Mikhail Malakhov <malakhv@gmail.com>        }
{                                                                 }
{  Unauthorized copying of this file, via any medium is           }
{  strictly prohibited.                                           }
{                                                                 }
{       Confidential and Proprietary. All Rights Reserved.        }
{                                                                 }
{-----------------------------------------------------------------}

{-----------------------------------------------------------------}
{ Small program to see and save to external file a dump (like in  }
{ hex editors) of any file. It useful to investigate file formats }
{ and reverse engineering.                                        }
{                                                                 }
{ Created: 03.11.2022                                             }
{ Author: Mikhail.Malakhov                                        }
{-----------------------------------------------------------------}

program sdump;

// Compiler options
{$mode delphi}
{$h+}

uses
    SysUtils, Dump, Mikhan.Util.AppArgs, Mikhan.Util.AppLogs, Mikhan.Util.AppVersion,
    Mikhan.Util.StrUtils;

const

    { The name of this program. }
    APP_NAME = 'S-Dump';

    { The author of this program. }
    APP_AUTHOR = 'Mikhail.Malakhov';

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

    { Program option: An offset from file beginning to process, short format. }
    OPT_OFFSET_SHORT = '-s';
    { Program option: An offset from file beginning to process, long format. }
    OPT_OFFSET_LONG = '--offset';

    { Program option: Represents all data as a char array, short format. }
    OPT_CHAR_SHORT = '-c';
    { Program option: Represents all data as a char array, long format. }
    OPT_CHAR_LONG = '--char';

{ Program commands }
const

    { Program command: any actions for testing. }
    CMD_TEST = 'test';

{ Global Scope }
var
    AppVer: TSemVer;        // Program version
    AppArgs: TAppArgs;      // Program command line arguments

    InputFile: TFileName;   // Input file path
    OutputFile: TFileName;  // Outpot file path

    I: Integer;
    Arg: TArgument;
    Data: Array of Byte;
    Tmp: String;
    WasRead: Integer;

    OptLimit: Integer;      // See Limit program option
    OptOffset: Integer;     // See Offset program option
    OptFormat: TOutFormat;   // See Char program option


const
    DEF_INDENT = '  ';

{ Prints program version. }
procedure PrintVersion();
begin
    AppVer := TSemVer.Create(DEBUG);
    AppVer.LoadFromFile();
    WriteLn(APP_NAME);
    WriteLn(AppVer.ToString());
    WriteLn('Copyright: ', APP_AUTHOR);
end;

{ Prints program help. }
procedure PrintHelp();
begin
    WriteLn(APP_NAME, ' command line options:');
    WriteLn(DEF_INDENT, ' -h or --help                  - Display this information.');
    WriteLn(DEF_INDENT, ' FILE_NAME                     - An input file to process.');
    WriteLn(DEF_INDENT, ' -s VALUE or --offset VALUE    - An offset from file beginning.');
    WriteLn(DEF_INDENT, ' -l VALUE or --limit           - A limit of bytes processing.');
    WriteLn(DEF_INDENT, ' -c or --char                  - Represents all data as a char array.');
    WriteLn(DEF_INDENT, ' -v or --version               - The program version.');
    WriteLn();
end;

function LoadData(const AFile: TFileName; var Buf: array of byte;
    Offset: Integer; Limit: Integer): Integer;
var size, fsize: Integer;
    f: File;
begin
    Result := 0;
    // Check offset
    if Offset <= 0 then Offset := 0;
    // Check limit. Right now 1024 bytes max
    if (Limit <= 0) or (Limit > 1024) then Limit := 1024;
    size := Length(Buf);
    if size > Limit then size := Limit;

    // Open inpurt file read and setting up size of read chunk
    // to 1 byte
    AssignFile(f, AFile);
    Reset(f, 1);
    fsize := FileSize(f);
    if size > fsize then size := fsize;
    WriteLn('Size: ', size);
    WriteLn('FSize: ', fsize);
    try
        Seek(f, Offset);
        BlockRead(f, Buf, size, Result);
    finally
        CloseFile(f);
    end;
end;

//
// Program entry point
//
begin

    // Parse input arguments
    AppArgs := TAppArgs.Create();
    AppArgs.ParseArgs();
    if DEBUG then AppArgs.PrintAll();

    // Any actios for testing, if we have it, we'll ignore other
    if AppArgs.Has(CMD_TEST) then
    begin
        // Print program version
        WriteLn('Program version:');
        PrintVersion();
        // Print program command line args
        WriteLn('Program args:');
        AppArgs.PrintAll();
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
        PrintHelp(); Exit;
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

    // Program argument: input file
    // (first command line argument without value)
    InputFile := '';
    for I := 0 to AppArgs.Count - 1 do
    begin
        Arg := AppArgs[I];
        if not Arg.IsArgument() then Continue;
        InputFile := TFileName(Arg.Key);
    end;
    if DEBUG then WriteLn('Input: ', InputFile);

    // Read and print data
    SetLength(Data, 1024);
    WasRead := LoadData(InputFile, Data, OptOffset, OptLimit);
    SetLength(Data, WasRead);
    WriteLn();
    Dump.Dump(Data, OptOffset, 0, OptFormat);

end.
