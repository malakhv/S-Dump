{-----------------------------------------------------------------}
{                                                                 }
{                       S-Dump project                            }
{                                                                 }
{  Copyright (C) 2022 Mikhail Malakhov <malakhv@gmail.com>        }
{                                                                 }
{  Licensed under the Apache License, Version 2.0 (the "License") }
{  You may not use this file except in compliance with the        }
{  License. You may obtain a copy of the License at               }
{                                                                 }
{     http://www.apache.org/licenses/LICENSE-2.0                  }
{                                                                 }
{  Unless required by applicable law or agreed to in writing,     }
{  software distributed under the License is distributed on an    }
{  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,   }
{  either express or implied.                                     }
{                                                                 }
{  See the License for the specific language governing            }
{  permissions and limitations under the License.                 }
{                                                                 }
{-----------------------------------------------------------------}

unit Dump;

{$mode delphi}
{$h+}

interface

uses SysUtils;

const

    { The maximum bytes to processing. }
    MAX_BYTES = 2048;

type

    { The formats of output, hex or char. }
    TOutFormat = (ofHex, ofChar);

{ Prints raw data. }
procedure Dump(Source: Array of Byte; Offset: Integer; Limit: Integer); overload;

{ Prints raw data. }
procedure Dump(Source: Array of Byte; Offset: Integer; Limit: Integer;
    Format: TOutFormat); overload;

{ Load raw data from file to Buf array. }
function LoadData(const AFile: TFileName; Offset: Integer; Limit: Integer;
    var Buf: array of byte): Integer;

implementation

uses AppMsg;

{ Some stuff to print data }
const 
    COL_LIMIT = $F;
    COL_OFFSET = '          ';
    COL_SEP = '| ';
    HEADER_SEP = '---------------------------------------------------------';

{ Prints header with bytes offset. }
procedure PrintHeader();
var i: Integer;
begin
    Write(COL_OFFSET);
    for i := 0 to COL_LIMIT do
    begin
        Write(IntToHex(i, 2));
        Write(' ');
    end;
    WriteLn();
    WriteLn(HEADER_SEP);
end;

procedure PrintChar(Value: Integer);
begin
    if Value >= 20 then
        Write(Char(Value), Char($0))
    else
        Write(IntToHex(Value, 2));
end;

procedure Dump(Source: Array of Byte; Offset: Integer; Limit: Integer);
begin
    Dump(Source, OffSet, Limit, ofHex);
end;

procedure Dump(Source: Array of Byte; Offset: Integer; Limit: Integer; Format: TOutFormat);
var 
    i, col, off, skip: Integer;
    val: Byte;

    procedure NewRow();
    begin
        Writeln('');
        off := off + COL_LIMIT + 1;
        Write(IntToHex(off, 8), COL_SEP);
        col := COL_LIMIT;
    end;

begin
    PrintHeader();

    col := COL_LIMIT;
    off := (Offset div 16) + (COL_LIMIT * (Offset div 16));
    skip := (Offset mod 16);
    if Limit <= 0 then Limit := MaxInt;
    Write(IntToHex(off, 8), COL_SEP);
    for i := Low(Source) to High(Source) do
    begin

        if col < 0 then NewRow();
        Dec(col);

        while skip > 0 do
        begin
            Write('   ');
            Dec(skip);
            Dec(col);
        end;

        val := Source[i];
        if (Format = ofHex) then
            Write(IntToHex(val, 2))
        else
            PrintChar(val);
        Write(' ');
        Dec(Limit);
        if Limit <= 0 then break;
    end;
    Writeln('');

end;

function LoadData(const AFile: TFileName; Offset: Integer; Limit: Integer;
    var Buf: array of byte): Integer;
var size, fsize: Integer;
    f: File;
begin
    Result := 0;
    // Check file
    if not FileExists(AFile) then
    begin
        WriteLn(MSG_INPUT_NOT_FOUND); Exit;
    end;
    // Check offset
    if Offset <= 0 then Offset := 0;
    // Check limit. Right now 1024 bytes max
    if (Limit <= 0) or (Limit > MAX_BYTES) then Limit := MAX_BYTES;
    size := Length(Buf);
    if size > Limit then size := Limit;

    // Open inpurt file read and setting up size of read chunk
    // to 1 byte
    AssignFile(f, AFile);
    Reset(f, 1);
    fsize := FileSize(f);
    if size > fsize then size := fsize;
    try
        Seek(f, Offset);
        BlockRead(f, Buf, size, Result);
    finally
        CloseFile(f);
    end;
end;

end.
