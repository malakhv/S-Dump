{--------------------------------------------------------------------}
{                                                                    }
{                   Pascal Utils Library (PUL)                       }
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

unit Dump;

{$mode delphi}
{$h+}

interface

uses SysUtils;

const

    { The maximum bytes to processing. }
    MAX_BYTES = 4096;

type

    { The formats of output, hex or char. }
    TOutFormat = (ofHex, ofChar, ofHexAndText);

{ Prints raw data. }
procedure Dump(const Source: Array of Byte; Offset: Integer; Limit: Integer); overload;

{ Prints raw data. }
procedure Dump(const Source: Array of Byte; Offset: Integer; Limit: Integer;
    Format: TOutFormat); overload;

{ Load raw data from file to Buf array. }
function LoadData(const AFile: TFileName; Offset: Integer; Limit: Integer;
    var Buf: array of byte): Integer;

implementation

uses ProgMsg, Mikhan.Util.StrUtils;

procedure Dump(const Source: Array of Byte; Offset: Integer;
    Limit: Integer);
begin
    Dump(Source, OffSet, Limit, ofHex);
end;

procedure Dump(const Source: Array of Byte; Offset: Integer;
    Limit: Integer; Format: TOutFormat);
const COL_LIMIT = $F;

var 
    col, off, skip: Integer;

    procedure Header();
    var i: Integer;
    begin
        Write(EMPTY:10);
        for i := 0 to COL_LIMIT do
            Write(IntToHex(i, 2), CHAR_SPACE);
        WriteLn();
        WriteLn(RepeatString('-', 57));
    end;

    procedure NewRow();
    begin
        WriteLn();
        Write(IntToHex(off, 8), CHAR_VERT_SLASH, CHAR_SPACE);
    end;

    function MakeSymbol(Value: Integer): String;
    begin
        // Hex
        if Format = ofHex then
        begin
            Result := IntToHex(Value, 2); Exit;
        end;
        // Char
        if Value >= 20 then
            Result := Char(Value) + Char($0)
        else
            Result := '  ';
    end;

var
    val: Byte;
    i: Integer;

begin
    Header();

    // Calc initial parameters
    col := COL_LIMIT;
    off := (Offset div 16) + (COL_LIMIT * (Offset div 16));
    skip := (Offset mod 16);
    if Limit <= 0 then Limit := MaxInt;

    // Start printing
    NewRow();
    for i := Low(Source) to High(Source) do
    begin

        // Should we go to a new row?
        if col < 0 then
        begin
            off := off + COL_LIMIT + 1;
            NewRow();
            col := COL_LIMIT;
        end;
        Dec(col);

        // Need to skip?
        while skip > 0 do
        begin
            Write(EMPTY:3);
            Dec(skip); Dec(col);
        end;

        // Print value
        val := Source[i];
        Write(MakeSymbol(val), CHAR_SPACE);
        Dec(Limit);

        // Should stop?
        if Limit <= 0 then break;
    end;
    Writeln();
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

    // Check offset and limit
    if Offset <= 0 then Offset := 0;
    if (Limit <= 0) or (Limit > MAX_BYTES) then Limit := MAX_BYTES;
    size := Length(Buf);
    if size > Limit then size := Limit;

    // Read data
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
