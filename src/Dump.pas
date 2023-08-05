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
const ADDRESS_SPACE = $F;

    procedure Header();
    var i: Integer;
    begin
        Write(EMPTY:10);
        for i := 0 to ADDRESS_SPACE do
            Write(IntToHex(i, 2), CHAR_SPACE);
        WriteLn();
        WriteLn(RepeatString('-', 57));
    end;

    procedure NewRow(Address: Integer);
    begin
        WriteLn();
        Write(IntToHex(Address, 8), CHAR_VERT_SLASH, CHAR_SPACE);
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
    COL, OFF, SKIP: Integer;

var
    i: Integer; val: Byte;

begin
    Header();

    // Calc initial parameters
    COL := ADDRESS_SPACE;
    OFF := (Offset div 16) + (ADDRESS_SPACE * (Offset div 16));
    SKIP := (Offset mod 16);
    if Limit <= 0 then Limit := MaxInt;

    // Start printing
    NewRow(OFF);
    for i := Low(Source) to High(Source) do
    begin

        // Should we go to a new row?
        if COL < 0 then
        begin
            OFF := OFF + ADDRESS_SPACE + 1;
            NewRow(OFF);
            COL := ADDRESS_SPACE;
        end;
        Dec(COL);

        // Need to skip?
        while SKIP > 0 do
        begin
            Write(EMPTY:3);
            Dec(SKIP);
            Dec(COL);
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
