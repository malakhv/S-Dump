{------------------------------------------------------------------------------}
{                                                                              }
{                          Pascal Utils Library (PUL)                          }
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
{ The Unit contains methods to print raw data in hexadecimal format (dump,     }
{ like in hex editors).                                                        }
{                                                                              }
{ Package: Mikhan.Util                                                         }
{ Types: TDumpOutFormat                                                        }
{ Dependencies: SysUtils, ProgPipes                                            }
{                                                                              }
{ Created: 14.08.2023                                                          }
{ Author: Mikhail.Malakhov                                                     }
{------------------------------------------------------------------------------}

unit Mikhan.Util.Dump;

{$MODE DELPHI}
{$H+}

INTERFACE                                                          { INTERFACE }
 
type

    { The formats of dump output, hex or char. }
    TDumpOutFormat = (dfHex, dfChar);

{
  Prints raw data in hexadecimal format.
}
procedure Dump(const Source: Array of Byte); overload;
    
{
  Prints raw data in hexadecimal format.
}
procedure Dump(const Source: Array of Byte; Limit: Integer); overload;
    
{
  Prints raw data in hexadecimal format.
}
procedure Dump(const Source: Array of Byte; Offset, Limit: Integer);
    overload;
    
{
  Prints raw data in hexadecimal or char format.
}
procedure Dump(const Source: Array of Byte; Offset, Limit: Integer;
    Format: TDumpOutFormat); overload;

{------------------------------------------------------------------------------}

IMPLEMENTATION                                                { IMPLEMENTATION }

{------------------------------------------------------------------------------}

uses SysUtils;

const
    BYTES_PER_ROW = 16;
    ADDRESS_SIZE = 8;
    CELL_SIZE = 3;
    PREFIX_SIZE = ADDRESS_SIZE + 2; // Address + '| '
    SEPARATOR_SIZE =
        PREFIX_SIZE + BYTES_PER_ROW * CELL_SIZE - 1;

{ Makes a symbol to output. }
function MakeSymbol(Value: Byte; Format: TDumpOutFormat): String;
const PLACEHOLDER = '..';
begin
    // Hex
    if Format = dfHex then
    begin
        Result := IntToHex(Value, 2); Exit;
    end;
    // Char
    if (Value >= $20) and (Value <= $7E) then
        Result := Char(Value) + ' ' //Char($20)
    else
        Result := PLACEHOLDER;
end;

procedure Dump(const Source: Array of Byte);
begin
    Dump(Source, 0, 0, dfHex);
end;

procedure Dump(const Source: Array of Byte; Limit: Integer);
begin
    Dump(Source, 0, Limit, dfHex);
end;

procedure Dump(const Source: Array of Byte; Offset, Limit: Integer);
begin
    Dump(Source, Offset, Limit, dfHex);
end;

procedure Dump(const Source: Array of Byte; Offset, Limit: Integer;
        Format: TDumpOutFormat);

    procedure Header();
    var i: Integer;
    begin
        Write(EMPTY:10);
        for i := 0 to BYTES_PER_ROW - 1 do
            Write(IntToHex(i, 2), CHAR_SPACE);
        WriteLn();
        Write(RepeatString('-', 57));
    end;

    procedure NewRow(Address: Integer);
    begin
        WriteLn();
        Write(IntToHex(Address, 8), CHAR_VERT_SLASH, CHAR_SPACE);
    end;

var
    COL, Count, RowAddress, StartColumn, I: Integer;

begin

    // Initial parameters
    if Offset < 0 then Offset := 0;
    RowAddress := (Offset div BYTES_PER_ROW) * BYTES_PER_ROW;
    StartColumn := Offset mod BYTES_PER_ROW;
    
    // How many data we should process?
    Count := Length(Source);
    if (Limit > 0) and (Limit < Count) then
        Count := Limit;

    // Start printing
    Header();
    NewRow(RowAddress);
    COL := StartColumn;

    // Empty columns before the first byte
    if Count > 0 then
        for I := 1 to StartColumn do
            Write(EMPTY:3);

    // Print data
    for I := 0 to Count - 1 do
    begin
        // Should we go to a new row?
        if COL >= BYTES_PER_ROW then
        begin
            RowAddress := RowAddress + BYTES_PER_ROW;
            NewRow(RowAddress);
            COL := 0;
        end;
    
        // Print value
        Write(MakeSymbol(Source[I], Format), CHAR_SPACE);
        Inc(COL);
    end;
    // Last empty string
    Writeln();
end;

end.

{-------------------------------------------------------------------------}
{ END                                                                     }
{-------------------------------------------------------------------------}
