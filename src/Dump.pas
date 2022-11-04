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

unit Dump;

{$mode delphi}
{$h+}

interface

type

    { The formats of output, hex or char. }
    TOutFormat = (ofHex, ofChar);

{ Prints raw data. }
procedure Dump(Source: Array of Byte; Offset: Integer; Limit: Integer); overload;

{ Prints raw data. }
procedure Dump(Source: Array of Byte; Offset: Integer; Limit: Integer;
    Format: TOutFormat); overload;

implementation

uses SysUtils;

const

    { The maximum bytes to processing. }
    MAX_BYTES = 2048;

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

end.
