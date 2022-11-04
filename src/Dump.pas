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

{ Prints raw data. }
procedure Dump(Source: Array of Byte; Offset: Integer; Limit: Integer; InHex: Boolean);

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

procedure Dump(Source: Array of Byte; Offset: Integer; Limit: Integer; InHex: Boolean);
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
        if (InHex) then
            Write(IntToHex(val, 2))
        else
            Write(val);
        Write(' ');
        Dec(Limit);
        if Limit <= 0 then break;
    end;
    Writeln('');

end;

end.
