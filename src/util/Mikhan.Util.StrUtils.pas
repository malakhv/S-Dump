{-----------------------------------------------------------------}
{                                                                 }
{                  Pascal Utils Library (PUL)                     }
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

{-----------------------------------------------------------------}
{ The Unit contains constants and methods to working              }
{ with strings.                                                   }
{                                                                 }
{ Package: Mikhan.Util                                            }
{ Types: No                                                       }
{ Dependencies: No                                                }
{                                                                 }
{ Created: 15.08.2022                                             }
{ Author: Mikhail.Malakhov                                        }
{-----------------------------------------------------------------}

unit Mikhan.Util.StrUtils;

{$mode delphi}
{$h+}

Interface

const

    { The empty string. }
    EMPTY = '';

    { The space character. }
    CHAR_SPACE = ' ';

    { The dot character. }
    CHAR_DOT = '.';

    { The comma character. }
    CHAR_COMMA = ',';

    { The colon character. }
    CHAR_COLON = ':';

    { The semicolon character. }
    CHAR_SEMICOLON = ';';

    { The less-than sign. }
    CHAR_LESS_THAN = '<';

    { The equal sign. }
    CHAR_EQUAL = '=';

    { The greater-than sign. }
    CHAR_GREATER_THAN = '>';

    { The question mark. }
    CHAR_QUESTION_MARK = '?';

    { The at sign. }
    CHAR_AT = '@';

    { The special char: end of the line. }
    CHAR_NEW_LINE = '\n';

    { The special char: empty line. }
    CHAR_EMPTY_LINE = '\n\n';

    { The special char: slash }
    CHAR_SLASH = '/';

{
    Trims blank characters (spaces and control characters) at the beginning
    and end of the specified string.
}
procedure TrimStr(var Source: String);

{
    Returns True if the string is null, 0-length, or this string
    contains only whitespaces.
}
function IsEmpty(const Source: String): Boolean;

{ Returns true, if string S starts with specified Prefix. }
function StartWith(const Prefix, S: String): Boolean;

{
  Converts an array of bytes to string.
}
function BytesToStr(const Source: Array of Byte): String;

Implementation

uses SysUtils;

procedure TrimStr(var Source: String);
begin
    Source := Trim(Source);
end;

function IsEmpty(const Source: String): Boolean;
begin
    Result := (Length(Source) <= 0) or (Length(Trim(Source)) <= 0);
end;

function StartWith(const Prefix, S: String): Boolean;
begin
    if IsEmpty(S) or IsEmpty(Prefix) or (Length(Prefix) > Length(S)) then
    begin
        Result := False;
        Exit;
    end;
    Result := pos(Prefix, S) = 1;
end;

function BytesToStr(const Source: Array of Byte): String;
var i: Integer;
begin
    Result := '';
    for i := Low(Source) to High(Source) do
        Result := Result + Char(Source[i]);
end;

end.
