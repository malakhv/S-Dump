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
{  SPDX-License-Identifier: Apache-2.0                                         }
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{ Unit: Mikhan.Utils.StrUtils                                                  }
{                                                                              }
{ String utilities and common character constants.                             }
{                                                                              }
{ The Unit provides common character constants and methods such as string      }
{ trimming, blank-string and prefix checks, byte-to-string conversion, string  }
{ repetition, etc.                                                             }
{                                                                              }
{ Project: Pascal Utils Library (PUL)                                          }
{ Package: Mikhan.Utils                                                        }
{                                                                              }
{ Dependencies: SysUtils                                                       }
{                                                                              }
{ Notes:                                                                       }
{   String resolves to AnsiString in the current compiler mode. String lengths }
{   and indices refer to bytes. Memory-copying routines rely on this represen- }
{   tation.                                                                    }
{                                                                              }
{ Created: 15.08.2022                                                          }
{ Authors: Mikhail.Malakhov                                                    }
{------------------------------------------------------------------------------}

Unit Mikhan.Utils.StrUtils;                                             { UNIT }

{$MODE DELPHI}
{$H+}

INTERFACE                                                          { INTERFACE }

{
    Common string and character constants
}
const

    { The empty string. }
    EMPTY = '';

    { The null character (NUL). }
    CHAR_NUL = #0;

    { The horizontal tab character (HT). }
    CHAR_TAB = #9;

    { The space character. }
    CHAR_SPACE = ' ';

    { The home character. }
    CHAR_HOME = #36;

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

    { The number sign. }
    CHAR_NUMBER = '#';

    { The question mark. }
    CHAR_QUESTION_MARK = '?';

    { The at sign. }
    CHAR_AT = '@';

    { The asterisk. }
    CHAR_ASTERISK = '*';

    { The forward slash. }
    CHAR_SLASH = '/';

    { The backslash. }
    CHAR_BACK_SLASH = '\';

    { The vertical bar. }
    CHAR_VERT_SLASH = '|';

    { The tilde. }
    CHAR_TILDE = '~';

    { The section. }
    CHAR_SECTION = '§';

{------------------------------------------------------------------------------}

{ Removes leading and trailing characters (spaces and control characters) at
    the beginning and the end of the specified string. }
procedure TrimStr(var Source: String);

{ Returns True if Source is empty or contains only special characters. }
function IsBlank(const Source: String): Boolean;

{ Returns True if S starts with Prefix. The comparison is case-sensitive,
    spaces and control characters are significant. }
function StartWith(const Prefix, S: String): Boolean;

{ Represents Source bytes as a string without decoding or encoding conversion.
    Preserves all byte values, including #0. }
function BytesToStr(const Source: Array of Byte): String;

{ Returns Source repeated Count times. }
function RepeatString(const Source: String; Count: Integer): String;

{------------------------------------------------------------------------------}

IMPLEMENTATION                                                { IMPLEMENTATION }

{------------------------------------------------------------------------------}

uses SysUtils;

procedure TrimStr(var Source: String);
begin
    Source := Trim(Source);
end;

function IsBlank(const Source: String): Boolean;
begin
    Result := (Length(Source) <= 0) or (Length(Trim(Source)) <= 0);
end;

function StartWith(const Prefix, S: String): Boolean;
begin
    if (S = '') or (Prefix = '') or (Length(Prefix) > Length(S)) then
    begin
        Result := False; Exit;
    end;
    Result := pos(Prefix, S) = 1;
end;

function BytesToStr(const Source: Array of Byte): String;
var
    Len: Integer;
begin
    Len := Length(Source);
    SetLength(Result, Len);
    if Len > 0 then
        Move(Source[0], Result[1], Len);
end;

function RepeatString(const Source: String; Count: Integer): String;
var
    I, Len: Integer;
begin
    Result := EMPTY;
    if (Source = '') or (Count <= 0) then Exit;
    Len := Length(Source);
    SetLength(Result, Len * Count);
    for I := 0 to Count - 1 do
        Move(Source[1], Result[Len * I + 1], Len);
end;

{------------------------------------------------------------------------------}

END.                                                                     { END }

{------------------------------------------------------------------------------}
