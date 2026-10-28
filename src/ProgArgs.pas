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
{ The Unit contains program options and arguments.                             }
{                                                                              }
{ Dependencies: No                                                             }
{                                                                              }
{ Created: 28.10.2026                                                          }
{ Authors: Mikhail.Malakhov                                                    }
{------------------------------------------------------------------------------}

UNIT ProgArgs;                                                          { UNIT }

{$MODE DELPHI}
{$H+}

INTERFACE                                                          { INTERFACE }

{
    Program options and arguments
}
const

    { Option: A limit of bytes processing. }
    OPT_LIMIT_SHORT = '-l';
    OPT_LIMIT_LONG = '--limit';

    { Option: An offset from file beginning to process. }
    OPT_OFFSET_SHORT = '-s';
    OPT_OFFSET_LONG = '--skip';

    { Option: Represents all data as a char array, short format. }
    OPT_CHAR_SHORT = '-c';
    OPT_CHAR_LONG = '--char';

    { Option: Use specified text as a program data source. This option
      has a higher priority than file name or pipe data. }
    OPT_TEXT_SHORT = '-t';
    OPT_TEXT_LONG = '--text';

    { Option: Search something into data source. }
    OPT_FIND_SHORT = '-f';
    OPT_FIND_LONG = '--find';

const

    { Command: Any actions for testing. }
    CMD_TEST = 'test';

{  }
type

    TProgArgs = class (TAppArgs);
    public
        //function HasLimit(): Boolean;
        function GetLimit(): Integer; // 0 = no limit
        function GetOffset(): Integer; // 0 by default
    end;

{------------------------------------------------------------------------------}

IMPLEMENTATION                                                { IMPLEMENTATION }

{------------------------------------------------------------------------------}

function GetInt(const Short, Long: TArgString; Def: Integer): Integer;
var
    Val: String;
begin
    Result := Def;
    Val := AppArgs.GetValue(Short, Long);
    // TODO Need to check Val bacause it can be not a int str
    if not Mikhan.Utils.StrUtils.IsBlank(Val) then Result := StrToInt(Val);
end;

function GetLimit(): Integer;
begin
    Result := GetInt(OPT_LIMIT_SHORT, OPT_LIMIT_LONG, 0);
end;

function GetOffset(): Integer;
begin
    Result := GetInt(OPT_OFFSET_SHORT, OPT_OFFSET_LONG, 0);
end;

{------------------------------------------------------------------------------}

END.                                                                     { END }

{------------------------------------------------------------------------------}
