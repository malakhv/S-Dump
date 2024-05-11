{------------------------------------------------------------------------------}
{                                                                              }
{                                S-Dump project                                }
{                                                                              }
{  Copyright (C) 1996-2023 Mikhail Malakhov <malakhv@gmail.com>                }
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
{ The Unit contains program version information.                               }
{                                                                              }
{ Dependencies: Mikhan.Util.AppVersion                                         }
{                                                                              }
{ Created: 04.08.2023                                                          }
{ Author: Mikhail.Malakhov                                                     }
{------------------------------------------------------------------------------}

UNIT ProgVer;                                                           { UNIT }

{$mode delphi}
{$h+}

INTERFACE                                                          { INTERFACE }

{
    Program version components. It uses "Semantic Versioning 2.0.0" conception.
}
const

    { The version component: Major. }
    PROG_VERSION_MAJOR = 1;
    
    { The version component: Minor. }
    PROG_VERSION_MINOR = 0;
    
    { The version component: Patch. }
    PROG_VERSION_PATCH = 0;

{
    Returns information about program version as a human readable string.
}
function GetVersion(Debug: Boolean): String;

IMPLEMENTATION                                                { IMPLEMENTATION }

uses Mikhan.Util.AppVersion;

function GetVersion(Debug: Boolean): String;
var AppVer: TSemVer;
begin
    AppVer := TSemVer.Create(Debug, PROG_VERSION_MAJOR,
        PROG_VERSION_MINOR, PROG_VERSION_PATCH);
    Result:= AppVer.ToString();
end;

END.                                                                     { END }

{------------------------------------------------------------------------------}
