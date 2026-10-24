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
{ Unit: Mikhan.Utils.Pipes                                                     }
{                                                                              }
{ The Unit contains stuff to pipes support.                                    }
{                                                                              }
{ Dependencies: Pipes, BaseUnix, Windows                                       }
{                                                                              }
{ Created: 23.10.2026                                                          }
{ Author: Mikhail.Malakhov                                                     }
{------------------------------------------------------------------------------}

UNIT Mikhan.Utils.Pipes;                                                { UNIT }

{$MODE DELPHI}
{$H+}

INTERFACE                                                          { INTERFACE }

{

}
function HasRedirectedInput: Boolean;

{------------------------------------------------------------------------------}

IMPLEMENTATION                                                { IMPLEMENTATION }

{------------------------------------------------------------------------------}

USES
    {$IFDEF UNIX}BaseUnix, Termio;{$ENDIF}
    {$IFDEF MSWINDOWS}Windows;{$ENDIF}

{------------------------------------------------------------------------------}
{ HasRedirectedInput                                                           }
{------------------------------------------------------------------------------}

{$IFDEF UNIX}
function HasRedirectedInputNix: Boolean;
var
    Info: Stat;
begin
    Result := False;
    if FpFStat(StdInputHandle, Info) <> 0 then Exit;

    Result := fpS_ISFIFO(Info.st_mode) or fpS_ISREG(Info.st_mode) or
        (fpS_ISCHR(Info.st_mode) and (Termio.IsATTY(StdInputHandle) = 0));
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function HasRedirectedInputWin: Boolean;
var
    Kind: DWORD;
begin
    Kind := Windows.GetFileType(StdInputHandle);
    Result := (Kind = FILE_TYPE_PIPE) or (Kind = FILE_TYPE_DISK);
end;
{$ENDIF}

function HasRedirectedInput: Boolean;
begin
    {$IFDEF UNIX}
    Result := HasRedirectedInputNix();
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    Result := HasRedirectedInputWin();
    {$ENDIF}
end;

{------------------------------------------------------------------------------}

END.                                                                     { END }

{------------------------------------------------------------------------------}
