{--------------------------------------------------------------------}
{                                                                    }
{                           S-Dump project                           }
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

{--------------------------------------------------------------------}
{ The Unit contains program version information.                     }
{                                                                    }
{ Created: 04.08.2023                                                }
{ Author: Mikhail.Malakhov                                           }
{--------------------------------------------------------------------}

unit ProgVer;

{$mode delphi}
{$h+}

interface

{
  Program version components. It uses "Semantic Versioning 2.0.0"
  conception.
}
const

    { The version component: Major. }
    PROG_VERSION_MAJOR = 0;
    
    { The version component: Minor. }
    PROG_VERSION_MINOR = 1;
    
    { The version component: Patch. }
    PROG_VERSION_PATCH = 0;

{
  Prints information about program version.
}
procedure PrintVersion(Debug: Boolean);

implementation

uses Mikhan.Util.AppVersion;

procedure PrintVersion(Debug: Boolean);
var AppVer: TSemVer;
begin
    AppVer := TSemVer.Create(Debug);
    WriteLn(AppVer.ToString());
end;

end.
