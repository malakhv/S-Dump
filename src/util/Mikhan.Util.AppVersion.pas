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

{--------------------------------------------------------------------}
{ The Unit contains types, methods and classes to working with       }
{ program version.                                                   }
{                                                                    }
{ Package: Mikhan.Util                                               }
{ Types: TAppParams                                                  }
{ Dependencies: Mikhan.Util.StrUtils                                 }
{                                                                    }
{ Created: 17.08.2022                                                }
{ Author: Mikhail.Malakhov                                           }
{--------------------------------------------------------------------}

{--------------------------------------------------------------------}
{ This unit uses "Semantic Versioning 2.0.0" conception, for         }
{ more information about it, please see http://semver.org/.          }
{                                                                    }
{ The version name represents in format:                             }
{   XX.YY.ZZZ[.DEBUG][-BRANCH_NAME][-COMMIT_ID], where:              }
{                                                                    }
{   - XX - major version (max value is 99)                           }
{   - YY - minor version (max value is 99)                           }
{   - ZZZ - number of patch (max value is 999)                       }
{   - DEBUG - the debug flag (only for debug build), may be null     }
{   - BRANCH_NAME - the git branch name, for example "master"        }
{   - COMMIT_ID - the first seven letters of git commit id,          }
{     for example "94604e3"                                          }
{                                                                    }
{ Below, you could see several examples:                             }
{                                                                    }
{   - 0.1.0.debug-master - initial (default) version for all new     }
{     project (without any commits and on default branch)            }
{                                                                    }
{   - 1.2.15.debug-master-94604e3 - debug build of version           }
{     1.2.15, where latest commit id is 94604e3                      }
{                                                                    }
{   - 1.3.0.master-94604e3 - release build of version 1.3.0          }
{     where latest commit id is 94604e3                              }
{                                                                    }
{ The initial (default) value of version is:                         }
{                                                                    }
{   0.1.0.debug-master                                               }
{                                                                    }
{ The version code calculate from version name by                    }
{ following rule:                                                    }
{                                                                    }
{   XX * 100000 + YY * 1000 + ZZZ                                    }
{                                                                    }
{ Several examples:                                                  }
{                                                                    }
{   - Name 0.1.0.debug-master-0000000 = Code 1000                    }
{   - Name 0.1.8.debug-develop-0000000 = Code 1008                   }
{   - Name 2.5.17.master-94604e3 = Code 205017                       }
{   - Name 99.88.777.debug-qa-0000000 = Code 9988777                 }
{                                                                    }
{ The program version can be stored in separate file.                }
{--------------------------------------------------------------------}

unit Mikhan.Util.AppVersion;

{$mode delphi}
{$h+}

interface

uses SysUtils;

const

    { The default file name with version information. }
    VER_FILE_NAME = 'version';

    { The version name suffix using for debug builds. }
    VER_DEBUG_SUFFIX = 'debug';

const

    { The version component key: Major. }
    VER_MAJOR_KEY = 'MAJOR';

    { The version component key: Minor. }
    VER_MINOR_KEY = 'MINOR';

    { The version component key: Patch. }
    VER_PATCH_KEY = 'PATCH';

type

    { The version component available values: Major. }
    TVerMajor = 0..99;

    { The version component available values: Minor. }
    TVerMinor = 0..99;

    { The version component available values: Patch. }
    TVerPatch = 0..999;

type

    { Class implements basic "Semantic Versioning" conception. }
    TSemVer = class (TObject)
    private
        FMajor: TVerMajor;  // See Major property.
        FMinor: TVerMinor;  // See Minor property.
        FPatch: TVerPatch;  // See Patch property.
        FDebug: Boolean;    // See Debug property.
    protected
        { See Code property. }
        function GetCode(): Integer; virtual;
        { See Name property. }
        function GetName(): String; virtual;
    public
        { The version component: Major. }
        property Major: TVerMajor read FMajor;
        { The version component: Minor. }
        property Minor: TVerMinor read FMinor;
        { The version component: Patch. }
        property Patch: TVerPatch read FPatch;
        { The version code. }
        property Code: Integer read GetCode;
        { The version name. }
        property Name: String read GetName;
        { Debug app build or not? }
        property Debug: Boolean read FDebug;

        { Reads the version information from default file. }
        function LoadFromFile(): Boolean; overload;
        { Reads the version information from specified file. }
        function LoadFromFile(FileName: TFileName): Boolean; overload;

        { Represents data in this object as readable string. }
        function ToString(): String; override;

        constructor Create(Debug: Boolean); overload; virtual;
        constructor Create(Debug: Boolean; Major: TVerMajor; Minor: TVerMinor;
            Patch: TVerPatch); overload; virtual;

    end;

Implementation

uses IniFiles, Mikhan.Util.StrUtils;

{
    TSemVer
}

constructor TSemVer.Create(Debug: Boolean);
begin
    inherited Create();
    // The default app version is 0.1.0
    Self.Create(Debug, 0, 1, 0);
end;

constructor TSemVer.Create(Debug: Boolean; Major: TVerMajor; Minor: TVerMinor;
    Patch: TVerPatch);
begin
    inherited Create();
    FDebug := Debug;
    FMajor := Major;
    FMinor := Minor;
    FPatch := Patch;
end;

function TSemVer.GetCode(): Integer;
begin
    Result := Major * 100000 + Minor * 1000 + Patch;
end;

function TSemVer.GetName(): String;
const VER_NAME_SEP = '.';
begin
    Result := IntToStr(Major) + VER_NAME_SEP + IntToStr(Minor) +
        VER_NAME_SEP + IntToStr(Patch);
    if Debug then
        Result := Result + VER_NAME_SEP + VER_DEBUG_SUFFIX;
end;

function TSemVer.LoadFromFile(): Boolean;
begin
    Result := Self.LoadFromFile(VER_FILE_NAME);
end;

function TSemVer.LoadFromFile(FileName: TFileName): Boolean;
var ver : TIniFile;
begin
    Result := False;
    if Mikhan.Util.StrUtils.IsEmpty(FileName) then Exit;
    ver := TIniFile.Create(FileName);
    FMajor := ver.ReadInteger('version', VER_MAJOR_KEY, 0);
    FMinor := ver.ReadInteger('version', VER_MINOR_KEY, 0);
    FPatch := ver.ReadInteger('version', VER_PATCH_KEY, 0);
    ver.Free;
    Result := True;
end;

function TSemVer.ToString(): String;
begin
    Result := 'Version: ' + Name + ' (' + IntToStr(Code) + ')';
end;

end.
