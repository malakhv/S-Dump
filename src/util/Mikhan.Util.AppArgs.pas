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
{ program command line (input) arguments.                            }
{                                                                    }
{ Package: Mikhan.Util                                               }
{ Types: TAppParams                                                  }
{ Dependencies: Mikhan.Util.StrUtils                                 }
{                                                                    }
{ Created: 17.08.2022                                                }
{ Author: Mikhail.Malakhov                                           }
{--------------------------------------------------------------------}

{--------------------------------------------------------------------}
{ There are three types of program command line arguments:           }
{   - simple option or flag (short or long format) without any       }
{     data, for example: -l, --help                                  }
{   - option (short or long format) with value (key-value pair),     }
{     for example: -t "Text", --file ./file1.txt                     }
{   - program argument or command, for example: clone, status        }
{--------------------------------------------------------------------}

unit Mikhan.Util.AppArgs;

{$mode delphi}
{$h+}

interface

const

    { Program option: List, short format. }
    OPTION_LIST_SHORT = '-l';
    { Program option: List, long format. }
    OPTION_LIST_LONG = '--list';

    { Program option: Status, short format. }
    OPTION_STATUS_SHORT = '-s';
    { Program option: Status, long format. }
    OPTION_STATUS_LONG = '--status';
    
    { Program option: Info, short format. }
    OPTION_INFO_SHORT = '-i';
    { Program option: Info, long format. }
    OPTION_INFO_LONG = '--info';

    { Program option: File, short format. }
    OPTION_FILE_SHORT = '-f';
    { Program option: File, long format. }
    OPTION_FILE_LONG = '--file';

    { Program option: Help, short format. }
    OPTION_HELP_SHORT = '-h';
    { Program option: Help, long format. }
    OPTION_HELP_LONG = '--help';

    { Program option: Version, short format. }
    OPTION_VERSION_SHORT = '-v';
    { Program option: Status, long format. }
    OPTION_VERSION_LONG = '--version';

    { Program option: Verbose (turning all log messages), long format. }
    OPTION_VERBOSE_LONG = '--verbose';

const

    { Option prefix: short format. }
    OPTION_PREFIX_SHORT = '-';
    { Option prefix: long format. }
    OPTION_PREFIX_LONG = '--';

type

    { The base type of program commang line arguments. }
    TArgString = String;

    { The array of program commang line arguments. }
    TArgStrings = Array of TArgString;

type

    { A program argument or option (in short or long format). }
    TArgument = record
        Key: TArgString;
        Value: TArgString;
        function IsOption(): Boolean;
        function IsShort(): Boolean;
        function IsLong():  Boolean;
        function IsArgument(): Boolean;
        function HasValue(): Boolean;
    end;

    { The list of program arguments and options (in short or long format). }
    TArguments = Array of TArgument;

type

    { Class to retreive program arguments. }
    TAppArgs = class(TObject)
    private
        { The program file name with full path. }
        FName: TArgString;
        { Program command line arguments and options (in short and long format). }
        FArguments: TArguments;
    protected
        function Get(Index: Integer): TArgument;        // See Arguments property
        function GetCount(): Integer;                   // See Count property
        function GetValue(Index: Integer): TArgString;  // See Values property
        procedure Add(const Key: TArgString); overload;
        procedure Add(const Key, Value: TArgString); overload;

    public
        { The program file name. }
        property Name: TArgString read FName;

        { The current number of program arguments. }
        property Count: Integer read GetCount;

        { The array of program arguments. }
        property Arguments[Index : Integer]: TArgument read Get; default;
        property Values[Index: Integer]: TArgString read GetValue;

        { Returns true if program has specified argument (or option). }
        function Has(const Key: TArgString): Boolean; overload;
        { Returns true if program has specified option in short or long format. }
        function Has(const Short, Long: TArgString): Boolean; overload;

        { Returns true if program has Help option (-h or --help). }
        function HasHelp(): Boolean;
        { Returns true if program has Version option (-v or --version). }
        function HasVersion(): Boolean;
        { Returns true if program has Verbose option (--verbose). }
        function HasVerbose(): Boolean;

        { Returns value for specified option, or empty string. }
        function GetValue(const Key: TArgString): TArgString; overload;
        { Returns value for specified option (in short and long format), or empty string. }
        function GetValue(const Short, Long: TArgString): TArgString; overload;

        { Clears all stored program parameters. }
        procedure ClearArgs();
        { Persing all program parameters. }
        procedure ParseArgs();
        { Print all known (after parsing) program parameters. }
        procedure PrintAll();

        constructor Create; 
        destructor Destroy; override;
    end;

//--------------------------------------------------------------------------------------------------
// Implementation Section
//--------------------------------------------------------------------------------------------------
implementation

uses Mikhan.Util.StrUtils;

{-----------------------------------------------------------------}
{ Common things                                                   }
{-----------------------------------------------------------------}

const
    STR_EMPTY = Mikhan.Util.StrUtils.EMPTY;

function HasShortPrefix(const Argument: TArgString): Boolean;
begin
    Result := Mikhan.Util.StrUtils.StartWith(OPTION_PREFIX_SHORT, Argument);
end;

function HasLongPrefix(const Argument: TArgString): Boolean;
begin
    Result := Mikhan.Util.StrUtils.StartWith(OPTION_PREFIX_LONG, Argument);
end;

function IsOption(const Argument: TArgString): Boolean;
begin
    Result := HasShortPrefix(Argument) or HasLongPrefix(Argument);
end;

function FindArgument(Short, Long: TArgString; const Target: TArguments): Integer; overload;
var item: TArgument;
var i: Integer;
begin
    for i := Low(Target) to High(Target) do
    begin
        item := Target[i];
        if (item.Key = Short) or (item.Key = Long) then
        begin
            Result := i; Exit;
        end;
    end;
    Result := -1;
end;

function FindArgument(Key: TArgString; const Target: TArguments): Integer; overload;
begin
    Result := FindArgument(Key, STR_EMPTY, Target);
end;

{-----------------------------------------------------------------}
{ TArgument implementation                                        }
{-----------------------------------------------------------------}

function TArgument.IsOption: Boolean;
begin
    Result := HasLongPrefix(Self.Key) or HasShortPrefix(Self.Key);
end;

function TArgument.IsShort(): Boolean;
begin
    Result := HasShortPrefix(Self.Key) and (not HasLongPrefix(Self.Key));
end;

function TArgument.IsLong():  Boolean;
begin
    Result := HasLongPrefix(Self.Key);
end;

function TArgument.IsArgument(): Boolean;
begin
    Result := not HasShortPrefix(Self.Key);
end;

function TArgument.HasValue(): Boolean;
begin
    Result := not Mikhan.Util.StrUtils.IsEmpty(Self.Value);
end;

{-----------------------------------------------------------------}
{ TAppArgs implementation                                         }
{-----------------------------------------------------------------}

constructor TAppArgs.Create;
begin
    inherited Create();
    ClearArgs();
end;

destructor TAppArgs.Destroy;
begin
    ClearArgs();
    inherited Destroy();
end;

procedure TAppArgs.ClearArgs();
begin
    SetLength(FArguments, 0);
end;

function TAppArgs.GetCount(): Integer;
begin
    Result := Length(FArguments);
end;

function TAppArgs.Has(const Key: TArgString): Boolean; overload;
begin
    Result := FindArgument(Key, FArguments) <> -1;
end;

function TAppArgs.Has(const Short, Long: TArgString): Boolean; overload;
begin
    Result := FindArgument(Short, Long, FArguments) <> -1;
end;

function TAppArgs.HasHelp(): Boolean;
begin
    Result := Has(OPTION_HELP_SHORT, OPTION_HELP_LONG);
end;

function TAppArgs.HasVersion(): Boolean;
begin
    Result := Has(OPTION_VERSION_SHORT, OPTION_VERSION_LONG);
end;

function TAppArgs.HasVerbose(): Boolean;
begin
    Result := Has(OPTION_VERBOSE_LONG);
end;

function TAppArgs.GetValue(const Key: TArgString): TArgString;
begin
    Result := Self.GetValue(Key, Key);
end;

function TAppArgs.GetValue(const Short, Long: TArgString): TArgString;
var opt: Integer;
begin
    opt := FindArgument(Short, Long, FArguments);
    if opt >= 0 then
        Result := Arguments[opt].Value
    else
        Result := STR_EMPTY;
end;

function TAppArgs.GetValue(Index: Integer): TArgString;
begin
    Result := Self[Index].Value;
end;

function TAppArgs.Get(Index: Integer): TArgument;
begin
    Result := FArguments[Index];
end;

procedure TAppArgs.Add(const Key: TArgString); overload;
begin
    Add(Key, STR_EMPTY);
end;

procedure TAppArgs.Add(const Key, Value: TArgString); overload;
var len: integer;
begin
    len := Length(FArguments);
    SetLength(FArguments, len + 1);
    FArguments[len].Key := Key;
    FArguments[len].Value := Value;
end;

procedure TAppArgs.PrintAll();
var item: TArgument;
begin
    WriteLn('Name: ', Self.Name);
    WriteLn('Arguments:');
    for item in FArguments do
    begin
        Write('  ', item.Key);
        if item.HasValue then
            Write('=', item.Value);
        WriteLn();
    end;
end;

procedure TAppArgs.ParseArgs();
var
    arg, val: String;
    cur, count: Integer;
begin
    ClearArgs();
    count := ParamCount();
    if count <= 0 then Exit;
    FName := ParamStr(0);
    cur := 1;
    while cur <= count do
    begin
        arg := ParamStr(cur);
        if IsOption(arg) then
        begin
            if cur < count then
                val := ParamStr(cur + 1)
            else
                val := '';
            if (val <> '') and (not IsOption(val)) then
            begin
                Add(arg, val);
                Inc(cur);
            end else
                Add(arg);
        end else
            Add(arg);
        Inc(cur);
    end;
end;

end.

//--------------------------------------------------------------------------------------------------
