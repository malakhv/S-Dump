{-----------------------------------------------------------------}
{                                                                 }
{                  Pascal Utils Library (PUL)                     }
{                                                                 }
{  Copyright (C) 2022 Mikhail Malakhov <malakhv@gmail.com>        }
{                                                                 }
{  Unauthorized copying of this file, via any medium is           }
{  strictly prohibited.                                           }
{                                                                 }
{       Confidential and Proprietary. All Rights Reserved.        }
{                                                                 }
{-----------------------------------------------------------------}

{-----------------------------------------------------------------}
{ The Unit contains types, methods and classes to working with    }
{ program logs.                                                   }
{                                                                 }
{ Package: Mikhan.Util                                            }
{ Types: TAppLogs, TLogLevel                                      }
{ Dependencies: Mikhan.Util.StrUtils                              }
{                                                                 }
{ Created: 14.08.2022                                             }
{ Author: Mikhail.Malakhov                                        }
{-----------------------------------------------------------------}

unit Mikhan.Util.AppLogs;

{$mode delphi}
{$h+}

Interface

type

    { The available logging levels. }
    TLogLevel = (
        { Logging level: Not inportant information only for development time. }
        llVerbose,
        { Logging level: Information only for development, debugging and testing time. }
        llDebug,
        { Logging level: Any important information for release version. }
        llInfo,
        { Logging level: Program warnings. }
        llWarn,
        { Logging level: Program errors and critical messages. }
        llError,
        {
          Logging level: Silent mode, turning off all messages. In normal
          case program shouldn't use it to write a messages.
        }
        llSilent
    );

type

    { Class implements a program logging mechanism. }
    TAppLogs = class (TObject)
    private
        FAppTag: String;        // See AppTag property
        FLogLevel: TLogLevel;   // See LogLevel property
        IsDebug: Boolean;       // Debug mode?
        HasAppTag: Boolean;
    protected
        { Setter for LogLevel property }
        procedure DoSetLogLevel(LogLevel: TLogLevel); virtual;
        { Low-level logging call. Print a message with specified parameters. }
        procedure Print(Level: TLogLevel; const Message: String); overload;
        { Low-level logging call. Print a message with specified parameters. }
        procedure Print(Level: TLogLevel; const Tag, Message: String); overload;
        { Low-level logging call. Print a messages with specified parameters. }
        procedure Print(Level: TLogLevel; const Messages: array of const); overload;
        { Low-level logging call. Print a messages with specified parameters. }
        procedure Print(Level: TLogLevel; const Tag: String; const Messages: array of const); overload;
    public
        { The main program log tag. }
        property AppTag: String read FAppTag;

        { The current logging level. All messages less that level will be ignoring. }
        property LogLevel: TLogLevel read FLogLevel write DoSetLogLevel;
        { Checks to loggable or not messages with specified log level. }
        function IsLoggable(Level: TLogLevel): Boolean; virtual;

        { Sends a verbose log message with main program tag. }
        procedure V(const Message: String); overload;
        { Sends a verbose log message with main program tag and specified tag. }
        procedure V(const Tag: String; const Message: String); overload;
        { Sends a verbose log messages with main program tag. }
        procedure V(const Messages: array of const); overload;
        { Sends a verbose log messages with main program tag and specified tag. }
        procedure V(const Tag: String; const Messages: array of const); overload;

        { Sends a debug log message with main program tag. }
        procedure D(const Message: String); overload;
        { Sends a debug log message with main program tag and specified tag. }
        procedure D(const Tag: String; const Message: String); overload;
        { Sends a debug log messages with main program tag. }
        procedure D(const Messages: array of const); overload;
        { Sends a debug log messages with main program tag and specified tag. }
        procedure D(const Tag: String; const Messages: array of const); overload;

        { Sends a info log message with main program tag. }
        procedure I(const Message: String); overload;
        { Sends a info log message with main program tag and specified tag. }
        procedure I(const Tag: String; const Message: String); overload;
        { Sends a info log messages with main program tag. }
        procedure I(const Messages: array of const); overload;
        { Sends a info log messages with main program tag and specified tag. }
        procedure I(const Tag: String; const Messages: array of const); overload;

        { Sends a warning log message with main program tag. }
        procedure W(const Message: String); overload;
        { Sends a warning log message with main program tag and specified tag. }
        procedure W(const Tag: String; const Message: String); overload;
        { Sends a warning log messages with main program tag. }
        procedure W(const Messages: array of const); overload;
        { Sends a warning log messages with main program tag and specified tag. }
        procedure W(const Tag: String; const Messages: array of const); overload;

        { Sends a error log message with main program tag. }
        procedure E(const Message: String); overload;
        { Sends a error log message with main program tag and specified tag. }
        procedure E(const Tag: String; const Message: String); overload;
        { Sends a error log messages with main program tag. }
        procedure E(const Messages: array of const); overload;
        { Sends a error log messages with main program tag and specified tag. }
        procedure E(const Tag: String; const Messages: array of const); overload;

        { Construct a new instance of TAppLogs class with specified parameters. }
        constructor Create(AppTag: String); overload;
        { Construct a new instance of TAppLogs class with specified parameters. }
        constructor Create(AppTag: String; Debug: Boolean); overload;
        destructor Destroy; override;
        { The class method to print raw data in hexadecimal format. }
        class procedure Dump(Source: Array of Byte); overload;
        { The class method to print raw data in hexadecimal format. }
        class procedure Dump(Source: Array of Byte; Limit: Integer); overload;
        { The class method to print raw data. }
        class procedure Dump(Source: Array of Byte; Limit: Integer; InHex: Boolean); overload;
    end;

//--------------------------------------------------------------------------------------------------
// Implementation Section
//--------------------------------------------------------------------------------------------------
implementation

uses SysUtils, Mikhan.Util.StrUtils;

const

    { The empty log tag. }
    TAG_EMPTY = Mikhan.Util.StrUtils.EMPTY;

    { String that will use as delimiter for tags in LogCat message. }
    TAG_DELIMITER = ': ';

const

    { Array of LogLevel abbreviations. }
    LOG_LEVEL_STR: array[TLogLevel] of Char = ('V', 'D', 'I', 'W', 'E', 'S');

{ Returns LogLevel as string abbreviation. }
function LogLavelToStr(LogLevel: TLogLevel): String;
begin
    Result := LOG_LEVEL_STR[LogLevel];
end;

{ Write a TVarRec. }
procedure WriteVarRec(Value: TVarRec);
begin
    case Value.VType of
        vtBoolean: Write(Value.vBoolean);
        vtInteger: Write(Value.vInteger);
        vtCurrency: Write(Value.vCurrency^);
        vtExtended: Write(Value.VExtended^);
        vtInt64: Write(Value.vInt64^);
        vtChar: Write(Value.vChar);
        vtWideChar: Write(Value.vWideChar);
        vtString: Write(Value.vString^);
        vtAnsiString: Write(AnsiString(Value.vAnsiString));
        vtWideString: Write(WideString(Value.vWideString));
    else
        Write(Value.VType);
    end;
end;

{ Write a TVarRec and go to new string. }
procedure WriteVarRecLn(Value: TVarRec);
begin
    WriteVarRec(Value);
    WriteLn();
end;

{ Construct a new instance of TAppLogs class with specified parameters. }
constructor TAppLogs.Create(AppTag: String);
begin
    Self.Create(AppTag, False);
end;

{ Construct a new instance of TAppLogs class with specified parameters. }
constructor TAppLogs.Create(AppTag: String; Debug: Boolean);
begin
    inherited Create();
    FAppTag := AppTag;
    HasAppTag := not Mikhan.Util.StrUtils.isEmpty(FAppTag);
    LogLevel := TLogLevel.llDebug;
    IsDebug := Debug;
end;

destructor TAppLogs.Destroy;
begin
    inherited;
end;

{ The class method to print raw data in hexadecimal format. }
class procedure TAppLogs.Dump(Source: Array of Byte);
begin
    TAppLogs.Dump(Source, 0);
end;

{ The class method to print raw data in hexadecimal format. }
class procedure TAppLogs.Dump(Source: Array of Byte; Limit: Integer);
begin
    TAppLogs.Dump(Source, Limit, True);
end;

{ The class method to print raw data. }
class procedure TAppLogs.Dump(Source: Array of Byte; Limit: Integer; InHex: Boolean);
const
    COL_LIMIT = $F;
    COL_OFFSET = '          ';
    COL_SEP = '| ';
    HEADER_SEP = '---------------------------------------------------------';
var i, col, offset: Integer;
    val: Byte;

    procedure Header();
    var i: Integer;
    begin
        Write(COL_OFFSET);
        for i := 0 to COL_LIMIT do
        begin
            Write(IntToHex(i, 2)); Write(' ');
        end;
        WriteLn();
        WriteLn(HEADER_SEP);
    end;

    procedure NewRow();
    begin
        Writeln('');
        offset := offset + COL_LIMIT + 1;
        Write(IntToHex(offset, 8), COL_SEP);
        col := COL_LIMIT;
    end;

begin

    // Print header
    Header();

    col := COL_LIMIT;
    offset := 0;
    if Limit <= 0 then Limit := MaxInt;
    Write(IntToHex(offset, 8), COL_SEP);
    for i := Low(Source) to High(Source) do
    begin

        if col < 0 then NewRow();
        Dec(col);

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

{ Setter for LogLevel property }
procedure TAppLogs.DoSetLogLevel(LogLevel: TLogLevel);
begin
    FLogLevel := LogLevel;
end;

{ Checks to loggable or not messages with specified log level. }
function TAppLogs.IsLoggable(Level: TLogLevel): Boolean;
begin
    Result :=  Ord(Level) >= Ord(LogLevel);
end;

{ Low-level logging call. Print a message with specified parameters. }
procedure TAppLogs.Print(Level: TLogLevel; const Message: String);
begin
    Print(Level, TAG_EMPTY, Message);
end;

{ Low-level logging call. Print a message with specified parameters. }
procedure TAppLogs.Print(Level: TLogLevel; const Tag, Message: String);
var prefix: String;
begin
    if not IsLoggable(Level) then Exit;
    //prefix := '';
    prefix := LogLavelToStr(Level) + TAG_DELIMITER;
    if HasAppTag then
        prefix := AppTag + TAG_DELIMITER + prefix;
    if not Mikhan.Util.StrUtils.isEmpty(Tag) then
        prefix := prefix + Tag + TAG_DELIMITER;
    Writeln(prefix, Message);
end;

{ Low-level logging call. Print a messages with specified parameters. }
procedure TAppLogs.Print(Level: TLogLevel; const Messages: array of const);
begin
    Print(Level, TAG_EMPTY, Messages);
end;

{ Low-level logging call. Print a messages with specified parameters. }
procedure TAppLogs.Print(Level: TLogLevel; const Tag: String; const Messages: array of const);
var i: Integer;
    prefix: String;
begin
    if not IsLoggable(Level) then Exit;
    //prefix := '';
    prefix := LogLavelToStr(Level) + TAG_DELIMITER;
    if HasAppTag then
        prefix := AppTag + TAG_DELIMITER + prefix;
    if not Mikhan.Util.StrUtils.isEmpty(Tag) then
        prefix := prefix + Tag + TAG_DELIMITER;
    Write(prefix);
    for i := Low(Messages) to High(Messages) do
        WriteVarRec(Messages[i]);

    WriteLn();
end;


{ Sends a verbose log message with main program tag. }
procedure TAppLogs.V(const Message: String);
begin
    V(TAG_EMPTY, Message);
end;

{ Sends a verbose log message with main program tag and specified tag. }
procedure TAppLogs.V(const Tag: String; const Message: String);
begin
    Print(TLogLevel.llVerbose, Tag, Message);
end;

{ Sends a verbose log messages with main program tag. }
procedure TAppLogs.V(const Messages: array of const);
begin
    V(TAG_EMPTY, Messages);
end;

{ Sends a verbose log messages with main program tag and specified tag. }
procedure TAppLogs.V(const Tag: String; const Messages: array of const);
begin
    Print(TLogLevel.llVerbose, Tag, Messages);
end;

{ Sends a debug log message with main program tag. }
procedure TAppLogs.D(const Message: String);
begin
    D(TAG_EMPTY, Message);
end;

{ Sends a debug log message with main program tag and specified tag. }
procedure TAppLogs.D(const Tag: String; const Message: String);
begin
    Print(TLogLevel.llDebug, Tag, Message);
end;

{ Sends a debug log messages with main program tag. }
procedure TAppLogs.D(const Messages: array of const);
begin
    D(TAG_EMPTY, Messages);
end;

{ Sends a debug log messages with main program tag and specified tag. }
procedure TAppLogs.D(const Tag: String; const Messages: array of const);
begin
    Print(TLogLevel.llDebug, Tag, Messages);
end;

{ Sends a info log message with main program tag. }
procedure TAppLogs.I(const Message: String);
begin
    I(TAG_EMPTY, Message);
end;

{ Sends a info log message with main program tag and specified tag. }
procedure TAppLogs.I(const Tag: String; const Message: String);
begin
    Print(TLogLevel.llInfo, Tag, Message);
end;

{ Sends a info log messages with main program tag. }
procedure TAppLogs.I(const Messages: array of const);
begin
    I(TAG_EMPTY, Messages);
end;

{ Sends a info log messages with main program tag and specified tag. }
procedure TAppLogs.I(const Tag: String; const Messages: array of const);
begin
    Print(TLogLevel.llInfo, Tag, Messages);
end;

{ Sends a warning log message with main program tag. }
procedure TAppLogs.W(const Message: String);
begin
    W(TAG_EMPTY, Message);
end;

{ Sends a warning log message with main program tag and specified tag. }
procedure TAppLogs.W(const Tag: String; const Message: String);
begin
    Print(TLogLevel.llWarn, Tag, Message);
end;

{ Sends a warning log messages with main program tag. }
procedure TAppLogs.W(const Messages: array of const);
begin
    W(TAG_EMPTY, Messages);
end;

{ Sends a warning log messages with main program tag and specified tag. }
procedure TAppLogs.W(const Tag: String; const Messages: array of const);
begin
    Print(TLogLevel.llWarn, Tag, Messages);
end;

{ Sends a error log message with main program tag. }
procedure TAppLogs.E(const Message: String);
begin
    E(TAG_EMPTY, Message);
end;

{ Sends a error log message with main program tag and specified tag. }
procedure TAppLogs.E(const Tag: String; const Message: String);
begin
    Print(TLogLevel.llError, Tag, Message);
end;

{ Sends a error log messages with main program tag. }
procedure TAppLogs.E(const Messages: array of const);
begin
    E(TAG_EMPTY, Messages);
end;

{ Sends a error log messages with main program tag and specified tag. }
procedure TAppLogs.E(const Tag: String; const Messages: array of const);
begin
    Print(TLogLevel.llError, Tag, Messages);
end;

end.

//--------------------------------------------------------------------------------------------------
