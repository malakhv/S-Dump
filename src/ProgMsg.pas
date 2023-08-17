{--------------------------------------------------------------------}
{                                                                    }
{                          S-Dump project                            }
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
{ The Unit contains program messages.                                }
{                                                                    }
{ Created: 03.11.2022                                                }
{ Author: Mikhail.Malakhov                                           }
{--------------------------------------------------------------------}

unit ProgMsg;

{$mode delphi}
{$h+}

interface

{ Program messages }
const

    { Program message: No input file. }
    MSG_NO_INPUT = 'Please, specify source file...';

    { Program message: Input file not found. }
    MSG_INPUT_NOT_FOUND = 'The source file not found...';

    { Program message: Cannot read data from data source. }
    MSG_CANNOT_READ_DATA = 'Cannot read data from data source...';

{
  Prints program help message.
}
procedure PrintHelp(ProgName: String);

implementation

const
    DEF_INDENT = '  ';

procedure PrintHelp(ProgName: String);
begin
    WriteLn(ProgName, ' command line options:');
    WriteLn(DEF_INDENT, ' FILE_NAME               - An input file to process.');
    WriteLn(DEF_INDENT, ' -s (--offset) VALUE     - An offset from data beginning.');
    WriteLn(DEF_INDENT, ' -l (--limit) VALUE      - A limit of bytes processing.');
    WriteLn(DEF_INDENT, ' -t (--text) "ANY TEXT"  - A text to process.');
    WriteLn(DEF_INDENT, ' -v (--version)          - The program version.');
    WriteLn(DEF_INDENT, ' -h (--help)             - Display this information.');
    WriteLn(DEF_INDENT, ' --verbose               - Turning on all debug messages.');
    WriteLn();
end;

end.
