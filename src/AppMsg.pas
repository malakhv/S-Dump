{-----------------------------------------------------------------}
{                                                                 }
{                       S-Dump project                            }
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

unit AppMsg;

{$mode delphi}
{$h+}

interface

{ Program messages }
const

    { Program message: No input file. }
    MSG_NO_INPUT = 'Please, specify source file...';

    { Program message: Input file not found. }
    MSG_INPUT_NOT_FOUND = 'The source file not found...';

implementation

end.
